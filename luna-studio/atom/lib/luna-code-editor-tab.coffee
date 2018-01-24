{TextEditor, TextBuffer} = require 'atom'
{TextEditorView, View} = require 'atom-space-pen-views'
{CompositeDisposable} = require 'event-kit'
path = require 'path'
SubAtom = require 'sub-atom'
Spinner = require './spinner'
projects = require './projects'

TextBuffer::setModified = (@modified) ->

TextBuffer::isModified = ->
    if @modified? then return @modified
    # This is implementation for version 1.23.3
    if @file?
        not @file.existsSync() or @buffer.isModified()
    else
        @buffer.getLength() > 0

TextBuffer::isInConflict = ->
    if @modified? then return false
    # This is implementation for version 1.23.3
    @isModified() and @fileHasChangedSinceLastLoad

TextBuffer::subscribeToFileOverride = (codeEditor) ->
    @fileSubscriptions?.dispose()
    @fileSubscriptions = new CompositeDisposable

    if @file.onDidChange?
        @fileSubscriptions.add @file.onDidChange debounce(=>
            @setModified(false)
            codeEditor.pushInternalEvent(tag: "FileChanged", _path: @getPath())
        , @fileChangeDelay)

    if @file.onDidDelete?
        @fileSubscriptions.add @file.onDidDelete =>
            modified = @buffer.isModified()
            @emitter.emit 'did-delete'
            if not modified and @shouldDestroyOnFileDelete()
                @destroy()
            else
                @emitModifiedStatusChanged(true)

    if @file.onDidRename?
        @fileSubscriptions.add @file.onDidRename =>
            @emitter.emit 'did-change-path', @getPath()

    if @file.onWillThrowWatchError?
        @fileSubscriptions.add @file.onWillThrowWatchError (error) =>
            @emitter.emit 'will-throw-watch-error', error

    @fileSubscriptions?.dispose()
    @fileSubscriptions = new CompositeDisposable


subscribe = null

module.exports =
    class LunaCodeEditorTab extends TextEditor

        constructor: (@uri, @codeEditor) ->
            super()
            @initialized = false
            @setModified false
            @setUri @uri
            @diffToOmit = new Set()
            @setPlaceholderText 'Please wait'
            @codeEditor.pushInternalEvent(tag: 'OpenFile', _path: @uri)

            @codeEditor.onSetBuffer (uri, text) =>
                @setBuffer uri, text
                @initialize()
            @codeEditor.onSetClipboard @setClipboard
            @codeEditor.onInsertCode @insertCode
            @handleEvents()

            @subscribe = new SubAtom
            @subscribe.add @getBuffer().onDidStopChanging (event) =>
                diffs = []
                for change in event.changes
                    if @diffToOmit.has change.newText
                        @diffToOmit.delete change.newText
                    else
                        @setModified(true)
                        start = change.oldRange.start
                        end   = change.oldRange.end
                        cursor = @.getCursorBufferPosition()
                        diff =
                            _range:   [{_column: start.column, _row: start.row },
                                       {_column: end.column  , _row: end.row}]
                            _newText: change.newText
                            _cursor:  {_column: cursor.column, _row: cursor.row}
                          #   cursor: (@getBuffer().characterIndexForPosition(x) for x in @.getCursorBufferPositions()) #for multiple cursors
                        diffs.push diff
                if diffs.length > 0
                    @codeEditor.pushDiffs diffs
            spinner = new Spinner(progress = 0, overlap = true)
            @spinnerElement = @element.appendChild spinner.element

        initialize: ->
            unless @initialized
                @initialized = true
                @onInitialize?()

        serialize: -> { deserializer: 'LunaCodeEditorTab', uri: @uri }

        getTitle: -> path.basename(@uri)

        isLunaEditor: -> true

        setUri: (uri) =>
            @getBuffer().setPath(uri)
            @getBuffer().subscribeToFileOverride(@codeEditor)
            @uri = uri
        deactivate: -> @subscribe.dispose()

        handleEvents: =>
            atom.commands.add @element,
                'core:copy':  (e) => @handleCopy(e)
                'core:cut':   (e) => @handleCut(e)
                'core:paste': (e) => @handlePaste(e)
                'core:save':  (e) => @handleSave(e)

        spans: (markWholeLines) =>
            buffer = @getBuffer()
            for s in @getSelections()
                head = s.marker.oldHeadBufferPosition
                tail = s.marker.oldTailBufferPosition
                if markWholeLines and head.isEqual tail
                    head.column = 0
                    tail.column = 0
                    tail.row += 1
                [buffer.characterIndexForPosition(head),
                 buffer.characterIndexForPosition(tail)].sort((a,b) -> a - b)

        handleCopy: (e) =>
            e.preventDefault()
            e.stopImmediatePropagation()
            @codeEditor.pushInternalEvent(tag: "Copy", _path: @uri, _selections: @spans(true))

        handleCut: (e) =>
            @codeEditor.pushInternalEvent(tag: "Copy", _path: @uri, _selections: @spans(true))

        handlePaste: (e) =>
            cbd = atom.clipboard.readWithMetadata()
            cbdData = []
            if cbd.metadata? && cbd.metadata.selections?
                for x in cbd.metadata.selections
                    cbdData.push(x.text)
            else
                cbdData[0] = cbd.text
            e.preventDefault()
            e.stopImmediatePropagation()
            @codeEditor.pushInternalEvent(tag: "Paste", _selections: @spans(), _content: cbdData)

        handleSave: (e) =>
            @setModified(false)
            e.preventDefault()
            e.stopImmediatePropagation()
            @codeEditor.pushInternalEvent(tag: "SaveFile", _path: @uri)
            oldPath = atom.project.getPaths()[0]
            projects.temporaryProject.save (newPath) =>
                @codeEditor.pushInternalEvent(tag: 'MoveProject', _oldPath : oldPath, _newPath: newPath)

        insertCode: (uri, diffs) =>
            if @uri == uri
                selections = []
                for {_newText: text, _range: range, _cursor: cursor} in diffs
                    @omitDiff(text)
                    if range?
                        @setTextInBufferRange(range, text)
                    else
                        @setText(text)
                    if cursor?
                        selections.push([[cursor._row, cursor._column], [cursor._row, cursor._column]])
                if selections.length > 0
                    @setSelectedBufferRanges(selections)

        setClipboard: (uri, text) =>
            if @uri == uri
                atom.clipboard.write(text)

        setBuffer: (uri, text) =>
            if @uri == uri
                if @getPlaceholderText() != ''
                    @setPlaceholderText ''
                for child in @element.childNodes
                    if child.id == 'luna_logo-spinner'
                        @element.removeChild child
                        break
                unless @getBuffer().getText() is text
                    @omitDiff(text)
                    selections = @getSelectedBufferRanges()
                    @getBuffer().setText(text)
                    @setSelectedBufferRanges(selections)
                    console.log "setBuffer"

        setModified: (modified) =>
            @getBuffer().setModified(modified)
            @getBuffer().emitter.emit 'did-change-modified', modified

        omitDiff: (text) =>
            @diffToOmit.add(text)
