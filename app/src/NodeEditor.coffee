nodeEditorBaseGL = require 'luna-basegl-ui'

mountPoint = 'node-editor'

export class NodeEditor
    constructor: (@uri, @nodeBackend, @codeBackend) ->
        nodeEditorBaseGL.install mountPoint, 'rsc/',  (ne) =>
            @nodeBackend.setView ne
            @nodeBackend.start @uri, mountPoint
            @nodeBackend.pushEvent(tag: "SetFile", path: @uri)
            @codeBackend.pushInternalEvent(tag: "OpenFile", _path: @uri)
