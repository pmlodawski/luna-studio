nodeEditorBaseGL = require 'luna-basegl-ui'

mountPoint = 'node-editor'
export class NodeEditor
    constructor: (@uri, @nodeBackend) ->
        nodeEditorBaseGL.install mountPoint, 'rsc/',  (ne) =>
            @nodeBackend.setView ne
            @nodeBackend.start @uri, mountPoint
