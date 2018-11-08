nodeEditorBaseGL = require 'luna-basegl-ui'

mountPoint = 'node-editor'
export class NodeEditor
    constructor: ->
        nodeEditorBaseGL.install mountPoint, 'rsc/',  (ne) =>