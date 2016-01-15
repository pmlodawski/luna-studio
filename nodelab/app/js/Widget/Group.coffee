BaseWidget   = require ('Widget/BaseWidget')

class Group extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height
    @container = @mesh

module.exports = Group;
