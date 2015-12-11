class Group
  constructor: (widgetId) ->
    @widgetId = widgetId
    @mesh = @container = new THREE.Group()

module.exports = Group;
