customEvent = require('CustomEvent').customEvent

module.exports =
  init: ->
    $('body').append '<div id="onboarding"></div>'
    i = 0
    while i <= 14
      $('#onboarding').append require('templates/onboarding/' + i)
      ++i
    $('#onboarding .tour__exit a').click ->
      customEvent 'closeOnboarding', null
    $('#onboarding').hide()
    $('#onboarding .tour').removeClass 'active'

  show: (step) ->
    $('#onboarding').show()
    console.log 'onboarding', step
    $('#onboarding .tour').removeClass 'active'
    $('#onboarding .tour:nth-child(' + step + 1 + ')').addClass 'active'

  close: ->
    $('#onboarding').hide()
    localStorage.setItem 'onboarding', '1'

