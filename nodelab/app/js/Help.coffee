customEvent = require('CustomEvent').customEvent
Onboarding = require('Onboarding')

initialize = ->
  $('body').append require('templates/tutorial')()
  $('.tutorial-box').hide()

  hideTutorial = ->
    $('.tutorial-box').hide()
    $('#canvas2d').focus()
    localStorage.setItem 'tutorial', '1'

  $('.tutorial-box button, .tutorial-box .tutorial-bkg').click hideTutorial

  $('.tutorial-box').keydown (ev) ->
    escKeyCode = 27
    if ev.keyCode == escKeyCode
      hideTutorial()
  $('.tutorial').click (e) ->
    e.preventDefault()
    $('.tutorial-box').show().focus()

  $('.startOnboarding').click ->
    customEvent 'startOnboarding', null

  Onboarding.init()

module.exports =
  initialize: initialize
