{View}    = require 'atom-space-pen-views'
analytics = require './gen/analytics'
fs        = require 'fs-plus'
path      = require 'path'
yaml      = require 'js-yaml'
{VM}      = require 'vm2'

vm     = new VM
            timeout: 1000
            sandbox:
                window: window
                storage: {}

welcomeGuide = steps: []

encoding = 'utf8'
highlightClass = 'luna-guide__highlight'
pointerWindowClass = 'luna-guide__pointer--window'

module.exports =
    class VisualGuide extends View
        constructor: ->
            super

        @content: ->
            @div =>
                @div class: 'luna-guide__pointer', outlet: 'pointer'
                @div class: 'luna-guide__tooltip', outlet: 'tooltip', =>
                    @p
                        class: 'luna-guide__description'
                        outlet: 'guideDescription'
                @div class: 'luna-guide__navbar-positioner', =>
                    @div class: 'luna-guide__navbar', =>
                        @div
                            class: 'luna-guide__title'
                            outlet: 'guideTitle'
                        @button
                            outlet: 'buttonHide'
                            class: 'luna-guide__close-icon close icon icon-x'
                        @div class: 'luna-guide__buttons', =>
                            @button
                                outlet: 'buttonContinue'
                                class: 'luna-guide__button luna-guide__button--continue'
                                'Next'
                            @button
                                outlet: 'buttonDoIt'
                                class: 'luna-guide__button luna-guide__button--doit'
                                'Next'
                            # @button
                            #     outlet: 'buttonDisable'
                            #     class: 'luna-guide__button luna-guide__button--disable luna-guide__button--link'
                            #     'Do not show again'

        initialize: =>
            @buttonHide.on 'click', @detach
            # @buttonDisable.on 'click', @disable
            @buttonContinue.on 'click', =>
                @nextStep @nextStepNo
                @buttonContinue.hide()
            @buttonContinue.hide()
            @buttonDoIt.on 'click', =>
                @buttonDoIt.hide()
                @doIt()
            @buttonDoIt.hide()

        nextStep: (nextStepNo) =>
            if nextStepNo != @nextStepNo
                return

            if @currentStep? and @currentStep.after?
                try
                    vm.run @currentStep.after
                catch error
                    console.error error

            @unsetHighlightedElem()

            projectPath = atom.project.getPaths()[0]
            projectPath ?= '(None)'
            analytics.track 'LunaStudio.Guide.Step',
                number: @nextStepNo
                name: path.basename projectPath
                path: projectPath
            @currentStep = @guide.steps[@nextStepNo]
            @nextStepNo++

            if @currentStep?
                @target = @currentStep.target
                @target ?= {}
                @target.action ?= 'proceed'

                tooltipDefaultOffset = 20
                @tooltipOffset = @currentStep.offset
                @tooltipOffset ?= {}
                @tooltipOffset.left   ?= tooltipDefaultOffset
                @tooltipOffset.right  ?= tooltipDefaultOffset
                @tooltipOffset.top    ?= tooltipDefaultOffset
                @tooltipOffset.bottom ?= tooltipDefaultOffset
                @displayStep()
            else
                @detach()


        findTargetedElem: =>
            targetedElem = null
            if @target.className
                if typeof @target.className == 'string'
                    targetedElem = document.getElementsByClassName(@target.className)[0]
                else
                    for t in @target.className
                        if targetedElem?
                            targetedElem = targetedElem.getElementsByClassName(t)[0]
                        else
                            targetedElem = document.getElementsByClassName(t)[0]
                            unless targetedElem?
                                break
            else if @target.id
                targetedElem = document.getElementById(@target.id)
            else if @target.custom
                targetedElem = vm.run @target.custom
            return targetedElem

        setHighlightedElem: =>
            @highlightedElem = @findTargetedElem()
            if @highlightedElem?
                @highlightedElem.classList.add highlightClass
            @updatePointer()

        unsetHighlightedElem: =>
            if @highlightedElem?
                @highlightedElem.classList.remove highlightClass
                @highlightedElem = null
            @updatePointer()

        installHandlers: =>
            if @highlightedElem?
                hgElem = @highlightedElem
                nextStepNo = @nextStepNo
                highlightedRect = hgElem.getBoundingClientRect()
                if highlightedRect.width != 0 and highlightedRect.height != 0
                    if @target.action is 'value'
                        @buttonDoIt.show()
                        oldHandlers = hgElem.oninput
                        hgElem.oninput = =>
                            if hgElem? and (hgElem.value is @target.value)
                                hgElem.oninput = oldHandlers
                                @nextStep nextStepNo
                    else if @target.action.includes ':'
                        @buttonDoIt.show()
                        handler = {}
                        handler[@target.action] = =>
                            @disposable.dispose()
                            @nextStep nextStepNo
                        @disposable = atom.commands.add hgElem, handler
                    else if hgElem?
                        @buttonDoIt.show()
                        oldHandlers = hgElem[@target.action]
                        hgElem[@target.action] = =>
                            if hgElem?
                                hgElem[@target.action] = oldHandlers
                                @nextStep nextStepNo

        doIt: =>
            mkEvent = (name) => new Event name,
                                    view: window
                                    bubbles: true
                                    cancelable: true
            if @highlightedElem?
                if @target.action is 'value'
                    event = mkEvent 'input',
                    @highlightedElem.value = @target.value
                    event.simulated = true
                    @highlightedElem.dispatchEvent(event)
                else if @target.action.includes ':'
                    view = atom.views.getView @highlightedElem
                    atom.commands.dispatch view, @target.action
                else if @highlightedElem?
                    if @target.action.startsWith 'on'
                        action = @target.action.slice 2
                    else
                        action = @target.action
                    if action is 'click'
                        @highlightedElem.dispatchEvent mkEvent 'mousedown'
                        @highlightedElem.dispatchEvent mkEvent 'mouseup'
                    @highlightedElem.dispatchEvent mkEvent action

        displayStep: (retry = false) =>
            @setHighlightedElem()
            windowRect = document.body.getBoundingClientRect()

            if @target.action is 'proceed'
                @buttonContinue.show()
            else if not @highlightedElem?
                @buttonDoIt.hide()
                unless retry
                    @guideTitle[0].innerText = @currentStep.title
                    @guideDescription[0].innerText = 'Please wait...'
                    tooltipRect = @tooltip[0].getBoundingClientRect()
                    tooltipLeft = (windowRect.width - tooltipRect.width)/2
                    tooltipTop  = (windowRect.height - tooltipRect.height)/2
                    @tooltip[0].style.top = tooltipTop + 'px'
                    @tooltip[0].style.left = tooltipLeft + 'px'

                setTimeout (=> @displayStep(true)), 300
                return

            @installHandlers()

            @guideTitle[0].innerText = @currentStep.title
            @guideDescription[0].innerText = @currentStep.description
            tooltipRect = @tooltip[0].getBoundingClientRect()
            tooltipLeft = (windowRect.width - tooltipRect.width)/2
            tooltipTop  = (windowRect.height - tooltipRect.height)/2

            if @highlightedElem?
                highlightedRect = @highlightedElem.getBoundingClientRect()
                if highlightedRect.width != 0 and highlightedRect.height != 0
                    if highlightedRect.left > tooltipRect.width + @tooltipOffset.left
                        tooltipLeft = highlightedRect.left - tooltipRect.width - @tooltipOffset.left
                        tooltipTop = highlightedRect.top + highlightedRect.height/2 - tooltipRect.height/2
                    else if highlightedRect.right + tooltipRect.width + @tooltipOffset.right < windowRect.width
                        tooltipLeft = highlightedRect.right + @tooltipOffset.right
                        tooltipTop = highlightedRect.top + highlightedRect.height/2 - tooltipRect.height/2
                    else if highlightedRect.top > tooltipRect.height + @tooltipOffset.top
                        tooltipTop = highlightedRect.top - tooltipRect.height - @tooltipOffset.top
                    else if highlightedRect.bottom + tooltipRect.height + @tooltipOffset.bottom < windowRect.height
                        tooltipTop = highlightedRect.bottom + @tooltipOffset.bottom


            @tooltip[0].style.top = tooltipTop + 'px'
            @tooltip[0].style.left = tooltipLeft + 'px'

            if @highlightedElem?
                @retryWhenHighlightedElementIsGone()

        retryWhenHighlightedElementIsGone: =>
            targetedElem = @findTargetedElem()
            if targetedElem? and targetedElem.classList.contains highlightClass
                setTimeout @retryWhenHighlightedElementIsGone, 100
            else
                @displayStep(retry: true)


        updatePointer: =>
            if @highlightedElem?
                if (@highlightedElem.classList.contains 'luna-studio-window') or (@highlightedElem.classList.contains 'luna-studio-mount')
                    @pointer[0].classList.add pointerWindowClass
                else
                    @pointer[0].classList.remove pointerWindowClass
                highlightedRect = @highlightedElem.getBoundingClientRect()
                unless highlightedRect.width is 0 or highlightedRect.height is 0
                    @pointer.show()
                    @pointer[0].style.width  = highlightedRect.width + 'px'
                    @pointer[0].style.height = highlightedRect.height + 'px'
                    @pointer[0].style.top  = highlightedRect.top + 'px'
                    @pointer[0].style.left = highlightedRect.left + 'px'
                    window.requestAnimationFrame @updatePointer
                else
                    @pointer.hide()
            else
                @pointer.hide()

        attach: =>
            @panel ?= atom.workspace.addHeaderPanel({item: this, visible: false})
            @panel.show()

        detach: =>
            if @panel.isVisible()
                @panel.hide()

        disable: =>
            @disableGuide()
            @detach()

        startProject: =>
            projectPath = atom.project.getPaths()[0]
            if projectPath?
                guidePath = path.join projectPath, 'guide.yml'
                fs.readFile guidePath, encoding, (err, data) =>
                    unless err
                        parsed = yaml.safeLoad data
                        if parsed? && not parsed.disabled
                            @start parsed, guidePath

        disableGuide: =>
            if @guidePath?
                @guide.disabled = true
                data = yaml.safeDump(@guide)
                fs.writeFile @guidePath, data, encoding, (err) =>
                if err?
                    console.error err
            else
                atom.config.set('luna-studio.showWelcomeGuide', false)


        start: (@guide, @guidePath) =>
            @guide ?= welcomeGuide
            @nextStepNo = 0
            @attach()
            @nextStep 0
