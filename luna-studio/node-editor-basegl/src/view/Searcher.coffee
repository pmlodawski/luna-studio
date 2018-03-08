import {Component} from 'view/Component'
import * as basegl from 'basegl'
import * as style  from 'style'


searcherRoot = 'searcher-root'

export class Searcher extends Component
    updateModel: ({ key: @key = @key
                  , mode: @mode = @mode
                  , input: @input = @input
                  , selected: @selected = @selected or 0
                  , entries: @entries = @entries or []}) =>
        unless @def?
            root = document.createElement 'div'
            root.className = ['native-key-bindings'].concat style.luna ['input', 'searcher', 'searcher--node']
            root.id = searcherRoot
            root.style.width = 100 + 'px'
            root.style.height = 200 + 'px'
            @def = basegl.symbol root

    updateView: =>
        node = @parent.node(@key)
        if node?
            @view.position.xy = node.position

        @view.domElement.innerHTML = ''

        input = document.createElement 'input'
        inputClasses = \
            if @selected == 0
                ['searcher__input', 'searcher__input-selected']
            else
                ['searcher__input']
        input.className = style.luna inputClasses
        @view.domElement.appendChild input

        if @entries.length > 0
            results = document.createElement 'div'
            results.className = style.luna ['searcher__results']

            resultsList = document.createElement 'div'
            resultsList.className = style.luna ['searcher__results__list']
            results.appendChild resultsList

            @view.domElement.appendChild results

            i = 0
            for entry in @entries.slice @selected
                resultName = document.createElement 'div'
                resultName.className = style.luna ['searcher__results__item__name']

                prefixSpan = document.createElement 'span'
                prefixSpan.className = style.luna ['searcher__pre']
                prefixSpan.innerHTML = entry.className or ''
                resultName.appendChild prefixSpan

                nameSpan = document.createElement 'span'
                resultName.appendChild nameSpan

                name = entry.name
                last = 0
                if entry.highlights instanceof Array
                    for highlight in entry.highlights
                        normName = name.slice last, highlight.start
                        highName = name.slice highlight.start, highlight.end
                        last = highlight.end

                        normSpan = document.createElement 'span'
                        normSpan.innerHTML = normName
                        nameSpan.appendChild normSpan

                        highSpan = document.createElement 'span'
                        highSpan.className = style.luna ['searcher__hl']
                        highSpan.innerHTML = highName
                        nameSpan.appendChild highSpan
                normSpan = document.createElement 'span'
                normSpan.innerHTML = name.slice last
                nameSpan.appendChild normSpan

                result = document.createElement 'div'
                result.className = style.luna if i == 0 then ['searcher__results__item', 'searcher__results__item--selected'] else ['searcher__results__item']
                result.appendChild resultName
                resultsList.appendChild result
                i++
