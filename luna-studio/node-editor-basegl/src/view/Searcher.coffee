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
                resultName.innerHTML = entry.name

                result = document.createElement 'div'
                result.className = if i == 0 then style.luna ['searcher__results__item', 'searcher__results__item--selected'] else ['searcher__results__item']
                result.appendChild resultName
                resultsList.appendChild result
                i++
