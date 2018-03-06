import {Component} from 'view/Component'
import * as basegl from 'basegl'
import * as style  from 'style'


searcherRoot = 'searcher-root'

export class Searcher extends Component
    updateModel: ({ key: @key = @key
                  , mode: @mode = @mode
                  , selected: @selected = @selected or 0
                  , matches: @matches = @matches}) =>
        unless @def?
            root = document.createElement 'div'
            root.id = searcherRoot
            root.style.width = 100 + 'px'
            root.style.height = 200 + 'px'
            @def = basegl.symbol root

    updateView: =>
        @view.position.xy = @parent.node(@key).position

        @view.domElement.innerHTML = ''

        input = document.createElement 'input'
        inputClasses = \
            if @selected == 0
                ['searcher__input', 'searcher__input-selected']
            else
                ['searcher__input']
        input.className = style.luna inputClasses
        @view.domElement.appendChild input

        if @matches.length > 0
            @results = document.createElement 'div'
            @results.className = style.luna ['searcher__results']

            @resultsList = document.createElement 'div'
            @resultsList.className = style.luna ['searcher__results__list']
            @results.appendChild @resultsList

            @view.domElement.appendChild @results

            i = 0
            for match in @matches.slice @selected
                resultName = document.createElement 'div'
                resultName.className = style.luna ['searcher__results__item__name']
                resultName.append match.name

                result = document.createElement 'div'
                result.className = if i == 0 then style.luna ['searcher__results__item', 'searcher__results__item--selected'] else ['searcher__results__item']
                result.appendChild resultName
                @resultsList.appendChild result
                i++
