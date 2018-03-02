import {Component} from 'view/Component'
import * as basegl from 'basegl'


export class Searcher extends Component
    updateModel: ({ key: @key = @key
                  , matches: @matches = @matches}) =>
        unless @def?
            div = document.createElement( 'div' );
            div.style.width = 100 + 'px';
            div.style.height = '200px';
            div.style.backgroundColor = '#CCCCCC';
            div.id = 'examplebutton'
            @def = basegl.symbol div

    updateView: =>
        @view.position.xy = @parent.node(@key).position
