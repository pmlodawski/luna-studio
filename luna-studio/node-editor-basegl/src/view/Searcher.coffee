import {Component} from 'view/Component'
import * as basegl from 'basegl'


export class Searcher extends Component
    updateModel: ({position: @position = @position}) =>
        @position ?= [0,0]
        unless @def?
            div = document.createElement( 'div' );
            div.style.width = 100 + 'px';
            div.style.height = '200px';
            div.style.backgroundColor = '#111111';
            div.id = 'examplebutton'
            @def = basegl.symbol div

    updateView: =>
        @view.position.xy = @position
