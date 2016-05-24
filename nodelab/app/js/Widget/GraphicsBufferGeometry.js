"use strict";

THREE.GraphicsBufferGeometry = function ( boxes ) {

	THREE.BufferGeometry.call( this );

	this.type = 'GraphicsBufferGeometry';

  var numBoxes = boxes.length;
	var vertices = new Float32Array( numBoxes * 2 * 2 * 3 );
	var uvs      = new Float32Array( numBoxes * 2 * 2 * 2 );
	var sizes    = new Float32Array( numBoxes * 2 * 2 * 2 );
	var indices  = new ( ( numBoxes * 4 ) > 65535 ? Uint32Array : Uint16Array )( numBoxes * 6 );

	var offset3 = 0;
  var offset2 = 0;
  var offsetInd = 0;

  boxes.forEach(function(box, ix){
    vertices[offset3    ] = box._boxPosition._x - box._boxSize._x / 2;
    vertices[offset3 + 1] = box._boxPosition._y - box._boxSize._y / 2;
    uvs[offset2]          = 0;
    uvs[offset2 + 1]      = 0;
    sizes[offset2]        = box._boxSize._x;
    sizes[offset2 + 1]    = box._boxSize._y;
    offset3 += 3;
    offset2 += 2;

    vertices[offset3    ] = box._boxPosition._x + box._boxSize._x / 2;
    vertices[offset3 + 1] = box._boxPosition._y - box._boxSize._y / 2;
    uvs[offset2]          = 1;
    uvs[offset2 + 1]      = 0;
    sizes[offset2]        = box._boxSize._x;
    sizes[offset2 + 1]    = box._boxSize._y;
    offset3 += 3;
    offset2 += 2;

    vertices[offset3    ] = box._boxPosition._x - box._boxSize._x / 2;
    vertices[offset3 + 1] = box._boxPosition._y + box._boxSize._y / 2;
    uvs[offset2]          = 0;
    uvs[offset2 + 1]      = 1;
    sizes[offset2]        = box._boxSize._x;
    sizes[offset2 + 1]    = box._boxSize._y;
    offset3 += 3;
    offset2 += 2;

    vertices[offset3    ] = box._boxPosition._x + box._boxSize._x / 2;
    vertices[offset3 + 1] = box._boxPosition._y + box._boxSize._y / 2;
    uvs[offset2]          = 1;
    uvs[offset2 + 1]      = 1;
    sizes[offset2]        = box._boxSize._x;
    sizes[offset2 + 1]    = box._boxSize._y;
    offset3 += 3;
    offset2 += 2;

    indices[offsetInd + 0] = 4 * ix + 3;
    indices[offsetInd + 1] = 4 * ix + 1;
    indices[offsetInd + 2] = 4 * ix + 0;
    indices[offsetInd + 3] = 4 * ix + 0;
    indices[offsetInd + 4] = 4 * ix + 2;
    indices[offsetInd + 5] = 4 * ix + 3;
    offsetInd += 6;
  });

	this.setIndex( new THREE.BufferAttribute( indices, 1 ) );
	this.addAttribute( 'position', new THREE.BufferAttribute( vertices, 3 ) );
	this.addAttribute( 'uv',       new THREE.BufferAttribute( uvs,      2 ) );
	this.addAttribute( 'boxSize',  new THREE.BufferAttribute( sizes,    2 ) );
};

THREE.GraphicsBufferGeometry.prototype = Object.create( THREE.BufferGeometry.prototype );
THREE.GraphicsBufferGeometry.prototype.constructor = THREE.GraphicsBufferGeometry;
