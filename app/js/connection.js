"use strict";

function Connection(id) {
  this.id = id;

  this.geometry = new THREE.Geometry();

  this.geometry.vertices.push(new THREE.Vector3(0, 0, 0));
  this.geometry.vertices.push(new THREE.Vector3(0, 0, 0));

  this.geometry.dynamic = true;

  this.material = new THREE.LineBasicMaterial({
        color: 0x2255DD,
        linewidth: 1.0,
        linecap: 'round'
    });

  this.mesh = new THREE.Line(this.geometry, this.material);

  this.mesh.position.z = 0.0001;
}

Connection.prototype.setPos = function(x1, y1, x2, y2) {
  this.geometry.vertices[0].x = x1;
  this.geometry.vertices[0].y = y1;
  this.geometry.vertices[1].x = x2;
  this.geometry.vertices[1].y = y2;
  this.geometry.verticesNeedUpdate = true;
};

module.exports = Connection;
