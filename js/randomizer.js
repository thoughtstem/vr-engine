AFRAME.registerComponent('random-color', {
  schema: {
    sat: {default: '100%'},
    lum: {default: '50%'}
  },
  init: function(){
    var randomHue = Math.floor(Math.random() * 360);
    var data = this.data;
    var sat = data.sat;
    var lum = data.lum;
    this.el.setAttribute('color', "hsl("+ randomHue +"," + sat + "," + lum + ")");
  }
});

AFRAME.registerComponent('random-position', {
  schema: {
    min: {default: {x: -10, y: 1, z: -10}, type: 'vec3'},
    max: {default: {x: 10, y: 1, z: 10}, type: 'vec3'}
  },
  init: function(){
    var data = this.data;
    var max = data.max;
    var min = data.min;
    this.el.setAttribute('position', {
      x: Math.random() * (max.x - min.x) + min.x,
      y: Math.random() * (max.y - min.y) + min.y,
      z: Math.random() * (max.z - min.z) + min.z
    });
  }
});

AFRAME.registerComponent('random-bounce', {
  schema: {
    min: {default: 1},
    max: {default: 3},
    dur: {default: null}
  },
  init: function(){
    var data = this.data;
    var max = data.max;
    var min = data.min;
    var dur = data.dur;
    if (dur == null){
      dur = Math.floor(Math.random() * 1500 + 400)
    };
    var pos = this.el.getAttribute("position");
    var randomHeight = Math.random() * (max - min) + min
    var animation = document.createElement("a-animation");
    animation.setAttribute("attribute","position");
    animation.setAttribute("to", pos.x + " " + (pos.y + randomHeight) + " " + pos.z);
    animation.setAttribute("direction", "alternate");
    animation.setAttribute("dur", dur);
    animation.setAttribute("repeat", "indefinite");
    this.el.appendChild(animation);
  }
});

AFRAME.registerComponent('random-bounce-look', {
  schema: {
    min: {default: 2},
    max: {default: 5},
    dur: {default: null}
  },
  init: function(){
    var data = this.data;
    var max = data.max;
    var min = data.min;
    var dur = data.dur;
    if (dur == null){
      dur = Math.floor(Math.random() * 800 + 200);
    };
    var pos = this.el.getAttribute("position");
    var randomHeight = Math.random() * (max - min) + min
    var animation = document.createElement("a-animation");
    animation.setAttribute("attribute","position");
    animation.setAttribute("begin", "mouseenter");
    animation.setAttribute("to", pos.x + " " + (pos.y + randomHeight) + " " + pos.z);
    animation.setAttribute("direction", "alternate");
    animation.setAttribute("dur", dur);
    animation.setAttribute("repeat", "1");
    this.el.appendChild(animation);
  }
});