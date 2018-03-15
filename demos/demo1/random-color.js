"use strict";
((AFRAME["registerComponent"])("random-color",{schema:{},init:(function(){var randomHue=((Math.floor)((((Math.random)())*360)));return ((this.el["setAttribute"])("color",("hsl("+randomHue+",100%,50%)")));})}));
