"use strict";
((AFRAME["registerComponent"])("random-color2",{schema:{},init:(function(){var randR=((Math.floor)((((Math.random)())*255))),randG=((Math.floor)((((Math.random)())*255))),randB=((Math.floor)((((Math.random)())*255)));return ((this.el["setAttribute"])("color",("rgb("+randR+","+randG+","+randB+")")));})}));
