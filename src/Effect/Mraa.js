"use strict";

exports.mraa = require('mraaStub');

exports.dirOut = exports.mraa.DIR_OUT;
exports.dirIn = exports.mraa.DIR_IN;


exports.gpio = function(pin) {
  return function (){
    return new exports.mraa.Gpio(pin);
  }
}


exports.writePriv = function (gpio) {
  return function (state) {
    return function () {
      return gpio.write(state);
    }
  }
}

exports.readPriv = function (gpio) {
  return function (state) {
    return function () {
      return gpio.read();
    }
  }
}


exports.dirPriv = function (gpio) {
  return function (direction) {
    return function () {
      return gpio.dir(direction);
    }
  }
}
