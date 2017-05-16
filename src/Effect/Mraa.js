"use strict";

exports.mraa = require('mraa');

exports.gpio = function (mraa) {
  return function(pin) {
    return new mraa.Gpio(pin);
  }
}

exports.write = function (gpio) {
  return function (state) {
    return gpio.write(state);
  }
}

exports.dirOut = function (mraa) {
  return function (gpio) {
    return gpio.dir(mraa.DIR_OUT);
  }
}
