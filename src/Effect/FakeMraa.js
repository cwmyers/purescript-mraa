"use strict";

exports.mraa = {};

exports.gpio = function (mraa) {
  return function(pin) {
    return function(){
      return {};
    }
  }
}

exports.write = function (gpio) {
  return function (state) {
    return function () {
      console.log("state", state)
    }
  }
}

exports.dirOut = function (mraa) {
  return function (gpio) {
    return function () {
      return {};
    }
  }
}
