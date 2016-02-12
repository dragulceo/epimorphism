"use strict";

// module UI

exports.onKeyDown = function(handleKeyDown) {
  return function() {
    document.onkeydown = function(event) {handleKeyDown(event)()};
  };
};

exports.onKeyUp = function(handleKeyUp) {
  return function() {
    document.onkeyup = function(event) {handleKeyUp(event)()};
  };
};

exports.eventGetKeyCode  = function(event) {
  return (event.keyCode);
};


exports.registerEventHandler = function(handler) {
  return function() {
    document.eventHandler = function(event) {handler(event)()};
  };
};
