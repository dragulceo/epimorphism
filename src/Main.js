"use strict";

// module Main

exports.getURL = function(url) {
  var result;
  var request = new XMLHttpRequest();
  request.open('GET', url, false);
  request.addEventListener('load', function() {
    result = request.responseText;
  });
  request.send();
  return result;
}

exports.unsafeNull = null

//exports.requestAnimationFrame = function(x) {
  //return function() { return requestAnimationFrame(x) }
  //alert("hey!")
//}

exports.requestAnimationFrame = function(x){
  if (typeof rAF === 'undefined') {
    var rAF = (function(){
      return  window.requestAnimationFrame ||
        window.webkitRequestAnimationFrame ||
        window.mozRequestAnimationFrame    ||
        function( callback ){
          window.setTimeout(callback, 1000 / 60);
        };
    })();
  }
  return function(){
    return rAF(x);
  };
};
