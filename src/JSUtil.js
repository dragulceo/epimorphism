"use strict";

// module JSUtil

exports.unsafeURLGet = function(url) {
  return function (){
    var result;
    var request = new XMLHttpRequest();
    request.open('GET', url, false);
    request.addEventListener('load', function() {
      result = request.responseText;
    });
    request.send();
    return result;
  }
}

exports.unsafeNull = null


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


exports.unsafeLog = function unsafeLog(x) {
  return function () {
    console.log(x);
  }
}

window.cnt = 0;
exports.reallyUnsafeLog = function reallyUnsafeLog(x) {
	if(window.cnt < 10)
		console.log(x);
	window.cnt = window.cnt + 1;
}


exports.now = function () {
  return Date.now();
};

exports.unsafeEval = function (s) {
  return function () {eval(s);};
}

exports.winLog = function (x) {
  return function() {
    window.document.body.innerHTML = "<pre>" + x.replace(new RegExp("\n", 'g'), "<br/>") + "</pre>";
  };
}

exports.replaceAll = function(search) {
  return function (replacement) {
    return function (target) {
      return target.replace(new RegExp(search, 'g'), replacement);
    };
  };
};
