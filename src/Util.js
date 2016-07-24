"use strict";

// module Util
exports.unsafeNull = null

exports.unsafeSetAttr = function(obj){
	return function(attrn){
		return function(attrv){
			var obj2 = $.extend({}, obj)
			obj2[attrn] = attrv;
			return obj2;
		};
	};
};

exports.unsafeGetAttr = function(obj){
	return function(attrn){
		return obj[attrn];
	};
};

exports.unsafeCast = function(obj){
  return obj;
};


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


window.lgCnt = 0;
exports.tLg = function tLg(x) {
	if(window.lgCnt < 10)
		console.log(x);
	window.lgCnt = window.lgCnt + 1;
}


exports.lg = function lg(x) {
	console.log(x);
}

exports.stick = function stick(x) {
	$.stick = x;
}


exports.now = function () {
  return performance.now();
};

exports.unsafeEval = function (s) {
  return function () {eval(s);};
}

exports.winLog = function (x) {
  return function() {
    $('#container')[0].innerHTML = "<pre>" + x.replace(new RegExp("\n", 'g'), "<br/>") + "</pre>";
  };
}

exports.replaceAll = function(search) {
  return function (replacement) {
    return function (target) {
      return target.replace(new RegExp(search, 'g'), replacement);
    };
  };
};


exports.boolFromString = function(s) {
	return (s == "true");
}

exports.numFromStringImpl = function (just) {
  return function (nothing) {
    return function (s) {
      /* jshint bitwise: false */
      var i = parseFloat(s);
      return i != NaN ? just(i) : nothing;
    };
  };
};


exports.urlGetImpl = function (left) {
  return function (right) {
    return function (url) {
			return function() {
				var result;
				var request = new XMLHttpRequest();
				request.open('GET', url, false);
				request.addEventListener('load', function() {
					if (request.status == 200 || request.status == 304 )
						result = right(request.responseText);
					else
						result = left(url + " : " + request.statusText);
				});

				request.send();
				return result;
			};
    };
  };
};



exports.cxFromStringImpl = function (tuple) {
  return function (just) {
    return function (nothing) {
      return function (s) {
	var real, imag, regex, match, a, b, c;
	// TODO: Make this work better-er
	regex = /^([-+]?(\d+|\d*\.\d+))([-+](\d+|\d*\.\d+))[ij]$/i;
	s = String(s).replace (/\s+/g, '');

	match = s.match (regex);
	if (!match) {
	  return nothing;
	}

	a = match[1];
	b = match[3];

	real = parseFloat (a);
	imag = parseFloat (b);
	var tmp = tuple(real);
	return just(tmp(imag));
      };
    };
  };
};

exports.halt = function() {
	console.log("HALT");
	halt = true;
};


exports.isHalted = function() {
	return halt;
};


exports.uuid = function() {
	function s4() {
		return Math.floor((1 + Math.random()) * 0x10000)
			.toString(16)
			.substring(1);
	}
	return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
		s4() + '-' + s4() + s4() + s4();
};



exports.rndstr = function() {
	var text = "";
  var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

  for( var i=0; i < 10; i++ )
    text += possible.charAt(Math.floor(Math.random() * possible.length));

  return text;
};


exports.gmod = function(n) {
	return function(m) {
    return ((n%m)+m)%m;
	};
};


exports.random = function() {
	return Math.random();
};


exports.randInt = function(n) {
	return function() {
		return Math.floor(n * Math.random());
	};
};
