/* global exports */
"use strict";

// module Library

exports.numFromStringImpl = function (just) {
  return function (nothing) {
    return function (s) {
      /* jshint bitwise: false */
      var i = parseFloat(s);
      return i != NaN ? just(i) : nothing;
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


exports.asModRef = function(x) {
  return x;
};
