"use strict";

// module Engine

exports.createImageImpl = function(s){
	return function(callback){
		return function(){
			var im = new Image();
			im.onload = function() {callback(im)()};
//			console.log(callback);
			im.src = s;
		};
	};
};
