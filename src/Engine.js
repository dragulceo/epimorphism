"use strict";

// module Engine

exports.createImageImpl = function(s){
	return function(callback){
		return function(){
			if(!window.images)
				window.images = {};
			if(window.images[s]){
				callback(window.images[s])();
				return window.images[s];
			}

			var im = new Image();
			im.onload = function() {callback(im)()};
			im.src = s;
			window.images[s] = im;
		};
	};
};

exports.emptyImage = function(dim){
	return function(){
		if(window.epiBlank)
			return window.epiBlank;
		var canvas = document.createElement("canvas");
		canvas.width = dim;
		canvas.height = dim;
    var context = canvas.getContext("2d");
		context.fillStyle = "#000000";
		context.fill();

		window.epiBlank = new Image();
		window.epiBlank.src = canvas.toDataURL("image/png");
		return window.epiBlank;
	};
};
