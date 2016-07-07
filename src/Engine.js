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
		if(window.epiBlank && window.epiBlank[dim])
			return window.epiBlank[dim];
		if(!window.epiBlank)
			window.epiBlank = {}

		var canvas = document.createElement("canvas");
		canvas.width = dim;
		canvas.height = dim;
    var context = canvas.getContext("2d");
		context.fillStyle = "#000000";
		context.fill();

		window.epiBlank[dim] = new Image();
		window.epiBlank[dim].src = canvas.toDataURL("image/png");
		return window.epiBlank[dim];
	};
};
