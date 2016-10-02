"use strict";

// module Texture

// clean up
exports.uploadImageImpl = function(s){
	return function(callback){
		return function(){
			if(window.auxImages[s]){
				var im = window.auxImages[s];
				if(window.auxImagesLoaded[s])
					callback(im)();
				else{
					var func = im.onload;
					im.onload = function(){
						func();
						callback(im)();
					}
				}
				return window.auxImages[s];
			}else{
				console.log("COULDN'T FIND IMAGE? " + s);
				console.log(window.auxImages);
				console.log(window.auxImages[s]);
				var im = new Image();
				im.onload = function() {
					window.auxImagesLoaded[s] = true;
					callback(im)();
				};
				im.src = s;
				window.auxImages[s] = im;
			};
		};
	};
};


exports.preloadImages = function(images) {
	return function(callback){
  	return function() {
  		window.auxImages = {};
			window.auxImagesLoaded = {};
  		var promises = [];
  		for (var i = 0; i < images.length; i++) {
  			(function(url, promise) {
					var img = new Image();
  				window.auxImages[url] = img;
					img.onload = function() {
						promise.resolve();
						window.auxImagesLoaded[url] = true;
					};
					img.src = url;
  			})(images[i], promises[i] = $.Deferred());
  		}
  		$.when.apply($, promises).done(function() {
				callback();
				$("#loading").fadeOut("slow");
  		});
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
