"use strict";

// clean up
exports.uploadImageImpl = function(s){
	return function(callback){
		return function(){
			var im = window.auxImages[s];
			callback(im)();
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
