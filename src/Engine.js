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


exports.audioData = function(analyser) {
	return function() {
		var freqData = new Uint8Array(analyser.frequencyBinCount);
		var timeData = new Uint8Array(analyser.frequencyBinCount);
		analyser.getByteFrequencyData(freqData);
		analyser.getByteTimeDomainData(timeData);

		var data = new Uint8Array(2.0 * analyser.frequencyBinCount);
		data.set(freqData);
		data.set(timeData, freqData.length);
		return data;
	}
}



// not generally supported.  works on chrome atm
exports.initAudioAnalyzer = function(bufferSize){
	return function(){
		function streamError(err) {
			console.log("The following error occured getting user audio: " + err.name);
		}

		function gotStream(ctx, analyser){
			return function(stream) {
				var mediaStreamSource = ctx.createMediaStreamSource(stream);
				mediaStreamSource.connect(analyser);
			}
		}

		var ctx = new AudioContext();
		var analyser = ctx.createAnalyser();
		analyser.fftSize = bufferSize * 2;
		analyser.smoothingTimeConstant = 0.9;
		analyser.minDecibels = -85;

		navigator.webkitGetUserMedia({audio:true}, gotStream(ctx, analyser), streamError);

		return analyser;
	}
}
