"use strict";

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

		if(navigator.webkitGetUserMedia){
			navigator.webkitGetUserMedia({audio:true}, gotStream(ctx, analyser), streamError);
		}

		return analyser;
	}
}

/*
exports.initAudioAnalyzer2 = function(bufferSize){
	return function(){
		var ctx = new AudioContext();
		var audio = document.getElementById('myAudio');
		var audioSrc = ctx.createMediaElementSource(audio);
		var analyser = ctx.createAnalyser();
		analyser.fftSize = bufferSize * 2;
		analyser.smoothingTimeConstant = 0.9;
		analyser.minDecibels = -85;

		audioSrc.connect(analyser);
		audioSrc.connect(ctx.destination);

		return analyser;
	}
}
*/

exports.audioData = function(analyser) {
	return function() {
		var freqData = new Uint8Array(analyser.frequencyBinCount);
		var timeData = new Uint8Array(analyser.frequencyBinCount);
		analyser.getByteFrequencyData(freqData);
		analyser.getByteTimeDomainData(timeData);

		var data = new Uint8Array(2.0 * analyser.frequencyBinCount);
		data.set(freqData);
		data.set(timeData, freqData.length);
		return freqData;
	}
}
