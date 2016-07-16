"use strict";

// module Command

exports.saveCanvas = function() {
	var dataURL = $("#glcanvas")[0].toDataURL();

	$.ajax({
		type: "POST",
		url: "http://localhost:9000",
		data: {
			imgBase64: dataURL
		}
	}).done(function(o) {
		console.log('saved');
	});
};


exports.saveCanvas = $.saveCanvas
