"use strict";

// module Engine

exports.createImageImpl = function(s){
	return function(callback){
		return function(){
  		//var im = window.images[s];
			//callback(im)();
			var im = new Image();
			im.onload = function() {callback(im)()};
			//console.log(callback);
			console.log(s);
			im.src = s;
		};
	};
};

var image_list = [	"http://localhost:8000/textures/simplegeom/grid_1.png"];
/*"http://localhost:8000/textures/simplegeom/tile_rainbow1.png",
"http://localhost:8000/textures/simplegeom/tile_rainbow2.png",
"http://localhost:8000/textures/simplegeom/tile_grid1.png",
"http://localhost:8000/textures/simplegeom/tile_grid2.png",
"http://localhost:8000/textures/simplegeom/tile_grid3.png",
"http://localhost:8000/textures/misc/tile_vector1.png",
"http://localhost:8000/textures/misc/tile_vector2.png",
"http://localhost:8000/textures/misc/tile_vector3.png",
"http://localhost:8000/textures/misc/tile_vector4.png",
"http://localhost:8000/textures/Alexgrey/grey1.png",
"http://localhost:8000/textures/flowers/flowers3.png",
"http://localhost:8000/textures/flowers/flowers5.png",
"http://localhost:8000/textures/flowers/flowers6.png",
"http://localhost:8000/textures/flowers/flowers7.png",
"http://localhost:8000/textures/flowers/lines1.png",
"http://localhost:8000/textures/misc/tile_floral1.png",
"http://localhost:8000/textures/misc/tile_islamic1.png",
"http://localhost:8000/textures/misc/tile_oct1.png",
"http://localhost:8000/textures/nontile/flowers3.png",
"http://localhost:8000/textures/nontile/tile_dodec.png",
"http://localhost:8000/textures/nontile/tile_spidro.png",
"http://localhost:8000/textures/psych/psych_12.png",
"http://localhost:8000/textures/psych/psych_13.png",
"http://localhost:8000/textures/psych/psych_5.png",
"http://localhost:8000/textures/psych/psych_6.png",
"http://localhost:8000/textures/psych/psych_7.png",
"http://localhost:8000/textures/simplegeom/grid_2.png",
"http://localhost:8000/textures/simplegeom/tile_grid6.png",
"http://localhost:8000/textures/simplegeom/tile_hexagons1.png",
"http://localhost:8000/textures/simplegeom/tile_psych1.png",
"http://localhost:8000/textures/stoopid/hamster.png",
"http://localhost:8000/textures/stoopid/mouse.png",
"http://localhost:8000/textures/stoopid/puppy.png",
"http://localhost:8000/textures/stoopid/stoopid_1.png",
"http://localhost:8000/textures/stoopid/stoopid_4.png",
"http://localhost:8000/textures/stoopid/stoopid_6.png",
"http://localhost:8000/textures/stoopid/suckit.png",
"http://localhost:8000/textures/stoopid/tree.png",
"http://localhost:8000/textures/Vasarely/tile_vasarely2.png",
"http://localhost:8000/textures/Vasarely/tile_vasarely4.png",
"http://localhost:8000/textures/Vasarely/tile_vasarely7.png",
"http://localhost:8000/textures/Vasarely/tile_vasarely8.png"];
*/

exports.initAuxImages = function(){
	window.images = {};
	image_list.forEach(function(img){
		window.images[img] = new Image();
		window.images[img].onload = function(){console.log("loaded ", img);}
		window.images[img].src = img;
	});
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
