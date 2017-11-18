"use strict";

exports.loadImages = function(default_images){
  return function(images){
    return function (){

      // preloaded default image
      var img = $("#defaultImg")[0];
      window.auxImageLib = {"textures/default.png": img};
      window.loadedDefaults = ["textures/default.png"];

      var all = default_images.concat(images);
      for (var i = 0; i < all.length; i++) {
        (function (url) {
          var img = new Image();
          img.src = url;
          img.onload = function() {
            //console.log(url);
            if(default_images.indexOf(url) != -1)
              window.loadedDefaults.push(url);
            window.auxImageLib[url] = img;
          };
        })(all[i]);
      }

      window.auxImageLib['grad'] = gradientImg(128);
    }
  }
}

exports.registerImages = function(images){
  return function() {
    if(window.registeredImages == null)
      window.registeredImages = {};

    var newLookup = {};
    var key;

    for (var i = 0; i < images.length; i++){
      var url = images[i];
      if (url in window.registeredImages){ // already registered
        //console.log(url + " already registered");
        newLookup[url] = window.registeredImages[url];
      }else{ // load image
        if(url in window.auxImageLib){
          //console.log(url + " found");
          key = url;
        }else if (window.loadedDefaults.length != 0){
          var idx = Math.floor(window.loadedDefaults.length * Math.random());
          key = window.loadedDefaults[idx];
          console.log(url + " not found")
          console.log("Using default texture:" + key);
        } else{
          console.log(url + " not found")
          console.log("Using default gradient");
          key = "grad";
        }
        newLookup[url] = window.auxImageLib[key];
      }
    }
    window.registeredImages = newLookup;
  }
}

exports.getImageSrc = function(url){
  return function() {
    return window.registeredImages[url];
  }
}

function gradientImg(dim){
  var data = new Uint8ClampedArray(4 * dim * dim);

  for (var x = 0; x < dim; x++){
    var x_ofs;
    if(x < dim / 2)
      x_ofs = Math.round(255.0 * 2.0 * x / dim);
    else
      x_ofs = Math.round(255.0 * (2.0 - 2.0 * x / dim));
    for (var y = 0; y < dim; y++){
      var y_ofs;
      if(y < dim / 2)
        y_ofs = Math.round(255.0 * 2.0 * y / dim);
      else
        y_ofs = Math.round(255.0 * (2.0 - 2.0 * y / dim));
      data[4 * (dim * y + x) + 0] = 0;
      data[4 * (dim * y + x) + 1] = x_ofs;
      data[4 * (dim * y + x) + 2] = 255 - y_ofs;
      data[4 * (dim * y + x) + 3] = 255;
    }
  }

  var img = new ImageData(data, dim, dim)
  return img;
}

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
1 };
};
