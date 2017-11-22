var __, __M, __D, __T, __S;
var halt = false;
var handlerHasFocus = true;
var consoleImageSelectHandler;

window.requestFullScreen = function(id) {
  var elem = document.getElementById(id);
  if (elem.requestFullscreen) {
    elem.requestFullscreen();
  } else if (elem.msRequestFullscreen) {
    elem.msRequestFullscreen();
  } else if (elem.mozRequestFullScreen) {
    elem.mozRequestFullScreen();
  } else if (elem.webkitRequestFullscreen) {
    elem.webkitRequestFullscreen();
    }
}

window.requestExitFullScreen = function() {
  if (document.exitFullscreen) {
    document.exitFullscreen();
  } else if (document.webkitExitFullscreen) {
    document.webkitExitFullscreen();
  } else if (document.mozCancelFullScreen) {
    document.mozCancelFullScreen();
  } else if (document.msExitFullscreen) {
    document.msExitFullscreen();
  }
}

showTab = function(n) {
  $('.debugTab').hide();
  $('.debugTab.' + n).show();
  $('.tab').removeClass("selected")
  $('.tab.' + n).addClass("selected")
}

function webGLEnabled() {
  var canvas = document.createElement("canvas");
  var gl = canvas.getContext("webgl") || canvas.getContext("experimental-webgl");
  canvas = undefined;
  return (gl && gl instanceof WebGLRenderingContext);
}

// apparently preload images
var preload = ["textures/default/default0.png"];
var promises = [];
for (var i = 0; i < preload.length; i++) {
    (function(url, promise) {
        var img = new Image();
        img.onload = function() {
          promise.resolve();
        };
        img.src = url;
    })(preload[i], promises[i] = $.Deferred());
}
$.when.apply($, promises).done(function() {
  Main.main();
});

// start main application
$(document).ready( function() {
  if(webGLEnabled()){
    var c = getCookie("epimorphism_profile");
    if(c)
      $("#resolutionSel").val(c);
  }else{
    $('#loading').html("<center style='margin-top:50px;color:white;'>WebGL not available</center>")
  }
})
