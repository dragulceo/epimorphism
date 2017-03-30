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


$('document').ready(function(){$('#loading').fadeOut(1500);})
