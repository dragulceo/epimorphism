"use strict";

exports.registerEventHandler = function(handler) {
  return function() {
    window.eventHandler = function(msg){handler(msg)()};

		// a little ghetto
		__  = function(msg) {handler(msg)();}
		__M = function(msg) {handler("setP main.application." + msg)();}
		__D = function(msg) {handler("setP disp." + msg)();}
		__T = function(msg) {handler("setT " + msg)();}
		__S = function(msg) {handler("scr " + msg)();}
  }
};

exports.registerKeyHandler = function(handler) {
  return function(){
		document.onkeypress = function(event) {
			var code = event.which || event.keyCode
			var chr = String.fromCharCode(code);
			var cmd = handler(chr)();
// fix me
//			if(cmd != "null")
			//				event.preventDefault();
			if(handlerHasFocus){
				window.eventHandler(cmd);
			}
		};
	};
};


exports.addGlobalEventListeners = function(handler) {
  return function() {
		// FULL SCREEN
		var fsHandler = function(event){
			var isFullScreen = document.fullScreen ||
          document.mozFullScreen ||
          document.webkitIsFullScreen;
			if (isFullScreen) {
				$('#exitFullScreen').removeClass('hide');
				$('#enterFullScreen').addClass('hide');
			} else {
				$('#exitFullScreen').addClass('hide');
				$('#enterFullScreen').removeClass('hide');
			}
			handler('fullWindow')();
		};

		$(document).on('webkitfullscreenchange mozfullscreenchange fullscreenchange MSFullscreenChange', fsHandler);

		// RESIZE
		var resizeHandler = function(){
			if($('#glcanvas').length){
				handler('initLayout')();
			}
		}

		$(window).resize(resizeHandler);

		$("#menu-icon").hover(function(){$("#menuContainer").fadeIn("slow");$("#menu-icon").fadeOut("slow");},function(){});
		$("#menuContainer").hover(function(){},function(){$("#menuContainer").fadeOut("slow");$("#menu-icon").fadeIn("slow");});

		var resChangeHandler =
				function (){
					setCookie("epimorphism_profile", this.value)
					handler("setEngineProfile " + this.value)();
				};

		$("#resolutionSel").on('change', resChangeHandler);

		// PAUSE
		var pauseHandler = function(){
			// a little sketchy, the order wrt update layout, pause, is important
			// and I'm not sure its guaranteed
			handler('updateLayout')();
			var paused = this.innerHTML == "Pause";
			window.a = this.innerHTML;
			this.innerHTML = (paused ? "Unpause" : "Pause");
			$('.consoleUI').toggle();
			handler('pause')();
		}

		$("button#pause").on('click', pauseHandler);
  }
};


exports.registerAuxImages = function(imgs) {
  return function() {
		var imgDiv = $('#allImages');
		for (var i = 0; i < imgs.length; i++) {
			var src = imgs[i];
			imgDiv.append($("<img onclick='consoleImageSelectHandler(this)' src='" + src +"' data-src='" + src +"'>"));
		};
	}
};

exports.doneLoading = function() {
	$('#loading').fadeOut(1500);
}
