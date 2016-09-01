"use strict";

// module Console

exports.addEventListeners = function() {
	// SWITCH CHILD
	var switchChildHandler = function(){
		var mid = this.dataset.mid;
		var cname = this.dataset.cname;
		var mn = this.value;

		var cmd = "scr " + mid + " switchChild childN:" + cname + " to:" + mn + " spd:1.0"
		window.eventHandler(cmd)
		$("button#pause").click();
	}

	$('.switchChild').on('change', switchChildHandler);

	var componentClickHandler = function(){
		var mid = this.dataset.mid;
		var cnt = $('#' + mid).html();
		$('#consoleComponent').html(cnt);
		showTab("component");
	}

	$(".componentUI").on('click', componentClickHandler);

	var subSubmitHandler = function(e){
    if(e.which === 13){
			var mid = this.dataset.mid;
			var subN = this.dataset.subn;
			var value = this.value.replace(/ /g,'');

			var cmd = "scr " + mid + " switchSub subN:" + subN + " to:" + value + " spd:1.0"
			window.eventHandler(cmd);
			$("button#pause").click();
		}
	}

	$(".consoleSub").on('keyup', subSubmitHandler);

	// disable key commands while typing
	$(".consoleSub").on('focus', function(){handlerHasFocus = false});
	$(".consoleSub").on('focusout', function(){handlerHasFocus = true});


	var consoleImageClickHandler = function(){
		var mid = this.dataset.mid;
		$('#consoleImages #mid').val(mid);

		var idx = this.dataset.idx;
		$('#consoleImages #idx').val(idx);

		var cnt = $("#allImages").html();
		$('#consoleImages #images').html(cnt);
		showTab("images");
	}

	$(".consoleImage").on('click', consoleImageClickHandler);

	// mad ghetto.  fuck it
	consoleImageSelectHandler = function(elt){
		var mid = $("#consoleImages #mid").val()
		var idx = $("#consoleImages #idx").val()
		var src = elt.dataset.src;

		var cmd = "scr " + mid + " switchImage idx:" + idx + " to:" + src + " spd:1.0"
		window.eventHandler(cmd);
		$("button#pause").click();
		showTab("main");
	}

}
