"use strict";

// module Console

exports.addEventListeners = function() {
	// SWITCH CHILD
	var switchChildHandler = function(){
		console.log(this);
		var mid = this.dataset.mid;
		var cname = this.dataset.cname;
		var mn = this.value;

		var str = "scr " + mid + " switchChild childN:" + cname + " to:" + mn + " spd:1.0"
		console.log(str);
		window.eventHandler(str)
		$("button#pause").click();
	}

	$('.switchChild').on('change', switchChildHandler);
}
