var __, __M, __D;
var halt = false;

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


var parseT = function(expr){
	var node = math.parse(expr); // check syntax errors
	$.n = node;
	console.log(node);
	return parseN(node);
}

var parseN = function(node){
		console.log(node.type);
	switch (node.type) {
  case 'ConstantNode':
		return "vec2(" + node.value + ",0.0)";
		break;
	case 'SymbolNode':
		return node.name == 'i' ? "vec2(0.0,1.0)" : node.name;
		break;
	case 'FunctionNode':
		args = $.map(node.args, function(n, i) {return parseN(n)});
		return node.fn.name + '(' + args.join(',') + ')'
		break;
	case 'OperatorNode':
		args = $.map(node.args, function(n, i) {return parseN(n)});
		return node.op + '(' + args.join(',') + ')'
		break;
	case 'ParenthesisNode':
		return parseN(node.content);
	}
}
