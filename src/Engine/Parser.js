"use strict";

var parseN = function(node){
	switch (node.type) {
  case 'ConstantNode':
		return "vec2(" + node.value + ",0.0)";
		break;
	case 'SymbolNode':
		return node.name == 'i' ? "vec2(0.0,1.0)" : node.name;
		break;
	case 'FunctionNode':
		var args = $.map(node.args, function(n, i) {return parseN(n)});
		return node.fn.name + '(' + args.join(',') + ')'
		break;
	case 'OperatorNode':
		var args = $.map(node.args, function(n, i) {return parseN(n)});
		return node.op + '(' + args.join(',') + ')'
		break;
	case 'AccessorNode':
		var struct = parseN(node.object);
		var index = node.index;
		if (struct == 'zn' || struct == 'par' || struct == 'aux'){
			index = "[#" + node.index.dimensions[0].value + "]";
		}
		return struct + index;
		break;
	case 'ParenthesisNode':
		return parseN(node.content);
	}
}

exports.parseT = function(expr){
	var node = math.parse(expr); // check syntax errors
	return parseN(node);
}
