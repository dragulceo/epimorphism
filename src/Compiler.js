"use strict";

// module Compiler


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
		return parseN(node.object) + node.index
		break;
	case 'ParenthesisNode':
		return parseN(node.content);
	}
}

exports.parseT = function(expr){
	var node = math.parse(expr); // check syntax errors
	return parseN(node);
}
