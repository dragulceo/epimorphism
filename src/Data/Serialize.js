"use strict";

function defaultVal(elt, set){
	var typ = elt.value0.constructor.name || functionName(elt.value0.constructor);
	switch(typ){
	case "SE_St":
		return "";
	case "SE_N":
		return 0.0;
	case "SE_I":
		return 0;
	case "SE_B":
		return false;
	case "SE_S":
		return set;
	case "SE_A_St":
		return [];
	case "SE_A_Cx":
		return [];
	case "SE_M_N":
		return {};
	case "SE_M_St":
		return {};
	default:
		console.log("unknown generic typ - " + type);
	}
}

exports.unsafeGenericIndexableImpl = function(idx_schema){
	return function(schema){
		return function (constr) {
			return function(set) {
				var idx = {};
				var obj = {};
				$.tmp = idx_schema;
				$.tmp1 = schema;

				for(var i=0; i < idx_schema.length; i++){
					var elt = idx_schema[i];
					var name = elt.value1;
					idx[name] = defaultVal(elt, set);

					//console.log(elt.value0);
					//console.log(name);
				}

				for(var i=0; i < schema.length; i++){
					var elt = schema[i];
					var name = elt.value1;
					obj[name] = defaultVal(elt, set);

					//console.log(elt.value0);
					//console.log(name);
				}
				return constr(idx)(obj);
			};
		};
	};
};

//ghetto bs for ie
function functionName(fun) {
  var ret = fun.toString();
  ret = ret.substr('function '.length);
  ret = ret.substr(0, ret.indexOf('('));
  return ret;
}


exports.unsafeSetIndexableAttr = function(obj){
	return function(field){
		return function(attrn){
			return function(attrv){
				return function (){
					var obj2 = $.extend(true, {}, obj)
					obj2[field][attrn] = attrv;
					return obj2;
				};
			};
		};
	};
};
