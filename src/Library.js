"use strict";

// module Library

exports.unsafeGenericObjectImpl = function(schema){
	return function(set){
		var obj = {};
		$.tmp = schema;

		for(var i=0; i < schema.length; i++){
			var elt = schema[i];
			var typ = elt.value0.constructor.name;
			var name = elt.value1;

			switch(typ){
			case "SE_St":
				obj[name] = "";
				break;
			case "SE_N":
				obj[name] = 0.0;
				break;
			case "SE_I":
				obj[name] = 0;
				break;
			case "SE_B":
				obj[name] = false;
				break;
			case "SE_S":
				obj[name] = set;
				break;
			case "SE_A_St":
				obj[name] = [];
				break;
			case "SE_A_Cx":
				obj[name] = [];
				break;
			case "SE_M_N":
				obj[name] = {};
				break;
			case "SE_M_St":
				obj[name] = {};
				break;
			default:
				console.log(typ);
				console.log(name);
				//console.log("failure to parse generic - " + elt);
			}
		}
		return obj;
	};
};
