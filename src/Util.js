"use strict";
function getFormattedDate() {
    var date = new Date();

    var month = date.getMonth() + 1;
    var day = date.getDate();
    var hour = date.getHours();
    var min = date.getMinutes();
    var sec = date.getSeconds();

    month = (month < 10 ? "0" : "") + month;
    day = (day < 10 ? "0" : "") + day;
    hour = (hour < 10 ? "0" : "") + hour;
    min = (min < 10 ? "0" : "") + min;
    sec = (sec < 10 ? "0" : "") + sec;

    var str = date.getFullYear() + "-" + month + "-" + day + " " +  hour + ":" + min + ":" + sec;

    return str;
}

exports.enableDebug = function () {
  window.epiDbg = true;
}

exports.log = function (x) {
  return function (){
    if(window.epiDbg){
      if(typeof(x) == 'object') // clone object for console
        x = jQuery.extend(true, {}, x);

      console.log(x);
      var timestamp = "<span class='timestamp'>[" + getFormattedDate() + "]</span>"
      $("#debugLog").append("<span class='log-line'>" + timestamp + " - " + x + "</span><br/>");
    }
  }
}

exports.flog = function(x) {
  return function(){
    console.log(x);
  }
}


exports.tLg = function tLg(x) {
  if(typeof window.lgCnt == 'undefined')
    window.lgCnt = 0;
  if(window.lgCnt < 10)
    console.log(x);
  window.lgCnt = window.lgCnt + 1;
}


exports.winLog = function (x) {
  return function() {
    $('body')[0].innerHTML = "<pre>" + x.replace(new RegExp("\n", 'g'), "<br/>") + "</pre>";
  };
}

exports.winAppend = function (x) {
  return function() {
    $('body').append("<pre>" + x.replace(new RegExp("\n", 'g'), "<br/>") + "</pre>");
  };
}



exports.unsafeNull = null

exports.unsafeSetAttr = function(obj){
  return function(attrn){
    return function(attrv){
      var obj2 = $.extend({}, obj)
      obj2[attrn] = attrv;
      return obj2;
    };
  };
};

exports.unsafeGetAttr = function(obj){
  return function(attrn){
    return obj[attrn];
  };
};

exports.hasAttr = function(obj){
  return function(attrn){
    return (obj[attrn] != undefined);
  };
};

exports.unsafeCast = function(obj){
  return obj;
};

exports.getProfileCookie = function(){
  var c = getCookie("epimorphism_profile")
  return c;
}


exports.requestAnimationFrame = function(func){
  return function(args){
    if (typeof rAF === 'undefined') {
      var rAF = (function(){
        return window.requestAnimationFrame ||
          window.webkitRequestAnimationFrame ||
          window.mozRequestAnimationFrame    ||
          function( callback ){
            window.setTimeout(callback, 1000 / 60);
          };
      })();
    }
    return function(){
      return rAF(func(args));
    };
  };
};

exports.stick = function stick(x) {
  $.stick = x;
}


exports.now = function () {
  return performance.now();
};

exports.now2 = exports.now;

exports.unsafeEval = function (s) {
  return function () {eval(s);};
}

exports.urlArgs = function() {
  var get = {};

  document.location.search.replace(/\??(?:([^=]+)=([^&]*)&?)/g, function () {
    function decode(s) {
      return decodeURIComponent(s.split("+").join(" "));
    }

    get[decode(arguments[1])] = decode(arguments[2]);
  });

  return get;
}

exports.isDev = function() {
  return document.location.hostname == "localhost";
}

exports.replaceAll = function(search) {
  return function (replacement) {
    return function (target) {
      return target.replace(new RegExp(search, 'g'), replacement);
    };
  };
};


exports.boolFromString = function(s) {
  return (s == "true");
}

exports.numFromStringImpl = function (just) {
  return function (nothing) {
    return function (s) {
      /* jshint bitwise: false */
      var i = parseFloat(s);
      return i != NaN ? just(i) : nothing;
    };
  };
};


exports.urlGetImpl = function (left) {
  return function (right) {
    return function (url) {
      return function() {
        var result;
        var request = new XMLHttpRequest();
        request.open('GET', url, false);
        request.addEventListener('load', function() {
          if (request.status == 200 || request.status == 304 ){
            result = right(request.responseText);
          }else
            result = left(url + " : " + request.statusText);
        });

        request.send();
        return result;
      };
    };
  };
};

exports.cxFromStringImpl = function (tuple) {
  return function (just) {
    return function (nothing) {
      return function (s) {
        var real, imag, regex, match, a, b, c;
        // TODO: Make this work better-er
        regex = /^([-+]?(\d+|\d*\.\d+))([-+](\d+|\d*\.\d+))[ij]$/i;
        s = String(s).replace (/\s+/g, '');

        match = s.match (regex);
        if (!match) {
          regex = /^([-+]?(\d+|\d*\.\d+))$/i;
          match = s.match (regex);
          if (!match) {
            regex = /^([-+]?(\d+|\d*\.\d+))[ij]$/i;
            match = s.match (regex);
            if (!match) {
              return nothing;
            }
            a = "0.0";
            b = match[1];
          }else{
            a = match[1];
            b = "0.0";
          }
        }else{
          a = match[1];
          b = match[3];
        }

        real = parseFloat(a);
        imag = parseFloat(b);
        var tmp = tuple(real);
        return just(tmp(imag));
      };
    };
  };
};

exports.halt = function() {
  $("#halt").hide();
  console.log("HALT");
  halt = true;
};


exports.isHalted = function() {
  return halt;
};


exports.uuid = function() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(16)
      .substring(1);
  }
  return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
    s4() + '-' + s4() + s4() + s4();
};



exports.rndstr = function() {
  var text = "";
  var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

  for( var i=0; i < 10; i++ )
    text += possible.charAt(Math.floor(Math.random() * possible.length));

  return text;
};


exports.gmod = function(n) {
  return function(m) {
    return ((n%m)+m)%m;
  };
};


exports.seedRandom = function(seed) {
  return function(){
    return Math.seedrandom(seed);
  };
};


exports.random = function() {
  return Math.random();
};


exports.randInt = function(n) {
  return function() {
    return Math.floor(n * Math.random());
  };
};


exports.isNumber = function(s) {
  return !isNaN(s);
}

exports.clickPause = function() {
  $("button#pause").click();
};


exports.offsetOf = function(tok) {
  return function(str){
    return function(){
      var lines = str.split("\n")
      for(var i = 0; i<lines.length; i++){
        var line = lines[i];
        if(line.includes(tok))
          return line.indexOf(tok);
      }
      return 0;
    }
  }
}
