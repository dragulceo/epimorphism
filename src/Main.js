"use strict";

// module Main

exports.getURL = function(url) {
  var result;
  var request = new XMLHttpRequest();
  request.open('GET', url, false);
  request.addEventListener('load', function() {
    result = request.responseText;
  });
  request.send();
  return result;
}
