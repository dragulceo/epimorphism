"use strict";

exports.addEventListeners = function() {
  // SWITCH CHILD
  var switchChildHandler = function(){
    var mid = this.dataset.mid;
    var cname = this.dataset.cname;
    var mn = this.value;

    var cmd = "scr " + mid + " switch childN:" + cname + " spd:1.0 op:load by:val " + "val:" + mn;
    window.eventHandler("pauseAfterSwitch");
    window.eventHandler(cmd);
    $("button#pause").click();
  }

  $('.switchChild').off('change');
  $('.switchChild').on('change', switchChildHandler);

  var componentClickHandler = function(){
    var mid = this.dataset.mid;
    var cnt = $('#' + mid).html();
    $('#consoleComponent').html(cnt);
    showTab("component");
  }

  $(".componentUI").off('click');
  $(".componentUI").on('click', componentClickHandler);

  var subSubmitHandler = function(e){
    if(e.which === 13){
      var mid = this.dataset.mid;
      var subN = this.dataset.subn;
      var value = this.value.replace(/ /g,'');
      var cmd = "scr " + mid + " switch mut:sub idx:" + subN + " spd:1.0 op:clone by:val val:" + value

      window.eventHandler("pauseAfterSwitch");
      window.eventHandler(cmd);
      $("button#pause").click();
    }
  }

  $(".consoleSub").off('keyup');
  $(".consoleSub").on('keyup', subSubmitHandler);

  // disable key commands while typing
  $(".consoleSub").off('focus');
  $(".consoleSub").off('focusout');
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

  $(".consoleImage").off('click');
  $(".consoleImage").on('click', consoleImageClickHandler);

  // mad ghetto.  fuck it
  consoleImageSelectHandler = function(elt){
    var mid = $("#consoleImages #mid").val()
    var idx = $("#consoleImages #idx").val()
    var src = elt.dataset.src;

    var cmd = "scr " + mid + " switch mut:image idx:" + idx + " spd:1.0 op:clone by:val val:" + src;

    window.eventHandler("pauseAfterSwitch");
    window.eventHandler(cmd);
    $("button#pause").click();
    showTab("main");
  }

  var consoleVarClickHandler = function(){
    var mid = this.dataset.mid;
    $('#consoleVar #mid').val(mid);

    var v = this.dataset.var;
    $('#consoleVar #var').val(v);
    $('#consoleVar #label').html(v);

    var val = this.dataset.val;
    $('#consoleVar #val').val(val);

    var type = this.dataset.type;
    $('#consoleVar #type').val(type);

    showTab("var");
  }

  $(".consoleVar").off('click');
  $(".consoleVar").on('click', consoleVarClickHandler);


  var varSubmitHandler = function(e){
    if(e.which === 13){
      e.preventDefault();

      var mid = $('#consoleVar #mid').val();
      var v = $('#consoleVar #var').val();
      var val = $('#consoleVar #val').val();
      var type = $('#consoleVar #type').val();

      var cmd;
      if (type == "par") {
        var val1 = val.trim().replace(/ /g,'___');
        cmd = "setP " + mid + " " + v + " " + val1;
      }else{
        var val1 = val.trim().replace(/ /g,'___');
        cmd = "setZn " + mid + " " + v + " " + val1;
      }
      window.eventHandler(cmd);

      $("button#pause").click();
      showTab("main");
    }
  }

  $("#consoleVar input").off('keypress');
  $("#consoleVar input").off('focus');
  $("#consoleVar input").off('focusout');
  $("#consoleVar input").on('keypress', varSubmitHandler);
  $("#consoleVar input").on('focus', function(){handlerHasFocus = false});
  $("#consoleVar input").on('focusout', function(){handlerHasFocus = true});

}
