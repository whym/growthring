<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%
   String str = request.getParameter("q");
   if ( str == null ) { str = ""; }
   String snippet = str.length() < 24 ? str : str.substring(0, Math.min(22, str.length())) + "...";
   String queryURL = "find-repeats" + "?format=raw&q=" + java.net.URLEncoder.encode(str, "UTF-8");
 %>
<%
   String thisURL = request.getServletPath().toString() + "?" + request.getQueryString();
   thisURL = thisURL.replace("index.jsp", "");
 %>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
<head>
<%@page contentType="text/html;charset=UTF-8"%>
<meta http-equiv="content-style-type" content="text/css" />
<meta http-equiv="content-script-type" content="text/javascript" />
<meta name="description" content="repeat detection in strings" />
<meta name="keywords" content="growthring,growth-ring,multiple-sequence-analysis,maximum-repeats,suffix-arrays" />
<meta name="robots" content="INDEX,FOLLOW" />
<link rev="made" href="http://whym.org" />
<link rel="INDEX" href="." />
<title>growthring<%= str.equals("") ? "": " - " + snippet%></title>

<script type="text/javascript" src="resources/jquery.js"></script>
<script type="text/javascript" src="resources/jquery-ui.js"></script>
<script type="text/javascript">
$(function(){

function get_time() {
  return new Date().getTime();
}

function update() {
  // ask the servlet, retrieve the result, and update the placeholder of the reuslt
  var query = $('#edit').val();
  $('#busy').show();
  state.updating = true;
  $.ajax({
    url: 'find-repeats',
    type: 'GET',
    dataType: 'json',
    data: {q: query, format: 'json', n: '2,3,4,5,6,7,8'},
    success: function(json){
      // set the result
      $('#result').val(json.raw);
       // update the permalink
      var url = document.location.href.split('?')[0] + '?q=' + query;
      $('#permalink').attr('href', url);
      $('#permalinkbox').val(url);
    },
    complete: function(){
      state.updateTime = get_time();
      state.updating = false;
      $('#busy').hide();
    }
  });
}

// initialization

  var state = {
    enterTime:  get_time(),
    updateTime: get_time(),
    updating: false
  };
  $('#submit').hide();
  $('#busy').hide();
  $('#busy').css({top: '1em'});
  $('#edit').bind('keypress click', function(){
    $('#editc').text($('#edit').val().length);
    state.enterTime = get_time();
  });
  $('#edit').focus();

  if ( /q=.+/.exec(location.href) ) {
    update();
  }

// periodic execution

  window.setInterval(function(){
    if ( !state.updating && state.updateTime <= state.enterTime && get_time() - state.updateTime >= 150 ) {
      update();
    }
  }, 50);
});
</script>
<style type="text/css">
#busy { position: fixed; right: 1em; top: -100%; }
h1 { text-align: center; }
body { background-color: #EEE; color: #444; }
#body { margin: 3em auto; }
#body { max-width: 60%; }
.counter { display: inline-block; width: 6%; font-size: 200%; text-align: right; vertical-align: top; }
textarea { display: inline-block; width: 78%; margin: 0 0 0 10%; height: auto; font-size: 180%; padding: 0.3em; border: 3px solid #777; }
.box p { text-align: center; }
.mode { text-align: center; font-size: 150%; width: 100%; padding: 0.5em 0; background: url("resources/images/arrow.png") no-repeat center; }
.mode label { display: inline-block; padding: 0 1.5em 0 0.8em; }
.description { font: normal normal 90% serif; width: 100%; text-align: center; }
.permalinkline { text-align: right; margin-bottom: 2em; }
#permalinkbox { font-family: monospace; }
h1 a img {border: none;}
input[type=submit] { display: block; margin: 0 auto; font-size:130%; width: 12em; }
</style>
</head>
<body>
<h1><a href=".">growthring</a></h1>
<p class="description"><!-- subtitle --></p>

<form action="/" method="get">
<div id="body">

<div class="box">
<p>↓ここにテキストを入力してください。</p>
<textarea rows="4" cols="20" id="edit" name="q">
<%=str%></textarea>
<label class="counter" id="editc" for="edit"><%=str.length()%></label>
</div>

<div class="mode">
&nbsp;
</div>

<div class="box">
<textarea rows="4" cols="20" id="result" readonly="readonly"><jsp:include page="<%=queryURL%>" /></textarea>
</div>

<input id="submit" type="submit" />

</div>
</form>

<p class="permalinkline">
<a id="permalink" name="permalink" href="<%=thisURL%>">この結果へのリンク</a>:
<input id="permalinkbox" type="text" size="50" readonly="readonly" value="<%=thisURL%>" />
</p>

<p id="busy"><img src="resources/images/Ajax-loader.gif" alt="busy" /></p>

<address>
運営 <a href="http://twitter.com/whym">@whym</a>
</address>
</body>
</html>
