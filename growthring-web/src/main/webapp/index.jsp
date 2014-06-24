<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%
   String str = request.getParameter("q");
   if ( str == null ) { str = ""; }

   String snippet = str.length() < 24 ? str : str.substring(0, Math.min(22, str.length())) + "...";

   String threshold = request.getParameter("n");
   if ( threshold == null ) { threshold = "2"; }

   String min_len = request.getParameter("min");
   if ( min_len == null ) { min_len = "2"; }

   String method = request.getParameter("reqm");
   if ( method == null ) { method = "POST"; }

   String queryURL = "find-repeats" + "?format=plain&n=" + threshold + "&q=" + java.net.URLEncoder.encode(str, "UTF-8");
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
<!-- q=<%=str%> threshold=<%=threshold%> min=<%=min_len%> method=<%=method%> -->
<script type="text/javascript" src="resources/jquery.js"></script>
<script type="text/javascript">
$(function(){

String.prototype.hashCode = function(){
    var hash = 0, i, char;
    if (this.length == 0) return hash;
    for (i = 0; i < this.length; i++) {
        char = this.charCodeAt(i);
        hash = ((hash<<5)-hash)+char;
        hash = hash & hash; // Convert to 32bit integer
    }
    return hash;
};

function get_time() {
  return new Date().getTime();
}

function make_table(str, trs) {
  return '<table><thead>' + str.split('').map(function(x){ return '<th>'+x+'</th>';}).join('') + '</thead><tbody>' + trs + '</tbody></table>';
}

function update() {
  // ask the servlet, retrieve the result, and update the placeholder of the reuslt
  var query = $('#edit').val();
  var before_query = get_time();
  var failed = undefined;
  if ( query.hashCode() == state.lastQueryHash ) {
    return;
  }
  $('#busy').show();
  state.updating = true;
  $('#resultmessage').text('...');
  $.ajax({
    url: 'find-repeats',
    type: '<%=method%>',
    dataType: 'json',
    data: {
      q: query,
      format: 'json',
      min: '<%=min_len%>',
      n: '<%=threshold%>'
    },
    success: function(json){
      // set the result
      $('#result').val(json.plain);
      $('#resulthtml').html(json.html);
       // update the permalink
      var url = document.location.href.split('?')[0] + '?q=' + query;
      $('#permalink').attr('href', url);
      $('#permalinkbox').val(url);
      $('#resultmessage').text((get_time() - before_query) + " msecs" + "\n").append(make_table(query, json.freqs_html));
      state.lastQueryHash = query.hashCode();
    },
    error: function(jqXHR, textStatus, errorThrown) {
      failed = 'Failed: ' + jqXHR + ': ' + textStatus + ': ' + errorThrown;
    },
    complete: function(){
      state.updateTime = get_time();
      state.updating = false;
      $('#busy').hide();
      if ( failed != undefined ) {
        $('#resultmessage').text(failed);
      }
    }
  });
}

// initialization

  var state = {
    enterTime:  get_time(),
    updateTime: get_time(),
    updating: false,
    lastQueryHash: 0
  };
  $('#submit').hide();
  $('#busy').hide();
  $('#busy').css({top: '1em'}).append(
    $('<img/>').attr({
       'src': 'resources/images/Ajax-loader.gif',
       'alt': 'busy'}
    )
  );
  $('#edit').bind('keyup click', function(){
    state.enterTime = get_time();
    $('#editcount').text($('#edit').val().length);
  }).focus();

  if ( /q=.+/.exec(location.href) ) {
    update();
  }

  // periodic execution

  window.setInterval(function(){
    if ( !state.updating && state.updateTime <= state.enterTime && get_time() - state.enterTime > 300 ) {
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

#resulthtml { color: #000; font-size: 130%; margin: 0 0 0 10%; }
#resulthtml del { text-decoration: none; color: #CCC; }

.cell {display: inline-block}
.c0{background-color:rgb(255,255,204)} .c1{background-color:rgb(255,237,160)} .c2{background-color:rgb(254,217,118)} .c3{background-color:rgb(254,178,76)} .c4{background-color:rgb(253,141,60)} .c5{background-color:rgb(252,78,42)} .c6{background-color:rgb(227,26,28)} .c7{background-color:rgb(189,0,38)} .c8{background-color:rgb(128,0,38)}

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
<label class="counter" id="editcount" for="edit"><%=str.length()%></label>
</div>

<div class="mode">
&nbsp;
</div>

<div class="box">
<textarea rows="4" cols="20" id="result" readonly="readonly"><jsp:include page="<%=queryURL%>" /></textarea>
<label id="resultmessage" for="edit"></label>
<div id="resulthtml"></div>
</div>

<input id="submit" type="submit" />

</div>
</form>

<p class="permalinkline">
<a id="permalink" name="permalink" href="<%=thisURL%>">この結果へのリンク</a>:
<input id="permalinkbox" type="text" size="50" readonly="readonly" value="<%=thisURL%>" />
</p>

<p id="busy"></p>

<address>
<a href="http://whym.org">whym.org</a>
</address>
</body>
</html>
