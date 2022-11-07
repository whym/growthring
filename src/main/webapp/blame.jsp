<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<%
   String str = request.getParameter("title");
   if ( str == null ) { str = ""; }

   String snippet = str.length() < 24 ? str : str.substring(0, Math.min(22, str.length())) + "...";

   String threshold = request.getParameter("n");
   if ( threshold == null ) { threshold = "140"; }

   String base = request.getParameter("base");
   if ( base == null ) { base = "https://en.wikipedia.org/w"; }

   String method = request.getParameter("reqm");
   if ( method == null ) { method = "POST"; }

   String queryURL = "wikiblame" + "?n=" + threshold + "&title=" + java.net.URLEncoder.encode(str, "UTF-8") + "&base=" +  java.net.URLEncoder.encode(base, "UTF-8");
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
<title>growthring<%= request.toString() %></title>
<script type="text/javascript" src="resources/jquery.js"></script>
<script type="text/javascript">
$(function(){

function get_time() {
  return new Date().getTime();
}

function update() {
  // ask the servlet, retrieve the result, and update the placeholder of the reuslt
  var query = $('#edit').val();
  var before_query = get_time();
  var failed = undefined;
  $('#busy').show();
  state.updating = true;
  $('#resultmessage').text('...');
  $.ajax({
    url: 'wikiblame',
    type: '<%=method%>',
    dataType: 'json',
    data: {
      title: query,
      n: '<%=threshold%>'
    },
    success: function(json){
      // set the result
      $('#result').val(json.plain);
      $('#resulthtml').html(json.html);
       // update the permalink
      var url = document.location.href.split('?')[0] + '?q=' + query;
      $('#resultmessage').text((get_time() - before_query) + " msecs" + "\n").append(json.layers_html);
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

  if ( /title=.+/.exec(location.href) ) {
    update();
  }
});
</script>
<style type="text/css">
#busy { position: fixed; right: 1em; top: -100%; }
h1 { text-align: center; }
body { background-color: #EEE; color: #444; }
#body { margin: 3em auto; }
#body { max-width: 60%; }
textarea { display: inline-block; width: 78%; margin: 0 0 0 10%; height: auto; font-size: 180%; padding: 0.3em; border: 3px solid #777; }
.box p { text-align: center; }
.mode { text-align: center; font-size: 150%; width: 100%; padding: 0.5em 0; background: url("resources/images/arrow.png") no-repeat center; }
.mode label { display: inline-block; padding: 0 1.5em 0 0.8em; }
.description { font: normal normal 90% serif; width: 100%; text-align: center; }
h1 a img {border: none;}
input[type=submit] { display: block; margin: 0 auto; font-size:130%; width: 12em; }

#resulthtml { color: #000; font-size: 130%; margin: 0 0 0 10%; }
#resulthtml del { text-decoration: none; color: #CCC; }
</style>
</head>
<body>
<h1><a href=".">growthring</a></h1>
<p class="description"><!-- subtitle --></p>

<form action="/" method="get">
<div id="body">

<div class="box">
<p>↓ここにページ名を入力してください。</p>
<input type="text" size="20" id="edit" name="q" value="<%=str%>">
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

<p id="busy"></p>

<address>
運営 <a href="http://twitter.com/whym">@whym</a>
</address>
</body>
</html>
