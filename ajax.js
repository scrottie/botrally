
function do_request(url, callback) {
  if( ! callback ) throw "no callback";
  var req;
  if (window.XMLHttpRequest) {
    req = new XMLHttpRequest();
  } else if (window.ActiveXObject) {
    req = new ActiveXObject("Microsoft.XMLHTTP");
  }
  if(req != undefined) {
    req.onreadystatechange = function() {
      if (req.readyState == 4) { // only if req is "loaded"
        if (req.status == 200) { // only if "OK"
          callback(req.responseText);
        } else {
          alert("AJAX Error:\r\n" + req.statusText + "... try again");
        }
      }
    }
    req.open("GET", url, true);
    req.send("");
  }
}

//function shownum(num) {
//  alert("Got callback, now displaying number...");
//  counter = document.getElementById('counter');
//  counter.innerHTML = num;
//  //do_request('ajax.pl?num=' + num, shownum);
//  alert("Re-initializing HTTP request");
//  do_request('http:thelackthereof.org/dev/pm-2006.01.25/ajax.pl?num='+num, shownum);
//}

//function go() {
//  do_request('http://thelackthereof.org/dev/pm-2006.01.25/ajax.pl?num=0', shownum);
//}


//      <body>
//        <h1>AJAX DEMO
//        <div id="counter">0
//        <a href="#" onclick="go()">GO!
//      </body>



