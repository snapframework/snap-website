$(document).ready(function(){ 
  $('.slideshow').cycle({
    fx: 'fade',
    timeout: 7000,
    pager: "#pager"
  });  
  $('a').click(function(){ $(this).blur(); });

  if ($.url.segment(0)) {
    $('.nav li.'+$.url.segment(0)).addClass('active');
  } else {
    $('.nav .home').addClass('active');
  }
});


var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));

try {
    var pageTracker = _gat._getTracker("UA-15984160-1");
    pageTracker._trackPageview();
} catch(err) {}
