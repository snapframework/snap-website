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

var _gaq = _gaq || [];
_gaq.push(['_setAccount', 'UA-15984160-1']);
_gaq.push(['_trackPageview']);

(function() {
  var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
  ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
})();

