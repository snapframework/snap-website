$(document).ready(function(){ 
  $('.slideshow').cycle({
    fx: 'fade',
    timeout: 7000,
    pager: "#pager"
  });  
  $('a').click(function(){ $(this).blur(); });

  var current = $.url.segment(0);
  console.log(current);
  if (current) {
    $('.nav li.'+current).addClass('active');
  }else{
    $('.nav .home').addClass('active');
  }

});
