// Animates the menu on click
$(document).ready(function(){
  $(".info-img").click(function() {
    $(".info-item").css('flex-direction', 'column-reverse');
    $(".info-item").css('width', '50px');
    $(".info-item").css('height', '250px');
    $(".info-item").css('justify-content', 'space-around');
      
    $(".info-img").css('width', '50px');
    $(".info-img").css('height', '50px');
    
    $(".info-text").css('rotate', '-90deg');
    $(".info-text").css('align-self', 'center');
    $(".info-text").css('display', 'flex');
    $(".info-text").css('width', '150px');
    
    $("#info-menu").css('width', '250px');
    $("#info-menu").css('float', 'left');
    
    $("#info-content").css('width', '450px');
    $("#info-content").css('float', 'right');
  });
  
  $("#dubois-light").click(function() {
    $("#info-dubois").css('display', 'flex');
    $("#info-icaro").css('display', 'none');
    $("#info-starks").css('display', 'none');
  });
  
  $("#icaro-light").click(function() {
    $("#info-dubois").css('display', 'none');
    $("#info-icaro").css('display', 'flex');
    $("#info-starks").css('display', 'none');  });
  
  $("#starks-light").click(function() {
    $("#info-dubois").css('display', 'none');
    $("#info-icaro").css('display', 'none');
    $("#info-starks").css('display', 'flex');
  });
});