// Animates the information menu on click
$(document).ready(function(){
  $(".info-img").click(function() {
    // Diminishes and moves the menu items
    $(".info-item").css('flex-direction', 'column-reverse');
    $(".info-item").css('width', '50px');
    $(".info-item").css('height', '250px');
    $(".info-item").css('justify-content', 'space-around');
    
    // Diminishes the menu image
    $(".info-img").css('width', '50px');
    $(".info-img").css('height', '50px');
    
    // Rotates and places the menu text
    $(".info-text").css('rotate', '-90deg');
    $(".info-text").css('align-self', 'center');
    $(".info-text").css('display', 'flex');
    $(".info-text").css('width', '150px');
    
    // Rearranges menu and content
    $("#info-menu").css('width', '250px');
    $("#info-menu").css('float', 'left');
    $("#info-content").css('width', '450px');
    $("#info-content").css('float', 'right');
  });
  
  // Defines which content to display
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