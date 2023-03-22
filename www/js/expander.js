// Establishes triggers to show and hide info with a slide animation.
// Also changes the svg text that is visible inside the button
$(document).ready(function(){
  $("#plus").click(function() {
    $("#info").slideDown("slow");
    $("#plus").fadeToggle({duration: 200, queue: false});
    $("#minus").fadeToggle({duration: 200, queue: false});
  });
  $("#minus").click(function() {
    $("#info").slideUp("slow");
    $("#minus").fadeToggle({duration: 200, queue: false});
    $("#plus").fadeToggle({duration: 200, queue: false});
  });
});