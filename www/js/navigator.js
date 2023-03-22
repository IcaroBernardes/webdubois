// Binds clicking on the buttons to scrolling behaviour inside the gallery menu
// Customizes their animation to indicate limits of the navigation
$(document).ready(function(){
  // Defines the scroll amount in px
  let scrl = 360;
  
  // Scrolls items to the top
  $(".menu-button.navdown").mousedown(function() {
    $(this).siblings("#gallery")[0].scrollBy({
      top: scrl,
      left: 0,
      behavior: "smooth",
    });
  });
  
  // Scrolls items to the bottom
  $(".menu-button.navup").mousedown(function() {
    $(this).siblings("#gallery")[0].scrollBy({
      top: -scrl,
      left: 0,
      behavior: "smooth",
    });
  });
  
  // Changes the color and disables each button when hitting the menu limits
  $("#gallery").on("scroll", function() {
    // Reads properties of the menu
    let galHEIGHT = document.querySelector("#gallery")['clientHeight'];
    let scrlHEIGHT = document.querySelector("#gallery")['scrollHeight'];
    let scrlPOS = document.querySelector("#gallery")['scrollTop'];
    
    // Handles changes on the up button
    if (scrlPOS == 0) {
      $(this).siblings(".navup").addClass("navdisable");
    } else {
      $(this).siblings(".navup").removeClass("navdisable");
    }
    
    // Handles changes on the down button
    if (scrlHEIGHT - galHEIGHT < scrlPOS+1) {
      $(this).siblings(".navdown").addClass("navdisable");
    } else {
      $(this).siblings(".navdown").removeClass("navdisable");
    }
  });
});