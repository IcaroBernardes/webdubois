// Connects slider input position to the width of the foreground image
$(document).ready(function(){
  // .on("input change") responds to changes on the input while dragging the mouse
  // .change() only triggers once the mouse stops being dragged
  $("#slider").on("input change", (e)=>{
    const sliderPos = e.target.value;
    // Update the width of the foreground image
    $('.foreground-img').css('width', `${sliderPos}%`);
    // Update the position of the slider button
    $('#slider-button').css('left', `calc(${sliderPos}% - 18px)`);
  });
});