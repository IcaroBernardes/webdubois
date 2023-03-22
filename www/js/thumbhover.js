// Binds hover effects to the pairs of thumbnails
$(document).ready(function(){
  $(".thumb-new, .thumb-original").hover(
    function() {
      $( this ).css("border-color", "#d18520");
      $( this ).siblings().css("border-color", "#d18520");
      $( this ).css("background-color", "#d18520");
      $( this ).siblings().css("background-color", "#d18520");
    }, function() {
      $( this ).css("border-color", "transparent");
      $( this ).siblings().css("border-color", "transparent");
      $( this ).css("background-color", "transparent");
      $( this ).siblings().css("background-color", "transparent");
    }
  );
  $(".thumb-original").hover(
    function() {
      $( this ).css("width", "70px");
    }, function() {
      $( this ).css("width", "50px");
    }
  );
  $(".thumb-new").hover(
    function() {
      $( this ).siblings().css("width", "70px");
    }, function() {
      $( this ).siblings().css("width", "50px");
    }
  );
});