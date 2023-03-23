// Binds hover effects to "more"" button
$(document).ready(function(){
  $("#more svg").hover(
    function() {
      $( this ).find("circle").css("stroke", "hsl(38deg 26% 18%)");
    }, function() {
      $( this ).find("circle").css("stroke", "transparent");
    }
  );
});