// Populates the menu with items
$(document).ready(function(){
  // Creates the HTML structure for the thumbnails of the gallery menu
  let galleryHTML = imageids.map((el) => {
    return "<div class='item'>" +
            "<img src='www/new/thumb_" + el + ".png' class='thumb-new' data='" + el + "'>" +
            "<img src='www/dubois/thumb_" + el + ".png' class='thumb-original' data='" + el + "'>" +
          "</div>";
  });
  
  // Applies the string to the gallery as a HTML structure
  $("#gallery").html(galleryHTML)  
});