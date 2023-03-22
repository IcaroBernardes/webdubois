// Connects clicking on the gallery menu to which
// images are shown in the comparative display and
// updates the data shown in the details section
$(document).ready(function(){
  // Initiates an object to hold url of the original images
  let origimages = {};
  
  // Fills the object with the urls by iterating through 'imageids'
  imageids.map((el, index) => {
    return origimages[imageids[index]] = 'www/dubois/' + imageids[index] + '.png';
  });
  
  // Initiates an object to hold url of the new images
  let newimages = {};
  
  // Fills the object with the urls by iterating through 'imageids'
  imageids.map((el, index) => {
    return newimages[imageids[index]] = 'www/new/' + imageids[index] + '.png';
  });
  
  // Gets the 'data' attribute of the clicked object and uses it to search
  // through the arrays and defines the new urls of the background images
  $('#menu-section').click(function(e) {
    const idimage = $(e.target).attr('data');
    
    // Guarantees that the change only happens if an object that has an id is clicked
    if (idimage.length != 0) {
      // Changes the background images in the comparative display
      $('.background-img').css('background-image', `url(${newimages[idimage]})`);
      $('.foreground-img').css('background-image', `url(${origimages[idimage]})`);
      
      // Updates the description items
      $('#original-title span').html(`${detailsDATA[idimage][0]["original-title"]}`);
      $('#new-title span').html(`${detailsDATA[idimage][0]["new-title"]}`);
      
      // Updates the list of packages
      let pkg = detailsDATA[idimage][0]["packages"].map((el) => {
        return "<button class='btn'>" + el + "</div>";
      });
      $('#packages div').html(pkg);
      
      // Updates the download links
      $('#down-data').attr("href", `${detailsDATA[idimage][0]["downloads"][0]}`);
      $('#down-original').attr("href", `${detailsDATA[idimage][0]["downloads"][1]}`);
      $('#down-new').attr("href", `${detailsDATA[idimage][0]["downloads"][2]}`);
      
      // Updates the downloads names
      let down1 = "original_" + idimage + ".png";
      $('#down-original').attr("download", down1);
      let down2 = "new_" + idimage + ".png";
      $('#down-new').attr("download", down2);
    }
  });
});