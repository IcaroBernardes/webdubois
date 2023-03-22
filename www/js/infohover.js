// Binds hover effects to the info images
$(document).ready(function(){
  $(".info-img").hover(
    function() {
      let path = this.id;
      path = path.replace("light", "dark");
      path = 'www/info/' + path + '.png';
      $( this ).css("background-image", `url(${path})`);
    }, function() {
      let path = this.id;
      path = 'www/info/' + path + '.png';
      $( this ).css("background-image", `url(${path})`);
    }
  );
});