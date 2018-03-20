// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("LAT");
  var long = $el.data("LONG");
  
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: LAT,
    lng: LONG,
    nonce: Math.random()
  });
});