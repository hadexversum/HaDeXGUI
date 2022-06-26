$(document).ready(function() {
  $(".card-header").on("click", function() {
    var id = $(this).attr("data-target");
    if ($(id).attr("aria-expanded") === "true") {
      $(this).find("i").removeClass("fa-angle-up").addClass("fa-angle-down");
    } else {
      $(this).find("i").removeClass("fa-angle-down").addClass("fa-angle-up");
    }
  });
});
