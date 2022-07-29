$(document).ready(function(){
  $(".dropdown").on("click", function(e){
    $(this).toggleClass("open");

    e.stopPropagation();
    e.preventDefault();
  });

  $(".dropdown [data-toggle=tab]").on("click", function(e){
    let dv = ($(this).attr("data-value"));

    //Set active element in tabcontents
    $(".tab-pane")
      .not(".tab-pane .tab-pane")
      .removeClass("active");
    $(".tab-pane[data-value='" + dv + "']")
      .addClass("active");

    // Set this element as the only one active in navbar
    $("a[data-toggle=tab]")
      .not(".tab-pane a[data-toggle=tab]")
      .parent()
      .removeClass("active");

    // Remove aria attributes from all togglers
    $("a[data-toggle=tab]")
      .attr({
        "aria-expanded": false,
        "aria-selected": false
      });

    // Make this specific navbar element active
    $("a[data-value='" + dv + "']")
      .parent()
      .addClass("active");

    // Add aria attributes to this one toggler
    $("a[data-value='" + dv + "']")
      .attr({
        "aria-expanded": true,
        "aria-selected": true
      })
      .trigger("change");


    // Add aria attribute to all active tab buttons within the tab currently selected
    $(".tab-pane[data-value='" + dv + "'] .nav li.active > a")
      .attr({
        "aria-expanded": true,
        "aria-selected": true
      });

    $(".tab-pane[data-value='" + dv + "'] .tab-pane.active .shiny-bound-output")
      .each(function () {
        const $this = $(this),
        binding = $this.data("shinyOutputBinding");

        $this.trigger({
          type: "shiny:visualchange",
          visible: true,
          binding: binding,
        });
      });

    //Close the dropdowns
    $(".dropdown").removeClass("open");

    //e.stopPropagation();
    //e.preventDefault();
  });
});
