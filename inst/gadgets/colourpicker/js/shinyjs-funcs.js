shinyjs.init = function() {
  $("#selected-cols-row").on("click", ".col", function(event) {
    var colnum = $(event.target).data("colnum");
    Shiny.onInputChange("jsColNum", [colnum, Math.random()]);
  });

  $("#rclosecolsSection, #allColsSection").on("click", ".rcol", function(event) {
    var col = $(event.target).data("col");
    Shiny.onInputChange("jsCol", [col, Math.random()]);
  });

  $(document).on("shiny:recalculated", function(event) {
    if (event.target == $("#allColsSection")[0]) {
      $("#allcols-spinner").hide();
    }
  });

  $(document).keydown(function(event) {
    // Ignore key presses inside input fields
    if ($(event.target).closest(".shiny-input-container").length > 0) {
      return;
    }
    if (event.which == 13) {
      $("#done").click();
    }

    if (event.which == 37) {
      Shiny.onInputChange("jsColNav", [-1, Math.random()]);
    }
    if (event.which == 39) {
      Shiny.onInputChange("jsColNav", [1, Math.random()]);
    }
  });
};
