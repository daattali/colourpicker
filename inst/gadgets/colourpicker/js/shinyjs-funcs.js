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

  $(document).mousedown(function(event) {
    // Close the keyboard shortcuts modal
    if ($("body").hasClass('modal-open')) {
      Shiny.onInputChange("hideShortcuts", Math.random());
      return;
    }
  });


  $(document).keydown(function(event) {
    // Close the keyboard shortcuts modal
    if ($("body").hasClass('modal-open')) {
      Shiny.onInputChange("hideShortcuts", Math.random());
      return;
    }

    // Ignore key presses inside input fields
    if ($(event.target).closest(".shiny-input-container").length > 0) {
      return;
    }

    // Escape is already taken care of by shiny

    // Enter
    if (event.which == 13) {
      $("#done").click();
    }

    // Left/right arrows
    if (event.which == 37) {
      Shiny.onInputChange("jsColNav", [-1, Math.random()]);
    }
    if (event.which == 39) {
      Shiny.onInputChange("jsColNav", [1, Math.random()]);
    }

    // Spacebar
    if (event.which == 32) {
      $("#addColBtn").click();
    }

    // Delete
    if (event.which == 46) {
      $("#removeColBtn").click();
    }

    // Number keys
    if (event.which >= 49 && event.which <= 57) {
      Shiny.onInputChange("jsColNum", [event.which-48, Math.random()]);
    }
  });
};

shinyjs.closeWindow = function() {
  window.close();
}
