HTMLWidgets.widget({

  name: 'colourWidget',

  type: 'output',

  factory: function(el, width, height) {

    var elementId = el.id;
    var container = $("#" + el.id);
    var initialized = false;

    return {

      renderValue: function(opts) {
        if (!initialized) {
          initialized = true;
          container.colourpicker({changeDelay : 0});
        }

        container.colourpicker("settings", opts);
        container.colourpicker("value", opts.value);
      },

      resize: function(width, height) {
      }

    };
  }
});
