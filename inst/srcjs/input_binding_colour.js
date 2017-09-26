var colourBinding = new Shiny.InputBinding();
$.extend(colourBinding, {
  find: function(scope) {
    // Check if colour plugin is loaded
    if (!$.fn.colourpicker)
      return [];
    return $(scope).find('input.shiny-colour-input');
  },
  getValue: function(el) {
    return $(el).colourpicker('value');
  },
  setValue: function(el, value) {
    $(el).colourpicker('value', value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.colourBinding", function(e) {
      callback(true);
    });
  },
  unsubscribe: function(el) {
    $(el).off(".colourBinding");
  },
  initialize : function(el) {
    var $el = this._jqid(el.id);  // for some reason using $(el) doesn't work

    var opts = {
      changeDelay      : 0,
      showColour       : $el.attr('data-show-colour'),
      palette          : $el.attr('data-palette'),
      allowedCols      : $el.attr('data-allowed-cols'),
      allowAlpha       : $el.attr('data-allow-alpha'),
      returnName       : $el.attr('data-return-name')
    };

    if (typeof opts.allowedCols !== 'undefined') {
      opts.allowedCols = JSON.parse(opts.allowedCols);
    }

    // initialize the colour picker
    $el.colourpicker(opts);

    this.setValue(el, $el.attr('data-init-value'));

    // save the initial settings so that we can restore them later
    $el.data('init-opts', $.extend(true, {}, $el.colourpicker('settings')));
  },
  // update the colour input
  receiveMessage: function(el, data) {
    var $el = $(el);

    if (data.hasOwnProperty('palette')) {
      $el.colourpicker('settings', { 'palette' : data.palette });
    }
    if (data.hasOwnProperty('allowedCols')) {
      $el.colourpicker('settings', { 'allowedCols' : data.allowedCols.join(" ") });
    }
    if (data.hasOwnProperty('allowAlpha')) {
      $el.colourpicker('settings', { 'allowAlpha' : data.allowAlpha });
    }
    if (data.hasOwnProperty('returnName')) {
      $el.colourpicker('settings', { 'returnName' : data.returnName });
    }
    if (data.hasOwnProperty('value')) {
      this.setValue(el, data.value);
    }
    if (data.hasOwnProperty('label')) {
      this._getContainer(el).find('label[for="' + el.id + '"]').text(data.label);
    }
    if (data.hasOwnProperty('showColour')) {
      $el.colourpicker('settings', { 'showColour' : data.showColour });
    }

    $el.trigger("change");
  },
  getRatePolicy : function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  },
  // Get the shiny input container
  _getContainer : function(el) {
    return $(el).closest(".shiny-input-container");
  },
  _jqid : function(id) {
    return $("#" + id.replace( /(:|\.|\[|\]|,)/g, "\\$1" ));
  }
});

Shiny.inputBindings.register(colourBinding);
