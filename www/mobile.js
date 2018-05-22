var isMobileBinding = new Shiny.InputBinding();
$.extend(isMobileBinding, {
  find: function(scope) {
    return $(scope).find(".mobile-element");
  },
  getValue: function(el) {
    alert(navigator.userAgent);
    return /((iPhone)|(iPod)|(iPad)|(Android)|(BlackBerry))/.test(navigator.userAgent);
  },
  setValue: function(el, value) {
  },
  subscribe: function(el, callback) {
  },
  unsubscribe: function(el) {
  }
});

Shiny.inputBindings.register(isMobileBinding);
