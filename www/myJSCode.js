
shinyjs.init = function() {
  //console.info("init");
  var cookie = Cookies.get("userLocation");
  if (typeof cookie !== "undefined") {
    //console.info("found cookie");
    Shiny.onInputChange("jscookie", cookie);
  } else {
    cookie = "";
  Shiny.onInputChange("jscookie", cookie);
  }
};

shinyjs.getcookie = function(params) {
  //console.info("getcookie");
  var cookie = Cookies.get("userLocation");
  if (typeof cookie !== "undefined") {
    Shiny.onInputChange("jscookie", cookie);
  } else {
    cookie = "";
    Shiny.onInputChange("jscookie", cookie);
  }
};

shinyjs.setcookie = function(params) {
  //console.info("setcookie");
  //console.info(params);
  Cookies.set("userLocation", escape(params), { expires: 3650 });
  Shiny.onInputChange("jscookie", params);
};

shinyjs.rmcookie = function(params) {
  Cookies.remove("userLocation");
  Shiny.onInputChange("jscookie", "");
};