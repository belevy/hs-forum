(function() {
  var script = document.createElement("script");
  var scripts = document.getElementsByTagName("script")[0];
  script.src = "/scripts/elm.js"
  script.onload = init
  scripts.parentNode.insertBefore(script, scripts);

  function init() {
    var app = Elm.Main.init();
  }
})();
