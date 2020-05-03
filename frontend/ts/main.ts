import { Elm } from "Elm/Main.elm"

document.addEventListener('DOMContentLoaded', function() {
  var app = Elm.Main.init({
    flags: {
      window: {height: window.innerHeight, width: window.innerWidth}
    }
  });
});
