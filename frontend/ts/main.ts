import { Elm } from "@elm/Main.elm"
import "@styles/main.scss"

document.addEventListener('DOMContentLoaded', function() {
  var app = Elm.Main.init({
    flags: {
      window: {height: window.innerHeight, width: window.innerWidth}
    }
  });
});
