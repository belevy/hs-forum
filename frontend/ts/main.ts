const Main = (process.env.NODE_ENV === "production")
        ? require("../dist/index.js")
        : require("../output/Main/index.js");

document.addEventListener('DOMContentLoaded', function() {
  Main.main();
});

