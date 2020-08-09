const getOptions = require('loader-utils').getOptions
const yargs = require('yargs');
const path = require('path');
const child_process = require('child_process');
const fs = require('fs');


/* Figures out if webpack has been run in watch mode
    This currently means either that the `watch` command was used
    Or it was run via `webpack-dev-server`
*/
var _isInWatchMode = null;
var isInWatchMode = function(){
  if(_isInWatchMode !== null) {
    return _isInWatchMode;
  }
  // parse the argv given to run this webpack instance
  var argv = yargs(process.argv)
      .alias('w', 'watch')
      .alias('stdin', 'watch-stdin')
      .argv;

  var hasWatchArg = isFlagSet(argv, 'watch');
  var hasStdinArg = isFlagSet(argv, 'watch-stdin');

  var hasWebpackServe = Array.prototype.filter.call(process.argv, function (arg) {
    return arg.indexOf('webpack-serve') !== -1;
  }).length > 0;

  var hasWebpackDevServer = Array.prototype.filter.call(process.argv, function (arg) {
    return arg.indexOf('webpack-dev-server') !== -1;
  }).length > 0;

  _isInWatchMode = hasWebpackServe || hasWebpackDevServer || hasWatchArg || hasStdinArg;
  return _isInWatchMode;
};

var isFlagSet = function(args, flag) {
    return typeof args[flag] !== "undefined" && args[flag];
};

var depsAdded = false;

module.exports = function(source) {
  const options = Object.assign({ pscIde : true, cwd : '.'}, getOptions(this));
  const callback = this.async();
  

  if(this.mode !== 'production' && isInWatchMode()) {
    if(options.pscIde) {
      this.addContextDependency('output');
    } else {
      this.addContextDependency('purs/src');
      child_process.execSync('npx spago build');
    }
    callback(null, 'var PS = require("'+ path.resolve('output/Main/index.js') + "\");\nmodule.exports = PS;");  
  } else {
    var spago = child_process.spawn('npx', ['spago', 'bundle-module']);
    spago.stderr.on('data', data => {
      console.info(data.toString());
    });
    spago.on('close', data => {
      var file = fs.readFileSync('index.js');
      callback(null, file);
      fs.unlinkSync('index.js');
    });
  }
}
