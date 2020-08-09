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

const defaultOptions = 
    {
      pscIde : true, 
      src: 'src',
      outputDir: 'output'
    };
module.exports = function(source) {
  const options = Object.assign(defaultOptions, getOptions(this));
  const callback = this.async();
  this.addContextDependency(options.src);

  if(this.mode !== 'production' && isInWatchMode()) {
    if(!options.pscIde) {
      var outputFlag = '-u "-o ' + path.resolve(options.outputDir) + '"';
      child_process.execSync('npx spago build ' + outputFlag);
    }
    const modulePath = path.resolve(this.resourcePath).replace(path.resolve(options.src) + '/', '').replace('.purs', '');
    const moduleName = modulePath.split('/').join('.');
    const moduleJsFile = path.resolve(options.outputDir, moduleName, 'index.js');
    callback(null, 'var PS = require("'+ moduleJsFile + "\");\nmodule.exports = PS;");  
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
