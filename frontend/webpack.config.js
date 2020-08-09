// Webpack uses this to work with directories
const path = require('path');

// This is main configuration object.
// Here you write different options and tell Webpack what to do
module.exports = {

  // Path to your entry point. From this file Webpack will begin his work
  entry: './ts/main.ts',
  devtool: 'inline-source-map',

  // Path and filename of your result bundle.
  // Webpack will bundle all JavaScript into this file
  output: {
    path: path.resolve(__dirname, '..', 'webroot', 'scripts'),
    filename: 'bundle.js'
  },

  // Default mode for Webpack is production.
  // Depending on mode Webpack will apply different things
  // on final bundle. For now we don't need production's JavaScript 
  // minifying and other thing so let's set mode to development
  mode: 'development',
  
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/
      }, 
      { test: /\.purs$/, 
        use: [ 
          { loader: 'spago-loader', 
            options: {
              pscIde: true,
              src: path.resolve('purs/src')
            }
          }
        ] 
      },
      {
        test: /\.scss$/,
        use: [ 
          'style-loader',
          'css-loader',
          'sass-loader',
        ]
      }
    ],
  },
  resolveLoader: {
    modules: ['node_modules', 'webpack_loaders']
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js', '.purs', '.scss'],
    alias: {
      '@purs': path.resolve("purs", "src"),
      '@styles': path.resolve("styles"),
    }
  }
};
