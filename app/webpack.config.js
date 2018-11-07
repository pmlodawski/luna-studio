const path = require('path');

function baseConfig(entryPath) {
  return {
    mode: "production" // "production" | "development" | "none" // Chosen mode tells webpack to use its built-in optimizations accordingly.

    , devtool: "eval-source-map"
    , entry: entryPath
    , resolve:
      { modules: 
        [ "node_modules"
        , path.resolve(__dirname, "dist")
        ]
      , extensions: [".js", ".coffee"]
      }
    , module: 
      { strictExportPresence: true
      , rules: 
        [ { use: [ 'coffee-loader' ], test: /\.coffee$/ } ]
      }
    , output: 
      { filename: '[name].js'
      , path: path.resolve(__dirname, 'dist')
      }
    , devServer: 
      { contentBase: path.join(__dirname, 'dist')
      , publicPath: '/'
      , compress: true
      , port: 9000
      }
  }
}


serverConfig = baseConfig('./src/index.js');
clientConfig = baseConfig('./src/main.coffee');

serverConfig.target = 'electron-main';

module.exports = [serverConfig, clientConfig];
