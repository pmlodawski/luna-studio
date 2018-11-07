const path = require('path');

module.exports = {
  mode: "production", // "production" | "development" | "none"
  // Chosen mode tells webpack to use its built-in optimizations accordingly.

  devtool: "eval-source-map",
  entry: './src/index.js',
  resolve: {
    modules: [
      "node_modules",
      path.resolve(__dirname, "dist")
    ],
    extensions: [".js", ".coffee"]
  },
  module: {
    strictExportPresence: true,
    rules: [
      { use: [ 'coffee-loader' ], test: /\.coffee$/ }
    ]
  },

  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'dist/lib')
  },
  devServer: {
    contentBase: path.join(__dirname, 'dist'),
    compress: true,
    port: 9000
  }
};
