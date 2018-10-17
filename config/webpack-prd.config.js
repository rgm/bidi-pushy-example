var path = require('path');

module.exports = {
  entry: {
    'react': './src/js/react.js',
    'semantic_ui': './src/js/semantic_ui.js'
  },
  mode: 'production',
  output: {
    path: path.resolve(path.join(__dirname, "../target/public/js-out")),
    filename: '[name].min.js'
  }
}
