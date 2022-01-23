const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin;
const outputDir = path.join(__dirname, 'build/');

const isProd = process.env.NODE_ENV === 'production';
const withAnalyzer = process.env.ANALYZE == "true";

module.exports = {
  entry: './_build/default/src/main.bc.js',
  mode: isProd ? 'production' : 'development',
  resolve: {
    modules: [path.resolve(__dirname, 'node_modules'), 'node_modules']
  },
  output: {
    path: outputDir,
    filename: 'index.js'
  },
  node: {
    fs: 'empty',
    child_process: 'empty'
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: 'src/index.html',
      inject: false,
      favicon: "./src/favicon.ico"
    }),
    ...(withAnalyzer ? [new BundleAnalyzerPlugin({
      analyzerMode: "static",
    })] : [])
  ],
  devServer: {
    compress: true,
    contentBase: outputDir,
    port: process.env.PORT || 8000,
    historyApiFallback: true
  },
  module: {
    rules: [
      {
        test: /\.css$/i,
        use: ['style-loader', 'css-loader'],
      },
    ],
  },
};
