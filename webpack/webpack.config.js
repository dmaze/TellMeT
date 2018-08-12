const path = require('path');
const webpack = require('webpack');

module.exports = (env) => ({
    mode: 'production',
    context: path.resolve(__dirname, '..'),
    entry: {
        index: [
            env.SRCDIR + '/all.js'
        ]
    },
    output: {
        path: path.resolve(__dirname, '..', 'dist'),
        filename: '[name].js'
    },
    module: {
        rules: [
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            }
        ]
    },
    node: {
        process: false
    },
    plugins: [
        new webpack.IgnorePlugin(/^(child_process|fs|os|path)$/)
    ]
});
