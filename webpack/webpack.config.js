const path = require('path');
const webpack = require('webpack');

module.exports = (env) => ({
    mode: 'production',
    entry: {
        index: [
            env.SRCDIR + '/all.js',
            'bootstrap/dist/css/bootstrap.min.css',
            '@fortawesome/fontawesome-free/css/all.css'
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
            },
            {
                test: /\.(eot|svg|ttf|woff|woff2)$/,
                loader: 'url-loader',
                options: {
                    limit: 65536
                }
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
