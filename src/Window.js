/* global exports */

exports.onResize = function (handler) {
    return function () {
        window.onresize = function (event) { handler(event)(); };
    };
};
