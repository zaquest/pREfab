/* global exports */
"use strict";

// module JQuery

exports.clientX = function (event) {
    return function () {
        return event.clientX;
    };
};


exports.clientY = function (event) {
    return function () {
        return event.clientY;
    };
};
