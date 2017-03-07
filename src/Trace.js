/* global exports */
"use strict";

// module Trace

exports.trace = function (a) {
    console.log(a);
    return a;
};

exports.traceMsg = function (msg) {
    return function (a) {
        console.log(msg);
        return a;
    };
};
