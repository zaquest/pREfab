/* global exports */
"use strict";

// module Ref

exports.newRef = function (val) {
    return function () {
        return { value: val };
    };
};

exports.writeRef = function (ref) {
    return function (a) {
        return function () {
            return ref.value = a;
        };
    };
};

exports.readRef = function (ref) {
    return function () {
        return ref.value;
    };
};

exports.modifyRef = function (ref) {
    return function (f) {
        return function () {
            return ref.value = f(ref.value);
        };
    };
};
