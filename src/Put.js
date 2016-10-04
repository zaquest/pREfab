/* global exports */
"use strict";

function grow(out, size) {
    if (out.view.byteLength > out.offset + size) { return; }
    var newLen = out.view.byteLength * 2;
    while (newLen < out.offset + size) { newLen *= 2; }
    var ab = new ArrayBuffer(newLen),
        dest = new Uint8Array(ab),
        source = new Uint8Array(out.view.buffer, out.view.byteOffset, out.offset);
    dest.set(source);
    out.view = new DataView(ab);
    out.array = new Uint8Array(ab);
}

exports.newOutput = function () {
    var ab = new ArrayBuffer(1024);
    return { view: new DataView(ab)
           , array: new Uint8Array(ab)
           , offset: 0 };
};

exports.putter = function (name) {
    return function (size) {
        return function (littleEndian) {
            return function (val) {
                return function (out) {
                    return function () {
                        grow(out, size);
                        out.view[name](out.offset, val, littleEndian);
                        out.offset += size;
                    };
                };
            };
        };
    };
};

exports.putASCII = function (str) {
    return function (out) {
        return function () {
            grow(out, str.length);
            var bytes = Array.from(str, function (c) { return c.codePointAt(0); });
            out.array.set(bytes, out.offset);
            out.offset += str.length;
        };
    };
};
