/* global exports */
"use strict";

function shrink(array, newLen) {
    if (array.byteLength === newLen) { return array; }
    return array.subarray(0, newLen);
}

exports.saveAsFile = function (filename) {
    return function (out) {
        return function () {
            var trimmed = shrink(out.array, out.offset),
                compressed = pako.gzip(trimmed),
                blob = new Blob([compressed], {type: 'application/obr'});
            if (window.navigator.msSaveOrOpenBlob) {
                window.navigator.msSaveBlob(blob, filename);
            } else {
                var elem = window.document.createElement('a');
                elem.href = window.URL.createObjectURL(blob);
                elem.download = filename;
                document.body.appendChild(elem)
                elem.click();
                document.body.removeChild(elem);
            }
        };
    };
};

exports.jsAlert = function (msg) {
    return function () {
        window.alert(msg);
    };
};

exports.getContextCanvas = function (ctx) { return ctx.canvas; }
