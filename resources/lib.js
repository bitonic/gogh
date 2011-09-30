"use strict";

// Utilities for the compiled JavaScript templates

// An utility function to build strings
function StringBuilder() {
    var strings = [];

    return {
        append: function (string) {
            strings.push(string);
        },

        toString: function () {
            return strings.join('');
        }
    };
}

// Foreach functions for arrays and objects.
Array.prototype.foreach = function (f) {
    var i;
    for (i = 0; i < this.length; i++) {
        f(this[i]);
    }
};

Object.prototype.foreach = function (f) {
    var key;
    for (key in this) {
        if (this.hasOwnProperty(key)) {
            f(this[key]);
        }
    }
};

// "empty" functions
Array.prototype.empty = function () {
    return this.length === 0;
};

Object.prototype.empty = function () {
    var key;
    for (key in this) {
        if (this.hasOwnProperty(key)) {
            return false;
        }
    }
    return true;
};

// Select some keys from a dictionary
Object.prototype.getKeys = function () {
    var dest = {};
    var i;
    for (i; i < arguments.length; i++) {
        dest[arguments[i]] = this[arguments[i]];
    }
    return dest;
};

// Safe show
String.prototype.safeShow = function () {
    return this
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/'/g, '&#39')
        .replace(/"/g, '&quot;');
};

Number.prototype.safeShow = Number.prototype.toString;

Boolean.prototype.safeShow = Boolean.prototype.toString;

if (typeof Gogh === 'undefined') {
    var Gogh = {};
}

Gogh.notNull = function (o) {
    return o !== null;
}

Gogh.isNull = function (o) {
    return o === null;
}
