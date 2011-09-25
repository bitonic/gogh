// Utilities for the compiled JavaScript templates

(function () {
    // Foreach functions for arrays and objects.
    if (typeof Array.prototype.foreach === 'undefined') {
        Array.prototype.foreach = function (f) {
            var i;
            for (i = 0; i < this.length; i++) {
                f(this[i]);
            }
        };
    }

    if (typeof Object.prototype.foreach === 'undefined') {
        Object.prototype.foreach = function (f) {
            var obj = this;
            var key;
            for (key in obj) {
                if (obj.hasOwnProperty(key)) {
                    f(obj[key]);
                }
            }
        };
    }

    // "empty" functions
    if (typeof Array.prototype.empty === 'undefined') {
        Array.prototype.empty = function () {
            this.length === 0;
        };
    }

    if (typeof Object.prototype.empty === 'undefined') {
        Object.prototype.empty = function() {
            var obj = this;
            var key;
            for (key in obj) {
                if (obj.hasOwnProperty(key)) {
                    return false;
                }
            }
            return true;
        }
    }

    // Select some keys from a dictionary
    if (typeof Object.prototype.getKeys === 'undefined') {
        Object.prototype.getKeys = function(source) {
            var source = this;
            var dest = {};
            var i;
            for (i; i < arguments.length; i++) {
                dest[arguments[i]] = source[arguments[i]];
            }
            return dest;
        }
    }
    
})();

function notNull(o) {
    return o !== null;
}

function isNull(o) {
    return o === null;
}
