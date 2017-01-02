"use strict";

// module Minimist

exports.parseArgsForeign = function parseArgsForeign(args) {
    return function (opts) {
        return require("minimist")(args, opts);
    };
};
