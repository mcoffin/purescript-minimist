"use strict";

// module Minimist

import minimist from 'minimist';

export function parseArgsForeign(opts, args) {
    return minimist(args, opts);
}
