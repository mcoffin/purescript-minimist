module Minimist.Options
    ( MinimistOptions
    , interpretAsStrings
    , interpretAsBooleans
    , aliases
    , defaults
    , stopEarly
    , splitOnDoubleDash
    , unknown
    ) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either, either)
import Data.Functor.Contravariant ((>#<))
import Data.Options (Option, opt)
import Foreign.Object (Object)

-- | Phantom data type of options for the minimist parser
data MinimistOptions

-- | Array of strings to always be interpreted as strings
interpretAsStrings :: Option MinimistOptions (Array String)
interpretAsStrings = opt "strings"

-- | Array of strings to always be interpreted as booleans
-- | or a boolean value indicating that all options starting
-- | with `--` that don't contain an equals sign should be
-- | interpreted as booleans
interpretAsBooleans :: Option MinimistOptions (Either Boolean (Array String))
interpretAsBooleans = opt "boolean" >#< either encodeJson encodeJson

-- | Mapping of strings to a list of aliases for that option
aliases :: Option MinimistOptions (Object (Array String))
aliases = opt "alias"

-- | Mapping of strings to default argument values
defaults :: Option MinimistOptions (Object Json)
defaults = opt "default" >#< map encodeJson

-- | When `true`, argument parsing will stop at the first non-flag
stopEarly :: Option MinimistOptions Boolean
stopEarly = opt "stopEarly"

-- | When `true`, all options appearing after `--` will be placed
-- | into `--` instead of `_` in the result mapping
splitOnDoubleDash :: Option MinimistOptions Boolean
splitOnDoubleDash = opt "--"

-- | Function to be called with every argument not found in the
-- | configuration object. If the function retruns false, then the
-- | argument is not added to argv.
unknown :: Option MinimistOptions (String -> Boolean)
unknown = opt "unknown"
