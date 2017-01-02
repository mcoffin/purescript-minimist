module Minimist
    ( Arg(..)
    , MinimistOptions
    , interpretAsStrings
    , interpretAsBooleans
    , aliases
    , defaults
    , stopEarly
    , splitOnDoubleDash
    , unknown
    , parseArgs
    ) where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either, fromRight)
import Data.Foreign (Foreign, isArray, toForeign)
import Data.Foreign.Class (class IsForeign, read, readEitherL)
import Data.Functor.Contravariant ((>#<))
import Data.Options (Option, Options, opt, options)
import Data.StrMap (StrMap)
import Partial.Unsafe (unsafePartial)

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
interpretAsBooleans = opt "boolean" >#< either toForeign toForeign

-- | Mapping of strings to a list of aliases for that option
aliases :: Option MinimistOptions (StrMap (Array String))
aliases = opt "alias"

-- | Mapping of strings to default argument values
defaults :: Option MinimistOptions (StrMap Arg)
defaults = opt "default" >#< (<$>) toForeignArg

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

foreign import parseArgsForeign :: Array String -> Foreign -> StrMap Foreign

-- | Data type representing the value of an argument in `argv`
data Arg = ArgString String
         | ArgMulti (Array String)
         | ArgFlag Boolean

toForeignArg :: Arg -> Foreign
toForeignArg (ArgString s) = toForeign s
toForeignArg (ArgMulti a) = toForeign a
toForeignArg (ArgFlag b) = toForeign b

derive instance eqArg :: Eq Arg

instance showArg :: Show Arg where
    show (ArgString str) = str
    show (ArgMulti multi) = show multi
    show (ArgFlag flag) = show flag

instance argIsForeign :: IsForeign Arg where
    read value
        | isArray value = ArgMulti <$> read value
        | otherwise = readEitherL value <#> either ArgString ArgFlag

-- | Parse arguments with a set of minimist options into an `argv` representation,
-- | mapping argument names to `Arg` values.
parseArgs :: Array String -> Options MinimistOptions -> StrMap Arg
parseArgs args opts = (<$>) foreignToArg (parseArgsForeign args $ options opts) where
    foreignToArg :: Foreign -> Arg
    foreignToArg value = unsafePartial fromRight (runExcept $ read value)
