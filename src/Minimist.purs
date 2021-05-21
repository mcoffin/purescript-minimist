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
    , parseArgsForeign
    ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either (Either, either)
import Minimist.Data.Either.Partial (unsafeFromRight)
import Foreign (Foreign, unsafeToForeign, unsafeFromForeign)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Object (Object)
import Data.Functor.Contravariant ((>#<))
import Data.Options (Option, Options, opt, options)

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
interpretAsBooleans = opt "boolean" >#< either unsafeToForeign unsafeToForeign

-- | Mapping of strings to a list of aliases for that option
aliases :: Option MinimistOptions (Object (Array String))
aliases = opt "alias"

-- | Mapping of strings to default argument values
defaults :: Option MinimistOptions (Object Arg)
defaults = opt "default" >#< (<$>) encode

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

foreign import parseArgsForeign :: Array String -> Foreign -> Foreign

-- | Data type representing the value of an argument in `argv`
data Arg = ArgString String
         | ArgFlag Boolean
         | ArgInt Int
         | ArgNum Number
         | ArgArray (Array Arg)

derive instance eqArg :: Eq Arg

instance showArg :: Show Arg where
    show (ArgString str) = str
    show (ArgFlag flag) = show flag
    show (ArgInt i) = show i
    show (ArgNum n) = show n
    show (ArgArray a) = show a

instance argDecode :: Decode Arg where
    decode value =
        (ArgString <$> decode value) <|>
        (ArgFlag <$> decode value) <|>
        (ArgInt <$> decode value) <|>
        (ArgNum <$> decode value) <|>
        (ArgArray <$> decode value)

instance argEncode :: Encode Arg where
    encode (ArgString s) = encode s
    encode (ArgFlag b) = encode b
    encode (ArgInt i) = encode i
    encode (ArgNum n) = encode n
    encode (ArgArray a) = encode a

-- | Parse arguments with a set of minimist options into an `argv` representation,
-- | mapping argument names to `Arg` values.
parseArgs :: Array String -> Options MinimistOptions -> Object Arg
parseArgs args opts = foreignToArg <$> (unsafeFromForeign $ parseArgsForeign args $ options opts) where
    foreignToArg :: Foreign -> Arg
    foreignToArg = unsafeFromRight <$> runExcept <$> decode
