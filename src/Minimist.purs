module Minimist where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), fromRight)
import Data.Foreign (Foreign, isArray, toForeign)
import Data.Foreign.Class (class IsForeign, read, readEitherL)
import Data.Functor.Contravariant ((>#<))
import Data.Options (Option, Options, opt, options)
import Data.StrMap (StrMap)
import Partial.Unsafe (unsafePartial)

data MinimistOptions

interpretAsStrings :: Option MinimistOptions (Array String)
interpretAsStrings = opt "strings"

interpretAsBooleans :: Option MinimistOptions (Either Boolean (Array String))
interpretAsBooleans = opt "boolean" >#< \e ->
    case e of
      Left b -> toForeign b
      Right strs -> toForeign strs

aliases :: Option MinimistOptions (StrMap (Array String))
aliases = opt "alias"

defaults :: Option MinimistOptions (StrMap Arg)
defaults = opt "default" >#< (<$>) toForeignArg

stopEarly :: Option MinimistOptions Boolean
stopEarly = opt "stopEarly"

splitOnDoubleDash :: Option MinimistOptions Boolean
splitOnDoubleDash = opt "--"

unknown :: Option MinimistOptions (String -> Boolean)
unknown = opt "unknown"

foreign import parseArgsForeign :: Array String -> Foreign -> StrMap Foreign

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
        | otherwise = readEitherL value <#> \e ->
            case e of
              Left str -> ArgString str
              Right b -> ArgFlag b

parseArgs :: Array String -> Options MinimistOptions -> StrMap Arg
parseArgs args opts = (<$>) foreignToArg (parseArgsForeign args $ options opts) where
    foreignToArg :: Foreign -> Arg
    foreignToArg value = unsafePartial fromRight (runExcept $ read value)
