module Minimist
    ( parseArgs
    ) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn2, runFn2)
import Foreign (Foreign)
import Foreign.Object (Object)
import Data.Options (Options, options)
import Minimist.Options (MinimistOptions)

foreign import parseArgsForeign :: Fn2 Foreign (Array String) (Object Json)

-- | Parse command line options into an `Object`
parseArgs :: Options MinimistOptions -> Array String -> Object Json
parseArgs = options >>> runFn2 parseArgsForeign
