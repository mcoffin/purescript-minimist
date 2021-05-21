module Minimist.Data.Either.Partial
    ( fromRightPartial
    , unsafeFromRight
    ) where

import Data.Either (Either(..))
import Partial.Unsafe (unsafePartial)

fromRightPartial :: ∀ a b. Partial => Either a b -> b
fromRightPartial (Right b) = b

unsafeFromRight :: ∀ a b. Either a b -> b
unsafeFromRight = unsafePartial fromRightPartial
