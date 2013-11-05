{-# LANGUAGE TemplateHaskell, TypeOperators, DataKinds #-}
module TH where

import Control.Lens
import Data.Vinyl
import Data.Vinyl.TH

foo = Field :: "foo" ::: Int

makeVinylFields [ ("bar", [t|Bool|])
                , ("baz", [t|String|])
                ]

type BarFields = '[ "bar" ::: Bool ]

makeVinylRecord "FooBaz" [ 'foo, 'baz ]

makeVinylExtendedRecord "FooBar" ''BarFields [ 'foo ]

r1 :: PlainRec FooBar
r1 = foo =: 3
 <+> bar =: False

r2 :: PlainRec FooBaz
r2 = foo =: 5
 <+> baz =: "BAZ"

-- |
-- >>> r1 ^. rLens foo
-- 3
-- >>> r1 ^. rLens bar
-- False
-- >>> r2 ^. rLens foo
-- 5
-- >>> r2 ^. rLens baz
-- "BAZ"
