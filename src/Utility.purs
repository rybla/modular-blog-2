module Utility where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "[bug]\n" <> msg

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "[todo]\n" <> msg

