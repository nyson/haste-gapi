{- Generates perfect requests every time
<http://community.haskell.org/~simonmar/papers/haxl-icfp14.pdf There is no Fork>
-}

module Haste.GAPI.Request.Batch where


-- | Monad which is responsible for GAPI request handling
data Batch a = Done a | Waiting (Batch a)

instance Functor Batch where
  -- Requests that are done can be applied directly 
  fmap f (Done x) = Done $ f x
  -- Waiting requests push down the function to lower levels
  fmap f (Waiting c) = Waiting (fmap f c)

instance Monad Batch where
  return = Done
  -- We just apply with done
  Done a    >>= k = k a
  -- We apply the inner value with done
  Waiting c >>= k = Waiting (c >>= k)

instance Applicative Batch where 
  pure = return
  Done g    <*> Done y    = Done (g y)
  Done g    <*> Waiting w = Waiting (g <$> w)
  Waiting c <*> Done y    = Waiting (c <*> Done y)
  Waiting c <*> Waiting d = Waiting (c <*> d)

