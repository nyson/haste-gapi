{-# LANGUAGE OverloadedStrings #-}

module Haste.GAPI.Internals where 

import Haste.Foreign hiding (has, get)
import Haste.Prim (JSString)
import qualified Haste.Foreign as F

emptyString :: IO JSAny
emptyString = ffi "(function() {return \"\";})"

-- | as Haste.Foreign.get, but will return an emptystring on fail
getFail :: FromAny a => JSAny -> JSString -> IO a
getFail obj field = do
  hast <- F.has obj field
  if not hast then do putStrLn $  "Field '" ++ show field ++ "' missing!"
                      emptyString >>= fromAny
    else F.get obj field
