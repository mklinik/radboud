module Options where

import System.IO

data Options = Options
  { input :: Handle
  , output :: Handle
  }
  deriving (Show)

defaultOptions = Options
  { input = stdin
  , output = stdout
  }
