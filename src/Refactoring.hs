module Refactoring () where

data ElementType = Post | Profile | Custom
  deriving (Show, Eq)
data Header = Header { elementType:: ElementType, date :: String, title :: String, extra :: [(String, [String])] }
  deriving (Show, Eq)
data Element = Element { header :: Header, body :: String }
  deriving (Show, Eq)

class ProcessFile a where
    process :: Element -> IO a

