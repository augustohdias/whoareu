module Blog.Component (
  Component,
  render,
  load,
) where

import           Blog.Setup.Load
import           Prelude          hiding (read)
import qualified Text.Blaze.Html5 as H

class Component a where
  load    :: Element ElementType -> a
  render  :: a -> H.Html

