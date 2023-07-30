module Blog.Component (
  Component,
  renderHtml,
  loadFromFile,
  parseTemplate,
) where
import qualified Blog.Setup.Template as BT
import qualified Text.Blaze.Html5    as H

class Component a where
  parseTemplate :: BT.BlogElementTemplate -> a
  renderHtml :: a -> H.Html

loadFromFile :: Component c => String -> IO (Maybe c)
loadFromFile path = do
  maybeTemplate <- BT.loadTemplateFromFile path
  return $ parseTemplate <$> maybeTemplate

