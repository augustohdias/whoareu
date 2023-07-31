{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}

module Blog.Component.Post (_DEFAULT, _STYLE) where

import           Prelude                     hiding (read)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Blog.Setup.Load (markdownToHtml,
  Element(ElementPost, ElementProfile), ElementType, Renderable(render), Rendered(RenderedPost, html))

{-
blazeComponent p = H.div H.! _STYLE $ do
  H.preEscapedString $ html p
  H.a H.! A.href "../index.html" $ "<<<"

replaceSpaces :: String -> String
replaceSpaces = Prelude.map (\c -> if c == ' ' then '-' else c)

pathOf :: Element ElementType -> String
pathOf post = "./posts/" ++ date post ++ "-" ++ replaceSpaces (title post) ++ ".html"

referenceFor :: Element ElementType -> H.Html
referenceFor (ElementPost body _) = H.div 
  H.! A.style "display: flex; justify-content: space-between; min-height: 20px; font-size: 12px; padding: 15px; margin: 15px;" $ do
    H.a H.! A.href (H.stringValue (pathOf post)) $ H.toHtml $ title post
    H.toHtml $ " :: " ++ date post

borrowStyle :: H.Html -> H.Html
borrowStyle = H.div H.! _STYLE
-}

_DEFAULT :: String
_DEFAULT= "---\ntype: BlogPost\ndate: 29.07.23\ntitle: Post title\n---\n# Titulo\n\nWada wada wada wadawada wadawadawada wada a wada."

_STYLE :: H.Attribute
_STYLE = A.style $ mconcat
  [ "background-color: black;"
  , "color: white;"
  , "padding: 15px;"
  , "width: 600px;"
  , "margin: 15px;"
  , "border-radius: 10px;"
  , "font-family: 'Courrier new', monospace;"
  , "font-size: 12px;"
  , "text-align: justify;"
  , "display: flex;"
  , "flex-direction: column;"
  , "flex-grow: 0;"
  , "flex-shrink: 0;"
  ]

