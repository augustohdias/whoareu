{-# LANGUAGE OverloadedStrings #-}

module Blog.Style.Post (applyStyle) where

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

applyStyle :: String -> H.Html
applyStyle p = H.div H.! _STYLE $ do
  H.preEscapedString p
  H.a H.! A.href "../index.html" $ "<<<"

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

