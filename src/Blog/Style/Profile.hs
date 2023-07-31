{-# LANGUAGE OverloadedStrings #-}

module Blog.Style.Profile (applyStyle) where

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

applyStyle :: String -> H.Html
applyStyle p = H.div H.! _STYLE $ do
  H.preEscapedString p

_STYLE :: H.Attribute
_STYLE = A.style $ mconcat
  [ "background-color: black;"
  , "color: white;"
  , "padding: 10px;"
  , "width: 375px;"
  , "margin: 15px;"
  , "border-radius: 15px;"
  , "font-family: 'Courrier new', monospace;"
  , "font-size: 12px;"
  , "text-align: justify;"
  , "display: flex;"
  , "flex-direction: column;"
  , "align-items: center;"
  , "flex-grow: 0;"
  , "flex-shrink: 0"
  ]
