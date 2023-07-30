{-# LANGUAGE OverloadedStrings #-}

module Blog.Component.Posts (_DEFAULT, build, Post, date, title, toHyperLink, getPostPath, borrowStyle) where

import qualified Blog.Component              as BC
import qualified Blog.Setup.Template         as BT
import           Control.Exception           (IOException, try)
import           Data.Maybe                  (catMaybes)
import           System.Directory            (listDirectory)
import           System.FilePath             ((</>))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.List (isInfixOf)

_DEFAULT :: String
_DEFAULT= "---\ntype: BlogPost\ndate: 29.07.23\ntitle: Post title\n---\n# Titulo\n\nWada wada wada wadawada wadawadawada wada a wada."

_STYLE :: H.Attribute
_STYLE = A.style $ mconcat
  [ "background-color: black;"
  , "color: white;"
  , "padding: 10px;"
  , "width: 400px;"
  , "margin: 10px;"
  , "border-radius: 10px;"
  , "font-family: 'Courrier new', monospace;"
  , "font-size: 10px;"
  , "text-align: justify;"
  , "display: flex;"
  , "flex-direction: column;"
  , "flex-grow: 0;"
  , "flex-shrink: 0;"
  ]

data Post = Post { htmlContent :: String, date :: String, title :: String }

instance BC.Component Post where
  renderHtml p = H.div H.! _STYLE $ H.preEscapedString $ htmlContent p
  parseTemplate template = Post { htmlContent = BT.html template, title = BT.title h, date = BT.date h }
    where h = BT.header template

build :: IO [Post]
build = do
  posts       <- loadPosts "./posts/"
  defaultPost <- BT.readFromString _DEFAULT
  case posts of
    [] -> return [BC.parseTemplate defaultPost :: Post]
    _  -> return posts

loadPosts :: String -> IO [Post]
loadPosts path = do
  files <- getFiles path
  catMaybes <$> mapM BC.loadFromFile files :: IO [Post]

getFiles :: String -> IO [FilePath]
getFiles path = do
  result <- try (listDirectory path) :: IO (Either IOException [FilePath])
  case result of
    Left _      -> return []
    Right names -> return $ map (path </>) $ filter (contains ".md") names

contains :: String -> String -> Bool
contains needle haystack = needle `isInfixOf` haystack

replaceSpaces :: String -> String
replaceSpaces = Prelude.map (\c -> if c == ' ' then '-' else c)

getPostPath :: Post -> String
getPostPath post = "./posts/" ++ date post ++ "-" ++ replaceSpaces (title post) ++ ".html"

toHyperLink :: Post -> H.Html
toHyperLink post = H.div H.! A.style "display: flex; justify-content: space-between; min-height: 20px; font-size: 11px; padding: 10px; margin: 10px;" $ do
    H.a H.! A.href (H.stringValue (getPostPath post)) $ H.toHtml $ title post
    H.toHtml $ " :: " ++ date post

borrowStyle :: H.Html -> H.Html
borrowStyle = H.div H.! _STYLE 

