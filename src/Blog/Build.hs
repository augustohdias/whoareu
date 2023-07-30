{-# LANGUAGE OverloadedStrings #-}

module Blog.Build (build) where

import qualified Blog.Component                  as BC
import qualified Blog.Component.Posts            as Posts
import qualified Blog.Component.Profile          as Profile
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H hiding (main)
import           Text.Blaze.Html5.Attributes     as A hiding (name)

_STYLE :: H.Attribute
_STYLE = A.style $ mconcat
  [ "min-height: 100vh;"
  , "max-width: 100vw;"
  , "overflow-x: hidden;"
  , "display: flex;"
  , "align-items: flex-start;"
  , "cursor: url(https://cur.cursors-4u.net/symbols/sym-1/sym86.cur), auto;"
  ]

_PROFILE_BOX_STYLE :: H.Attribute
_PROFILE_BOX_STYLE = A.style $ mconcat
  [ "display: inline-block;"
  , "max-height: 100%;"
  , "justify-content: center;"
  , "max-width: 100vw;"
  , "overflow: hidden"
  ]


_POSTS_BOX_STYLE :: H.Attribute
_POSTS_BOX_STYLE = A.style $ mconcat
  [ "display: flex;"
  , "flex-direction: column;"
  , "justify-content: center;"
  , "max-width: 100vw;"
  , "overflow: hidden"
  ]

build :: IO ()
build = do
    profile <- Profile.build
    posts <- Posts.build
    mapM_ (write profile) posts
    let links = Posts.borrowStyle $ mconcat $ Prelude.map Posts.toHyperLink posts
    let page = renderPage profile links
    writeFile "index.html" page
    where
      write :: H.Html -> Posts.Post -> IO ()
      write profile post = writeFile (Posts.getPostPath post) (renderPage profile (BC.renderHtml post))
      
renderPage :: H.Html -> H.Html -> String
renderPage profileContent bodyContent = renderHtml $ docTypeHtml $ do
    H.head $ do
      H.style $ mconcat [preEscapedToHtml (
          "a:link {color: white;}" ++
          "a:visited {color: white;}" ++
          "a:hover {color: grey;}" ++
          "a:active {color: grey;}"
          )]
      H.title "who am i?" 
    H.body ! (A.style $ mconcat
                [ "background-image: url(https://augustohdias.github.io/whoami/resources/background/bg_0.jpg);"
                , "background-size: cover;"
                , "background-attachment: fixed;"
                ]) $ do
      H.div ! _STYLE $ do
        H.div ! _PROFILE_BOX_STYLE  $ profileContent
        H.div ! _POSTS_BOX_STYLE    $ bodyContent

