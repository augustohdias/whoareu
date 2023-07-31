{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Build (build) where

import Blog.Build.Element (Rendered(..), Element(..), Profile, Post, Renderable(..))
import Blog.Build.Load (loadPosts, loadProfile)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

build :: IO b
build = do
  profile <- loadProfile
  let renderedProfile = render profile :: Rendered (Element Profile)
  posts <- loadPosts
  let renderedPosts = map render posts
  _ <- writePostsPages renderedProfile renderedPosts
  writeIndexPage renderedProfile renderedPosts

writePostsPages :: Rendered (Element Profile) -> [Rendered (Element Post)] -> IO b
writePostsPages _ _ = undefined

writeIndexPage :: Rendered (Element Profile) -> [Rendered a] -> IO b
writeIndexPage _ _ = undefined

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

{-- Assembly the entire blog
build :: IO ()
build = do
  elements <- extractElements
  let (profile', posts') = elements
  let profile = render (load profile' :: Profile.Profile)
  let posts = Prelude.map load posts' :: [Post.Post]
  writePostsPages profile posts
  writeIndexPage profile posts
  where
    writeIndexPage profile posts = do
      let links = mconcat $ Prelude.map hyperlinkOf posts
      let page = renderPage profile (Post.borrowStyle links)
      writeFile "index.html" page

    writePostsPages :: H.Html -> [Post.Post] -> IO ()
    writePostsPages profile = mapM_ (write profile)

    write :: H.Html -> Post.Post -> IO ()
    write profile post = writeFile (Post.pathOf post) (renderPage profile (render post))
--}

renderPage :: String -> String -> String
renderPage profileContent bodyContent = renderHtml $ H.docTypeHtml $ do
  H.head $ do
    H.style $ mconcat [H.preEscapedToHtml (
        "a:link {color: white;}" ++
        "a:visited {color: white;}" ++
        "a:hover {color: grey;}" ++
        "a:active {color: grey;}"
        )]
    H.title "who am i?"
  H.body H.! A.style ( mconcat
              [ "background-image: url(https://augustohdias.github.io/whoami/resources/background/bg_0.jpg);"
              , "background-size: cover;"
              , "background-attachment: fixed;"
              ]) $ do
    H.div H.! _STYLE $ do
      H.div H.! _PROFILE_BOX_STYLE  $ H.preEscapedToHtml profileContent
      H.div H.! _POSTS_BOX_STYLE    $ H.preEscapedToHtml bodyContent
