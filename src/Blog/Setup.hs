
{-# LANGUAGE OverloadedStrings #-}

module Blog.Setup (setup) where

import qualified Blog.Component.Post    as P
import qualified Blog.Component.Profile as PI
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        ((</>))
import           System.IO

setup :: IO ()
setup = do
  putStrLn "Name your blog ¬"
  name <- getLine

  putStrLn "Perfect. Tell me the root directory for your blog (default: .) ¬"
  path <-  getLine
  let path' = if null path then "." else path

  createDirectoryWithFile (path' </> name </> "posts") "Post.md" P._DEFAULT
  createDirectoryWithFile (path' </> name </> "profile") "Info.md" PI._DEFAULT
  createDirectoryWithFile (path' </> name </> "resources") "img.jpeg" ""

  -- TODO: configurar repo git
  putStrLn "The files under 'posts' directory will be used to create your posts."
  putStrLn "To customize your profile section, you can edit the 'Info.md' under 'profile' directory."
  putStrLn "You can configure the repository details in the '.env' file."
  putStrLn "Run the 'serve' command to see a preview at 'localhost:8080'."

createDirectoryWithFile :: FilePath -> FilePath -> String -> IO ()
createDirectoryWithFile dirPath fileName content = do
  putStrLn $ "Creating " ++ dirPath ++ " directory."
  createDirectoryIfMissing True dirPath
  withFile (dirPath </> fileName) WriteMode $ \handle -> do
    hPutStrLn handle content
  putStrLn "Done!"

