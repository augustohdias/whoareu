{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Blog.Build.Load (loadPosts, loadProfile) where

import           Blog.Build.Element (Element, Header (..), Post, Profile)
import           Control.Exception  (IOException, try)
import           Control.Monad      ((>=>))
import qualified Data.List          as DL
import           System.Directory   (listDirectory)
import           System.FilePath    ((</>))
import qualified Text.Parsec        as Par
import qualified Text.Parsec.String as Par.String (Parser)


--- Load

listMarkdownFiles :: [FilePath] -> IO [FilePath]
listMarkdownFiles [] = return []
listMarkdownFiles (x:xs) = do
  result <- try (listDirectory x) :: IO (Either IOException [FilePath])
  case result of
    Left _ -> listMarkdownFiles xs
    Right names -> do
      let mdFiles = filter (".md" `DL.isSuffixOf`) (Prelude.map (x </>) names)
      rest <- listMarkdownFiles xs
      return $ mdFiles ++ rest

loadPosts :: IO [Element Post]
loadPosts = do
  files <- listMarkdownFiles ["./posts"]
  mapM readFileAndParse files
  where
    readFileAndParse = readFile >=> (return . parsePost)

loadProfile :: IO (Element Profile)
loadProfile = do
  files <- listMarkdownFiles ["./profile"]
  head $ map readFileAndParse files
  where
    readFileAndParse = readFile >=> (return . parseProfile)

--- Parser

parsePost :: String -> Element Post
parsePost c = undefined
  where
    header = parsePostHeader c
    body = parseBody c

parseProfile :: String -> Element Profile
parseProfile c = undefined
  where
    header = parseProfileHeader c
    body = parseBody c

parseBody :: String -> String
parseBody = unlines . drop 5 . lines

parsePostHeader :: String -> Header Post
parsePostHeader s = header
  where
    (fileType, date, title) = case Par.parse extractHeaderInfo "" s of
      Right t -> t
      _       -> ("", "", "")
    header = PostHeader{date = date, title = title, extra = [("", [])]}

parseProfileHeader :: String -> Header Profile
parseProfileHeader _ = ProfileHeader {}

extractHeaderInfo :: Par.String.Parser (String, String, String)
extractHeaderInfo = do
    _ <- Par.string "---"
    _ <- Par.endOfLine
    _ <- Par.string "type:"
    fileType <- Par.manyTill Par.anyChar Par.endOfLine
    _ <- Par.string "date:"
    date <- Par.manyTill Par.anyChar Par.endOfLine
    _ <- Par.string "title:"
    title <- Par.manyTill Par.anyChar Par.endOfLine
    _ <- Par.string "---"
    _ <- Par.endOfLine
    return (fileType, date, title)
