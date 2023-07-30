
module Blog.Setup.Template (readFromString, loadTemplateFromFile, header, html, BlogElementTemplate, Header, fileType, date, title) where

import           Control.Exception
import           Data.Text          as T
import           Text.Pandoc
import           Text.Parsec
import           Text.Parsec.String (Parser)

data Header = Header { fileType :: String, date :: String, title :: String }

data BlogElementTemplate = BlogElementTemplate { header :: Header, html :: String }

processContent :: (Int -> [String] -> [String]) -> String -> String
processContent method = Prelude.unlines . method 5 . Prelude.lines

getProfile :: String -> String
getProfile = processContent Prelude.drop

getHeader :: String -> String
getHeader = processContent Prelude.take

readMarkdownToHTML :: String -> IO String
readMarkdownToHTML markdown = do
  eitherResult <- runIO $ do
    doc <- readMarkdown def $ T.pack markdown
    writeHtml5String def doc
  case eitherResult of
    Right html -> return . T.unpack $ html
    Left err   -> throwIO $ userError $ show err

readFromString :: String -> IO BlogElementTemplate
readFromString content = do
  let profileHeader = getHeader content
  let profileBody = getProfile content
  let parsedHeader = parseHeader profileHeader
  result <- readMarkdownToHTML profileBody
  case parsedHeader of
    Right h -> return $ BlogElementTemplate { header = h, html = result }
    _       -> return $ BlogElementTemplate { header = Blog.Setup.Template.Header {fileType="None", date="01-01-01", title="default"}, html = result }

parseHeader' :: Parser Blog.Setup.Template.Header
parseHeader' = do
    _ <- string "---"
    _ <- endOfLine
    _ <- string "type: "
    fileType <- manyTill anyChar endOfLine
    _ <- string "date: "
    date <- manyTill anyChar endOfLine
    _ <- string "title: "
    title <- manyTill anyChar endOfLine
    _ <- string "---"
    _ <- endOfLine
    return $ Blog.Setup.Template.Header fileType date title

parseHeader :: String -> Either ParseError Blog.Setup.Template.Header
parseHeader = parse parseHeader' ""

safeReadFile :: FilePath -> IO (Either IOException String)
safeReadFile path = (Right <$> readFile path) `catch` (return . Left)

loadTemplateFromFile :: FilePath -> IO (Maybe BlogElementTemplate)
loadTemplateFromFile path = do
  result <- safeReadFile path
  case result of
    Left _        -> return Nothing
    Right content -> Just <$> readFromString content

