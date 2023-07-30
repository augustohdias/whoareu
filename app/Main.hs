{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Blog.Setup as S
import System.Console.CmdArgs as Cmd
import System.Environment (getArgs, withArgs)
import Blog.Build as Build

data WhoAreU =  Init 
              | Post
              | Build
              | Serve
  deriving (Show, Data, Typeable)

cmdInit :: WhoAreU
cmdInit = Init
  &= Cmd.help "Start a new blog."
  &= Cmd.explicit
  &= Cmd.name "init"

cmdPost :: WhoAreU
cmdPost = Post
   &= Cmd.help "Commit your posts to the configured repository."
   &= Cmd.explicit
   &= Cmd.name "post"

cmdBuild :: WhoAreU
cmdBuild = Build
  &= Cmd.help "Build all the static pages."
  &= Cmd.explicit
  &= Cmd.name "build"

cmdServe :: WhoAreU
cmdServe = Serve
  &= Cmd.help "Serve the webpage at 'localhost:8080'."
  &= Cmd.explicit
  &= Cmd.name "serve"

appModes :: WhoAreU
appModes = Cmd.modes [cmdInit, cmdPost, cmdBuild, cmdServe] 
  &= Cmd.help "A 'jekyll-like' blog tool."
  &= Cmd.program "whoareu"

main :: IO ()
main = do 
  programArgs <- getArgs
  mode <- (if null programArgs then withArgs ["--help"] else Prelude.id) $ Cmd.cmdArgs appModes 
  case mode of
    Init -> S.setup
    Post -> putStrLn "Post"
    Build -> Build.build
    Serve -> putStrLn "Serve"

