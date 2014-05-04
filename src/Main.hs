{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Time
import Data.Text (Text, pack)
import Data.List (foldl')
import Data.List.Split
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH
import System.Environment

import qualified Data.Map as M

--import Types

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Commit
    content  Text
    date     UTCTime
    day      Day
    deriving Show

Tag
    name    Text
    commit  CommitId
    deriving Show
|]

main :: IO ()
main = do
    runSqlite "life.db" $ runMigrationSilent migrateTables

    (cmd:args) <- getArgs

    --let cmd = "show"
    --let args = []
    case cmd of 
        "commit" -> commitCmd args
        "show"   -> showCmd args
        _        -> putStrLn "no implemented"

--commit :: [String] -> IO ()
commitCmd (content:_) = runSqlite "life.db" $ do
    now <- liftIO $ getCurrentTime
    commitId <- insert $ Commit (pack content) now (utctDay now)
    mapM_ insert $ getTags commitId content

showCmd [] = runSqlite "life.db" $ do

    commits <- select 
        $ from 
        $ \(c :: SqlExpr (Entity Commit)) -> do 
            return c

    let d = foldl' f M.empty commits

    --mapM_ pp $ M.toList d
    liftIO $ print d
  where
    f m c = do
        let commit@(Commit content date day) = entityVal c
        M.insertWithKey u day [content] m
    u _ nv ov = ov ++ nv

    --pp = 

getTags :: CommitId -> String -> [Tag]
getTags commitId content = do
    map (\name -> Tag (pack name) commitId) $ 
        split (startsWith "@") content