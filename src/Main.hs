{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Text (Text, pack)
import Data.List.Split
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH
import System.Environment

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Commit
    content  Text
    date     UTCTime
    deriving Show

Tag
    name    Text
    commit  CommitId
    deriving Show
|]

main :: IO ()
main = do
    (cmd:args) <- getArgs
    case cmd of 
        "commit" -> runSqlite "life.db" $ commit args
        _        -> putStrLn "no implemented"

--commit :: [String] -> IO ()
commit (content:_) = do
    today <- liftIO $ getCurrentTime
    commitId <- insert $ Commit (pack content) today

    mapM_ insert $ getTags commitId content

getTags :: CommitId -> String -> [Tag]
getTags commitId content = do
    map (\name -> Tag (pack name) commitId) $ 
        split (startsWith "@") content

{--main :: IO ()
main = runSqlite "life.db" $ do 
    buildDb

    commits <- select $ from $ \(c :: SqlExpr (Entity Commit)) -> do 
        return c

    tags <- select $ from $ \(c, t) -> do
        where_ (t ^. TagCommit ==. c ^. CommitId)
        return t

    liftIO $ print tags
    

buildDb = do 
    --today <- liftIO $ getCurrentTime
    runMigrationSilent migrateTables
    --commit <- insert $ Commit "Went to the @gym" today
    --insert $ Tag "gym" commit--}