{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Text
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH

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
main = runSqlite ":memory:" $ do 
    buildDb

    commits <- select $ from $ \(c :: SqlExpr (Entity Commit)) -> do 
        return c

    tags <- select $ from $ \(c, t) -> do
        where_ (t ^. TagCommit ==. c ^. CommitId)
        return t

    liftIO $ print tags
    

buildDb = do 
    today <- liftIO $ getCurrentTime
    runMigrationSilent migrateTables
    commit <- insert $ Commit "Went to the @gym" today
    insert $ Tag "gym" commit