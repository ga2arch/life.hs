{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, DeriveDataTypeable#-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, ScopedTypeVariables #-}

module Types where

import Data.Data
import Data.Text (Text)
import Data.Time
import Data.Typeable
import Database.Persist.TH

data Command = Command { 
    cmd :: String 
,   commandArgs :: [String]
} deriving (Show, Data, Typeable)

--  SQL 

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Commit
    content  Text
    date     UTCTime
    day      Day
    deriving Show
    deriving Eq

Tag
    name    Text
    commit  CommitId
    deriving Show
    deriving Eq
|]


