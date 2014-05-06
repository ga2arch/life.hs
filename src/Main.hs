{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Time
import Data.Text (Text, pack, unpack)
import Data.List (foldl', sortBy)
import Data.List.Split
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Text.PrettyPrint.ANSI.Leijen
import System.Environment
import System.Console.CmdArgs

import qualified Data.Map as M

import Types

database = "life.db"

main :: IO ()
main = runSqlite database $ do
    runMigrationSilent migrateTables

    Command{..} <- liftIO $ cmdArgs $ Command {
        cmd = def &= argPos 0 &= opt ("show" :: String)
    ,   commandArgs = def &= args
    }
    case cmd of 
        "commit" -> commitCmd commandArgs >> showCmd []
        "show"   -> showCmd commandArgs
        "delete" -> deleteCmd commandArgs >> showCmd []
        _        -> liftIO $ putStrLn "not implemented"
  

groupCommitsByDate = foldl' f M.empty
  where
    f m c = do
        let (Commit content date day) = entityVal c
        M.insertWithKey u day [(c, text.unpack $ content)] m

    u _ nv ov = ov ++ nv

allCommits = select $ 
    from $ \(c :: SqlExpr (Entity Commit)) -> do
        return c

commitCmd (content:_) = do
    now <- liftIO $ getCurrentTime
    commitId <- insert $ Commit (pack content) now (utctDay now)
    mapM_ insert $ getTags commitId content

showCmd _ = do
    d <- fmap groupCommitsByDate allCommits
    mapM_ pp $ M.toDescList d
  where
    pp (day, commits) = liftIO $ putDoc $ mkDoc day commits
    mkDoc day commits = (text $ show day) 
        <$$> indent 1 (vsep $ map (((<+>) $ dot <> space).snd) commits)
        <$$> linebreak 

deleteCmd (d:pos:_) = do
    m <- fmap groupCommitsByDate allCommits
    let key = M.keys m !! (read d - 1)
    let k = entityKey . fst $ (m M.! key) !! (read pos - 1)

    delete $ from $ \c -> do
        where_ (c ^. CommitId ==. val k)

getTags :: CommitId -> String -> [Tag]
getTags commitId content = do
    map (\name -> Tag (pack name) commitId) $ 
        split (startsWith "@") content

