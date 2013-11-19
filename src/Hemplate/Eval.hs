{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Hemplate.Eval where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import System.Exit
import Data.Bson as B
import Data.Text

import Hemplate.Types
import Hemplate.Parser

runHemplate :: CompEnv -> [Stat] -> IO (Either String String) 
runHemplate compEnv stats =
  runErrorT $ runReaderT (evalHemplate stats) compEnv

-- Output of using evalHemplate should be output string of template when run
evalHemplate :: [Stat] -> Comp String
evalHemplate stats = foldM evalStat' "" stats

evalStat' :: String -> Stat -> Comp String
evalStat' str stat =
  evalStat stat >>= \str' ->
    return $ str ++ str'

evalStat :: Stat -> Comp String
evalStat (Script str) = return str

-- Read variable from a dictionary
evalStat (Var ident) = do
  var <- asks $ (B.lookup (pack ident)) . getDoc
  case var of
    Nothing -> throwError "Variable missing from dictionary"
    Just v  -> return v

-- Look up filePath and render file contents as new hemplate
evalStat (Template filePath) = do
  compEnv <- ask
  tempContents <- liftIO $ parseAndRunFile filePath compEnv
  return $ tempContents


-- Render Iteration statement, by looking up iteration identifier in dictionary, then
-- executing statements in iteration.
--
-- Alias for key to ident
-- Look up IterationVars inside dict key
-- key information should be in monad + alias information
-- possibly nested keys
-- data structure for iteration keys -> unique list

evalStat (Iteration ident stats) = do
  dict <- asks getDoc
  case pack ident `B.lookup` dict of
       Just elems -> foldM (evalLoopElem ident stats dict) "" elems
       Nothing -> return ""
    
-- Need to do:
-- Find some way of allowing the iterator list represented by the key to be accessible to computations inside the Iteration (below)
-- Alter the monad environment to contain enough information for Iteration to be executed
-- Then execute statements one time for each of the loop elements

evalStat (IterationVar loopKey var) = do
  (_, iterEnv) <- ask
  lookupVar iterEnv var loopKey
  where
    lookupVar iterEnv var loopKey =
      case iterEnv of
        Nothing -> throwError $ "Trying to evaluate `iteration' variable without `iteration' environment"
        Just (key, elems) -> checkKey key elems var loopKey

    checkKey key elems var loopKey =
      if key == loopKey
         then return $ pack var `at` elems
         else throwError $ "Given wrong Iteration Key"

evalLoopElem ident stats dict str doc = 
  local (addKey ident doc) (evalHemplate stats) >>= (\x -> return $ str ++ x)

parseAndRunFile :: FilePath -> CompEnv -> IO String
parseAndRunFile fName compEnv = do
  ast <- parseFile fName
  comp <- runHemplate compEnv ast
  case comp of
    Left e  -> return e
    Right s -> return s

