{-# LANGUAGE DeriveGeneric #-}

module Hemplate.Types where

import Control.Monad.Reader
import Control.Monad.Error
import Data.Bson hiding (lookup)
import Text.Parsec

type Comp a = ReaderT CompEnv (ErrorT String IO) a

type HemParser a = ParsecT String () IO a

type Ident = String

type CompEnv = (Document, Maybe IterEnv)

type LoopElems = Document
type IterEnv = (Ident, LoopElems)

addKey :: Ident -> LoopElems -> CompEnv -> CompEnv
addKey ident elems (dict, _) =
  (dict, Just (ident, elems))

getDoc :: CompEnv -> Document
getDoc = fst

getIterEnv :: CompEnv -> Maybe IterEnv
getIterEnv (_, iterEnv) = iterEnv

data Stat = Script String
          | Var Ident
          | Template FilePath
          | Iteration Ident [Stat]
          | IterationVar Ident Ident
     deriving (Show)
