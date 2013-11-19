{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Hemplate.Parser (parseFile) where

import Data.Char (isAlphaNum)
import Text.Parsec
import System.Exit

import Hemplate.Types

parseHemplate :: HemParser [Stat]
parseHemplate = many1 parseHemplate'

parseHemplate' :: HemParser Stat
parseHemplate' = parseScript
             <|> try parsePolyStat
             <|> try parseMonoStat
             <?> "valid content"

parseScript :: HemParser Stat
parseScript = do
  notFollowedBy beg
  _ <- lookAhead anyToken
  manyTill anyChar ((try $ lookAhead beg) <|> eof) >>= return . Script

parsePolyStat :: HemParser Stat
parsePolyStat = iteration
            <?> "multiline statement"

parseMonoStat :: HemParser Stat
parseMonoStat = beg >> monoStat >>= (\x -> end >> return x)

monoStat :: HemParser Stat
monoStat = template
       <|> var
       <?> "singular statement"

-- FIXME: notFollowedBy - use reserved instead
var :: HemParser Stat
var = notFollowedBy (string "end") >> genericVar >>= return . Var 

genericVar :: HemParser Ident
genericVar = do
  l <- letter
  rest <- many varAtom
  return (l:rest)

varAtom :: HemParser Char
varAtom = letter
      <|> digit
      <|> char '_'
      <?> "variable atom (letter, digit, underscore)"

-- Render "template <fileName>"
template :: HemParser Stat
template = do
  _ <- string "template"
  _ <- space
  fileName <- many1 filePathChar
  return $ Template fileName

iteration :: HemParser Stat
iteration = do
  beg
  _ <- string "iteration "
  ident <- genericVar
  end
  stats <- iterationBody
  return $ Iteration ident stats

iterationBody :: HemParser [Stat]
iterationBody = manyTill iterationStat (try iterationEnd)

iterationStat :: HemParser Stat
iterationStat =  try parseHemplate'
        <|> try iterationVar
        <?> "iteration statement"

iterationVar :: HemParser Stat
iterationVar = do
  beg
  key <- genericVar
  char '.'
  ident <- genericVar
  end
  return $ IterationVar key ident

iterationEnd :: HemParser ()
iterationEnd = beg >> string "end" >> end

filePathChar :: HemParser Char
filePathChar = satisfy (\c -> isAlphaNum c || c `elem` "/_-.")

beg :: HemParser ()
beg = char '{' >> char '-' >> space >> return ()

end :: HemParser ()
end = space >> char '-' >> char '}' >> return ()

runHemParser :: SourceName -> String -> IO (Either ParseError [Stat])
runHemParser = runParserT parseHemplate ()

parseFile :: FilePath -> IO [Stat]
parseFile fName = do
  fStream <- readFile fName
  parser  <- runHemParser fName fStream
  case parser of
    Left e  -> print e >> (exitWith $ ExitFailure 1)
    Right s -> return s
