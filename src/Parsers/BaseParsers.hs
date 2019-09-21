
-- | Module provides some basics parsers.
module Parsers.BaseParsers
     ( creationDate
     , letterString
     , sc
     , word) where

import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

import Data.Time.Format
import Data.Time.Calendar

import Control.Applicative
import Control.Monad (void)


-- | Parse whitespaces.
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

-- | Parse word consisting of letters.
word :: Parser String 
word = do
  wr   <- some letterChar
  ws   <- many whitespace
  return $ wr ++ ws
  
-- | Parse whitespace.
-- Needed cause used megaparsec version
-- haven appropriate func.
whitespace :: Parser Char
whitespace = char ' ' <|> char '\t'

-- | Parse string with whitespaces.
letterString :: Parser String
letterString = concat <$> some word 

-- | Parse creation date.
creationDate :: Parser Day
creationDate = (some (try digitChar <|> char '-') >>= parseTimeM True defaultTimeLocale "%Y-%-m-%-d") <* sc

