module Parsers.BaseParsers
     ( creationDate
     , letterString
     , sc
     , word) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

import Data.Time.Format
import Data.Time.Calendar

import Control.Applicative
import Control.Monad (void)

import Model.Issue
import Model.TrackedTime
import Utils.TrackedTimeUtils(trackedTimeToInt)

import Data.Time.Clock.System

-- | Parse whitespaces.
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

-- | Parse word consisting of letters.
word :: Parser String 
word = do
  word <- some letterChar 
  ws   <- many $ char ' ' 
  return $ word ++ ws
  
-- | Parse string with whitespaces.
letterString :: Parser String
letterString = concat <$> some word 

-- | Parse creation date.
creationDate :: Parser Day
creationDate = (some (try digitChar <|> char '-') >>= parseTimeM True defaultTimeLocale "%Y-%-m-%-d") <* sc
