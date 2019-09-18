{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module provides parser combinators for issue parsing.
module Parsers.IssueParser 
  ( parseIssues
  ) where

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

-- | Parse issue from string.
-- Returns list of Either with parsed 'Issue' instances and errors.
parseIssues :: FilePath -> IO [Either (ParseError Char Dec) Issue]
parseIssues path = do
  contents <- readFile path
  return $ map (parse issue "") (lines contents)

-- | Parse issue string representation.
issue :: Parser Issue
issue = do
  sc
  _issueName               <- concat <$> name
  char '|'
  _issuePriority           <- priority
  char '|'
  _issueCreationDate       <- creationDate
  let _issueLastTrackTimestamp = 0
  char '|'
  _issueTimeRecorded       <- trackedTimeToInt <$> timeTracked
  char '|'
  _issueTrackingStatus     <- trackingStatus
  return Issue {..}

-- | Parse whitespaces.
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

-- | Parse word consisting of letters.
word :: Parser String 
word = do
  word <- some letterChar 
  ws   <- many $ char ' ' 
  return $ word ++ ws

-- | Parse issue's name.
name :: Parser [String]
name = some word

-- | Parse issue's priority.
priority :: Parser Int
priority = fromIntegral <$> L.integer <* sc

-- | Parse issue's creation date.
creationDate :: Parser Day
creationDate = (some (try digitChar <|> char '-') >>= parseTimeM True defaultTimeLocale "%Y-%-m-%-d") <* sc

-- | Parse issue's time tracked.
timeTracked :: Parser TrackedTime
timeTracked = do
  _hours   <- fromIntegral <$> L.integer
  char ':'
  _minutes <- minsOrSecs
  char ':'
  _seconds <- minsOrSecs

  TrackedTime {..} <$ sc

-- | Parse minutes and seconds string representation.
minsOrSecs :: Parser Int
minsOrSecs = try withLeadingZero <|> withoutLeadingZero
  where
    withLeadingZero    = read . (: []) <$> (char '0' *> digitChar)
    withoutLeadingZero = do 
      fDigit <- satisfy (\c -> c >='0' && c <= '5')
      sDigit <- digitChar
      return $ read [fDigit, sDigit]
      
-- | Parse issues track status.
trackingStatus :: Parser Bool
trackingStatus =  read <$> choice [string "True", string "False"] <* sc

