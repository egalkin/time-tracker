{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module IssueParser 
  ( parseIssues
  ) where


import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

import Data.Void

import Data.Text (Text)

import Data.Time.Format
import Data.Time.Calendar

import Control.Applicative
import Control.Monad (void)

import Types

import Data.Time.Clock.System

parseIssues :: FilePath -> IO [Either (ParseError Char Dec) Issue]
parseIssues path = do
  contents <- readFile path
  return $ map (parse issue "") (lines contents)

issue :: Parser Issue
issue = do
  sc
  _issueName               <- name
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


sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty


name :: Parser String
name = some letterChar <* sc

priority :: Parser Int
priority = fromIntegral <$> L.integer <* sc

creationDate :: Parser Day
creationDate = (some (try digitChar <|> char '-') >>= parseTimeM True defaultTimeLocale "%Y-%-m-%-d") <* sc

timeTracked :: Parser TrackedTime
timeTracked = do
  _hours   <- fromIntegral <$> L.integer
  char ':'
  _minutes <- mins

  TrackedTime {..} <$ sc

mins :: Parser Int
mins = try withLeadingZero <|> withoutLeadingZero
  where
    withLeadingZero    = read . (: []) <$> (char '0' *> digitChar)
    withoutLeadingZero = fromIntegral <$> L.integer

trackingStatus :: Parser Bool
trackingStatus =  read <$> choice [string "True", string "False"] <* sc

