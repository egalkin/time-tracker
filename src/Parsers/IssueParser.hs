{-# LANGUAGE RecordWildCards #-}

-- | Module provides parser for issue parsing.
module Parsers.IssueParser 
  ( parseIssues
  , parseProjectIssues
  , projectIssue
  ) where

import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

import Control.Monad(void)

import Model.Issue
import Model.TrackedTime
import Utils.TrackedTimeUtils(trackedTimeToInt)
import Parsers.BaseParsers



-- | Parse issue from file.
-- Returns list of Either with parsed 'Issue' instances and errors.
parseIssues :: FilePath -> IO [Either (ParseError Char Dec) Issue]
parseIssues path = do
  contents <- readFile path
  return $ map (parse issue "") (lines contents)

-- | Parse project style issue.
projectIssue :: Parser Issue
projectIssue = char '#' *> sc *> issue <* some newline

-- | Parse projects style issues.
parseProjectIssues :: Parser [Issue]
parseProjectIssues = many projectIssue

-- | Parse issue string representation.
issue :: Parser Issue
issue = do
  sc
  _issueName               <- letterString 
  void $ char '|' 
  _issuePriority           <- priority
  void $ char '|'
  _issueCreationDate       <- creationDate
  let _issueLastTrackTimestamp = 0
  void $ char '|'
  _issueTimeTracked       <- trackedTimeToInt <$> timeTracked
  void $ char '|'
  _issueTrackingStatus     <- trackingStatus
  void $ char '|'
  _issueDescription        <- description
  return Issue {..}


-- | Parse issue's priority.
priority :: Parser Int
priority = fromIntegral <$> L.integer <* sc

-- | Parse issue's time tracked.
timeTracked :: Parser TrackedTime
timeTracked = do
  _hours   <- fromIntegral <$> L.integer
  void $ char ':'
  _minutes <- minsOrSecs
  void $ char ':'
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

-- | Parse issues description
description :: Parser String
description = char '[' *> many (noneOf  ['[', ']']) <* char ']'
