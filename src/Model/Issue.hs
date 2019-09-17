{-# LANGUAGE RecordWildCards #-}

module Model.Issue where

import Data.Time.Calendar
import Data.Time.Format
import Data.Binary

data Issue = Issue {
    _issueName               :: String
  , _issuePriority           :: Int
  , _issueCreationDate       :: Day
  , _issueLastTrackTimestamp :: Int
  , _issueTimeRecorded       :: Int
  , _issueTrackingStatus     :: Bool
} deriving (Show)


instance Binary Issue where
  put issue = do
    put (_issueName issue)
    put (_issuePriority issue)
    put (show $ _issueCreationDate issue)
    put (_issueLastTrackTimestamp issue)
    put (_issueTimeRecorded issue)
    put (_issueTrackingStatus issue)
  get = do
    _issueName               <- get
    _issuePriority           <- get
    _issueCreationDate       <- get >>= parseTimeM True defaultTimeLocale "%Y-%-m-%-d"
    _issueLastTrackTimestamp <- get
    _issueTimeRecorded       <- get
    _issueTrackingStatus     <- get
    return Issue {..}
    
