{-# LANGUAGE RecordWildCards #-}

-- | Model to store issues data.

module Model.Issue
     ( Issue(..)
     ) where

import Data.Time.Calendar
import Data.Time.Format
import Data.Binary

-- | This type stores data about issue.
data Issue = Issue
  {  _issueName              :: String
  , _issuePriority           :: Int
  , _issueCreationDate       :: Day
  , _issueLastTrackTimestamp :: Int
  , _issueTimeTracked        :: Int
  , _issueDescription        :: String
  , _issueTrackingStatus     :: Bool
  }

-- | This instance used to provide serialization opportunity.
instance Binary Issue where
  put issue = do
    put (_issueName issue)
    put (_issuePriority issue)
    put (show $ _issueCreationDate issue)
    put (_issueLastTrackTimestamp issue)
    put (_issueTimeTracked issue)
    put (_issueDescription issue)
    put (_issueTrackingStatus issue)
  get = do
    _issueName               <- get
    _issuePriority           <- get
    _issueCreationDate       <- get >>= parseTimeM True defaultTimeLocale "%Y-%-m-%-d"
    _issueLastTrackTimestamp <- get
    _issueTimeTracked        <- get
    _issueDescription        <- get
    _issueTrackingStatus     <- get
    return Issue {..}
    
 