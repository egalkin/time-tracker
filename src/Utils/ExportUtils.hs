{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module provides show instances for state exporting.
module Utils.ExportUtils where

import Data.List

import Model.Issue
import Model.Project
import Utils.TrackedTimeUtils

instance Show Project where
  show project = 
    _projectName project ++ 
    "|" ++ 
    show (_projectCreationDate project) ++ ":\n" ++
    concatMap (\is -> "#" ++ show is ++ "\n") (_projectIssues project)

instance Show Issue where
  show issue = intercalate "|"
    [ _issueName issue
    , show (_issuePriority issue)
    , show (_issueCreationDate issue)
    , show $ convertSecondsToTrackedTime (_issueTimeTracked issue)
    , show (_issueTrackingStatus issue)
    , "["++ _issueDescription issue ++"]"
    ]
