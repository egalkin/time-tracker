{-# LANGUAGE RecordWildCards #-}

module Model.Project where

import Data.Binary
import Data.Time.Calendar
import Data.Time.Format
import Model.Issue

data Project = Project {
    _projectName         :: String
  , _projectCreationDate :: Day
  , _projectTimeRecorded :: Int
  , _projectIssues       :: [Issue]
} deriving (Show)


instance Binary Project where
  put project = do
    put (_projectName project)
    put (show $ _projectCreationDate project)
    put (_projectTimeRecorded project)
    put (_projectIssues project)
  get = do
    _projectName         <- get
    _projectCreationDate <- get >>= parseTimeM True defaultTimeLocale "%Y-%-m-%-d"
    _projectTimeRecorded <- get
    _projectIssues       <- get
    return Project {..}

