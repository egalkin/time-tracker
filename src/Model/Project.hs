{-# LANGUAGE RecordWildCards #-}

-- | Model to store projects data.
module Model.Project 
     ( Project(..)
     ) where

import Data.Binary
import Data.Time.Calendar
import Data.Time.Format

import Model.Issue


-- | This type stores data about project.
data Project = Project
  { _projectName         :: String
  , _projectCreationDate :: Day
  , _projectIssues       :: [Issue]
  } 

-- | This instance used to provide serialization opportunity.
instance Binary Project where
  put project = do
    put (_projectName project)
    put (show $ _projectCreationDate project)
    put (_projectIssues project)
  get = do
    _projectName         <- get
    _projectCreationDate <- get >>= parseTimeM True defaultTimeLocale "%Y-%-m-%-d"
    _projectIssues       <- get
    return Project {..}
