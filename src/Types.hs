{-# LANGUAGE TemplateHaskell #-}

module Types where

import Graphics.UI.Gtk
import Control.Lens

import Data.Semigroup
import Data.IORef

import Data.Time.Calendar

data Project = Project {
    _projectName         :: String
  , _projectCreationDate :: Day
  , _projectTimeRecorded :: Int
  , _projectIssues       :: [Issue]
} deriving (Show)

data ProjectUiFieldsBundle = ProjectUiFieldsBundle {
    _projectNameField :: Entry
}

data Issue = Issue {
    _issueName               :: String
  , _issuePriority           :: Int
  , _issueCreationDate       :: Day
  , _issueLastTrackTimestamp :: Int
  , _issueTimeRecorded       :: Int
  , _issueTrackingStatus     :: Bool
} deriving (Show)

data IssueUiFieldsBundle = IssueUiFieldsBundle {
    _issueNameField           :: Entry
  , _issuePriorityField       :: SpinButton
  , _issueTrackingStatusField :: CheckButton
}

data TrackedTime = TrackedTime {
    _hours   :: Int
  , _minutes :: Int
}

instance Show TrackedTime where
  show (TrackedTime hr mi) = show hr <> "::" <> show mi


data InterfaceMainContext = InterfaceMainContext {
  _projectsStore         :: ListStore Project,
  _projectUiFieldsBundle :: ProjectUiFieldsBundle,
  _issuesStore           :: TreeStore Issue,
  _issueUiFieldsBundle   :: IssueUiFieldsBundle,
  _activeProject         :: IORef Int,
  _activeIssue           :: IORef Int,
  _trackedTimeStatusbar  :: Entry
}

makeLenses ''Project
makeLenses ''ProjectUiFieldsBundle
makeLenses ''Issue
makeLenses ''IssueUiFieldsBundle
makeLenses ''InterfaceMainContext
makeLenses ''TrackedTime

trackedTimeToInt :: TrackedTime -> Int
trackedTimeToInt time = time^.hours * 3600 + time^.minutes * 60
