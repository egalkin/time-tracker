{-# LANGUAGE TemplateHaskell #-}

module Types where

import Graphics.UI.Gtk

import Data.IORef

import Control.Monad.Reader

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

type ContextIO a = ReaderT InterfaceMainContext IO a
type Message = String

data InterfaceMainContext = InterfaceMainContext {
  _projectsStore         :: ListStore Project,
  _projectUiFieldsBundle :: ProjectUiFieldsBundle,
  _issuesStore           :: TreeStore Issue,
  _issueUiFieldsBundle   :: IssueUiFieldsBundle,
  _activeProject         :: IORef (Maybe Int),
  _activeIssue           :: IORef (Maybe Int),
  _trackedTimeStatusbar  :: Statusbar,
  _notificationDialog    :: Dialog,
  _notificationStatusbar :: Statusbar
}


