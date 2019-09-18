module Model.Types where

import Graphics.UI.Gtk

import Data.IORef
import Data.Binary

import Control.Monad.Reader

import Data.Time.Calendar

import Model.Project
import Model.Issue

type ContextIO a = ReaderT InterfaceMainContext IO a
type Message = String

data ThreadType = GtkThread | TimeHelperThread deriving (Eq)

data ProjectUiFieldsBundle = ProjectUiFieldsBundle {
    _projectNameField :: Entry
}

data IssueUiFieldsBundle = IssueUiFieldsBundle {
    _issueNameField           :: Entry
  , _issuePriorityField       :: SpinButton
  , _issueTrackingStatusField :: CheckButton
}

data InterfaceMainContext = InterfaceMainContext {
  _projectsStore         :: ListStore Project,
  _projectsView          :: TreeView,
  _projectUiFieldsBundle :: ProjectUiFieldsBundle,
  _issuesStore           :: ListStore Issue,
  _issuesView             :: TreeView,
  _issueUiFieldsBundle   :: IssueUiFieldsBundle,
  _activeProject         :: IORef (Maybe Int),
  _trackedTimeStatusbar  :: Statusbar,
  _notificationDialog    :: Dialog,
  _notificationStatusbar :: Statusbar,
  _sortedIssuesStore     :: TypedTreeModelSort Issue
}


