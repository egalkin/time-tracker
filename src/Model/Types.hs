
-- | Module contains default types for interface runtime.
module Model.Types
     ( ContextIO
     , Message
     , ThreadType
     , InterfaceMainContext(..)
     , IssueUiFieldsBundle(..)
     , ProjectUiFieldsBundle(..)
     ) where

import Graphics.UI.Gtk

import Data.IORef
import Data.Time.Calendar
import Control.Monad.Reader

import Model.Project
import Model.Issue

-- | Type represents runtime context.
type ContextIO a = ReaderT InterfaceMainContext IO a

-- | Type represents message for notifications.
type Message = String

-- | Type show which thread have made gtk call.
data ThreadType = GtkThread | TimeHelperThread deriving (Eq)

-- | Type represents fields for 'Project' construction.
data ProjectUiFieldsBundle = ProjectUiFieldsBundle {
    _projectNameField :: Entry
}

-- | Type represents fields for 'Issue' construction.
data IssueUiFieldsBundle = IssueUiFieldsBundle {
    _issueNameField           :: Entry
  , _issuePriorityField       :: SpinButton
  , _issueTrackingStatusField :: CheckButton
}

-- | Type represents main interface context and store all 
-- necessary for application data.
data InterfaceMainContext = InterfaceMainContext {
    _projectsStore         :: ListStore Project
  , _projectsView          :: TreeView
  , _projectUiFieldsBundle :: ProjectUiFieldsBundle
  , _issuesStore           :: ListStore Issue
  , _issuesView             :: TreeView
  , _issueUiFieldsBundle   :: IssueUiFieldsBundle
  , _activeProject         :: IORef (Maybe Int)
  , _trackedTimeStatusbar  :: Statusbar
  , _notificationDialog    :: Dialog
  , _notificationStatusbar :: Statusbar
  , _sortedIssuesStore     :: TypedTreeModelSort Issue
}


