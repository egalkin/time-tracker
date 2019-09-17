module PrimaryInterface
     ( buildMainContext
     ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import Data.IORef

import Control.Monad.Reader
import Control.Lens.Operators
import Types(ContextIO, Message, InterfaceMainContext(..), Project, Issue)
import ProjectInterfaceView
import IssueInterfaceView
import TypesLenses

buildMainContext :: Builder
                 -> TreeView
                 -> TreeView
                 -> IO InterfaceMainContext
buildMainContext gui projectsView issuesView = do
  (projectsStore, issuesStore) <- initStores projectsView issuesView
  projectUiFieldsBundle        <- initProjectUiFieldBundle gui
  issueUiFieldsBundle          <- initIssueUiFieldBundle gui
  activeProject                <- newIORef Nothing
  activeIssue                  <- newIORef Nothing
  trackedTimeStatusbar         <- builderGetObject gui castToStatusbar "issueTimeTrackedStatusbar"
  notificationDialog           <- builderGetObject gui castToDialog "notificationDialog"
  notificationStatusbar        <- builderGetObject gui castToStatusbar "notificationStatusbar"

  return InterfaceMainContext {
    _projectsStore = projectsStore,
    _projectUiFieldsBundle = projectUiFieldsBundle,
    _issuesStore = issuesStore,
    _issueUiFieldsBundle = issueUiFieldsBundle,
    _activeProject = activeProject,
    _activeIssue = activeIssue,
    _trackedTimeStatusbar = trackedTimeStatusbar,
    _notificationDialog   = notificationDialog,
    _notificationStatusbar = notificationStatusbar
  }
  
  
initStores :: TreeViewClass view
             => view
             -> view
             -> IO (View.ListStore Project, View.TreeStore Issue)
initStores projectsView issuesView = do
  projectStore      <- projectsStoreImpl
  issuesStore       <- issuesStoreImpl
  sortedIssuesStore <- View.treeModelSortNewWithModel issuesStore
  View.treeViewSetModel projectsView projectStore
  View.treeViewSetModel issuesView sortedIssuesStore
  setupProjectsView projectsView projectStore
  setupIssuesView issuesView issuesStore sortedIssuesStore
  return (projectStore, issuesStore)  


projectsStoreImpl = View.listStoreNew []

issuesStoreImpl = View.treeStoreNew []