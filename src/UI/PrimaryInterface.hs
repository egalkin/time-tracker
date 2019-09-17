module UI.PrimaryInterface
     ( buildMainContext
     , saveStateAndQuit
     ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import Data.IORef

import Control.Monad.Reader
import Control.Lens.Operators
import Model.Types(ContextIO, Message, InterfaceMainContext(..))
import Model.Project
import Model.Issue
import UI.ProjectInterfaceView
import UI.IssueInterfaceView
import Model.TypesLenses
import System.Directory
import Data.Binary

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
  projectStore      <- initProjectStore
  issuesStore       <- issuesStoreImpl
  sortedIssuesStore <- View.treeModelSortNewWithModel issuesStore
  View.treeViewSetModel projectsView projectStore
  View.treeViewSetModel issuesView sortedIssuesStore
  setupProjectsView projectsView projectStore
  setupIssuesView issuesView issuesStore sortedIssuesStore
  return (projectStore, issuesStore)  

initProjectStore = do
  previousStateFlag <- doesFileExist "projects.dat"
  if previousStateFlag then do
    projects <- decodeFile "projects.dat"
    projectsStoreImpl projects
  else 
    projectsStoreImpl []

saveStateAndQuit :: ContextIO ()
saveStateAndQuit = do
  context <- ask
  lift $ do
    projects <- View.listStoreToList (context^.projectsStore)
    encodeFile "projects.dat" projects
    mainQuit

projectsStoreImpl = View.listStoreNew

issuesStoreImpl = View.treeStoreNew []