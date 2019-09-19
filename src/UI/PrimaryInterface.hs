{-# LANGUAGE RecordWildCards #-}

module UI.PrimaryInterface
     ( buildMainContext
     , saveStateAndQuit
     ) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Data.IORef

import Control.Monad.Reader
import Control.Lens.Operators
import Model.Types(ContextIO, Message, InterfaceMainContext(..))
import Model.Project
import Model.Issue
import UI.ProjectInterfaceView
import UI.IssueInterfaceView
import UI.Controllers.IssueInterfaceController
import Model.TypesLenses
import System.Directory
import Data.Binary

buildMainContext :: Builder
                 -> TreeView
                 -> TreeView
                 -> IO InterfaceMainContext
buildMainContext gui _projectsView _issuesView = do
  (  _projectsStore
   , _sortedProjectsStore
   , _issuesStore
   , _sortedIssuesStore)  <- initStores _projectsView _issuesView
  _projectUiFieldsBundle  <- initProjectUiFieldBundle gui
  _issueUiFieldsBundle    <- initIssueUiFieldBundle gui
  _activeProject          <- newIORef Nothing
  _activeIssue            <- newIORef Nothing
  _trackedTimeStatusbar   <- builderGetObject gui castToStatusbar "issueTimeTrackedStatusbar"
  _notificationDialog     <- builderGetObject gui castToDialog "notificationDialog"
  _notificationStatusbar  <- builderGetObject gui castToStatusbar "notificationStatusbar"

  return InterfaceMainContext {..}
  
  
initStores :: TreeViewClass view
             => view
             -> view
             -> IO (ListStore Project,  TypedTreeModelSort Project, ListStore Issue, TypedTreeModelSort Issue)
initStores projectsView issuesView = do
  projectStore          <- initProjectStore
  sortedProjectsStore   <- View.treeModelSortNewWithModel projectStore
  issuesStore           <- storeImpl []
  sortedIssuesStore     <- View.treeModelSortNewWithModel issuesStore
  View.treeViewSetModel projectsView sortedProjectsStore
  View.treeViewSetModel issuesView sortedIssuesStore
  setupProjectsView projectsView projectStore sortedProjectsStore
  setupIssuesView issuesView issuesStore sortedIssuesStore
  return (projectStore, sortedProjectsStore, issuesStore, sortedIssuesStore)

initProjectStore = do
  previousStateFlag <- doesFileExist "projects.dat"
  if previousStateFlag then do
    projects <- decodeFile "projects.dat"
    updatedProjects <- mapM updateProject projects
    storeImpl updatedProjects
  else 
    storeImpl []

updateProject :: Project -> IO Project
updateProject project = do
  updatedIssues <- mapM updateIssueTiming (project^.projectIssues)
  return $ project & projectIssues .~ updatedIssues


saveStateAndQuit :: ContextIO ()
saveStateAndQuit = do
  context <- ask
  lift $ do
    projects <- View.listStoreToList (context^.projectsStore)
    encodeFile "projects.dat" projects
    mainQuit

storeImpl = View.listStoreNew
