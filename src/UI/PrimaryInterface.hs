{-# LANGUAGE RecordWildCards #-}

-- | This module provides functions for main
-- app context initialization.
module UI.PrimaryInterface
     ( buildMainContext
     , saveStateAndQuit
     ) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Data.IORef
import Data.Binary

import Control.Monad.Reader
import Control.Lens.Operators

import System.Directory

import Model.Types(ContextIO, InterfaceMainContext(..))
import Model.Project
import Model.Issue
import Model.TypesLenses
import UI.ProjectInterfaceView
import UI.IssueInterfaceView
import UI.Controllers.IssueInterfaceController


-- | Builds main app context used in
-- most of functions.
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
  
-- | Initialize projects and issues data stores.
initStores :: TreeViewClass view
             => view
             -> view
             -> IO (ListStore Project,  TypedTreeModelSort Project, ListStore Issue, TypedTreeModelSort Issue)
initStores prView issView = do
  prsStore       <- initProjectStore
  sortedPrsStore <- View.treeModelSortNewWithModel prsStore
  issStore       <- storeImpl []
  sortedIssStore <- View.treeModelSortNewWithModel issStore
  View.treeViewSetModel prView sortedPrsStore
  View.treeViewSetModel issView sortedIssStore
  setupProjectsView prView prsStore sortedPrsStore
  setupIssuesView issView issStore sortedIssStore
  return (prsStore, sortedPrsStore, issStore, sortedIssStore)

-- | Reads serialized app state if exists, otherwise
-- initialize app with empty projects state.
initProjectStore :: IO (ListStore Project)
initProjectStore = do
  previousStateFlag <- doesFileExist "projects.dat"
  if previousStateFlag
  then do
    projects <- decodeFile "projects.dat"
    updatedProjects <- mapM updateProject projects
    storeImpl updatedProjects
  else 
    storeImpl []

-- | Returns new store initialized with
-- given data.
storeImpl :: [a] -> IO (ListStore a)
storeImpl = View.listStoreNew

-- | Updates deserialized project timing.
updateProject :: Project -> IO Project
updateProject project = do
  updatedIssues <- mapM updateIssueTiming (project^.projectIssues)
  return $ project & projectIssues .~ updatedIssues

-- | Saves app state and quit.
saveStateAndQuit :: ContextIO ()
saveStateAndQuit = do
  context <- ask
  liftIO $ do
    projects <- View.listStoreToList (context^.projectsStore)
    encodeFile "projects.dat" projects
    mainQuit


