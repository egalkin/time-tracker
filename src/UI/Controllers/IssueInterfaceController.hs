module UI.Controllers.IssueInterfaceController where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Model.Types(ContextIO)
import Model.Issue
import Model.Project
import Model.TypesLenses
import Control.Lens.Operators
import UI.PrimaryInterface

import Data.IORef
import Control.Monad.Reader

import UI.Notifications
import Utils.TimeUtils

displayIssues :: TreePath -> ContextIO ()
displayIssues path  = do
  context <- ask
  projectEntity <- lift $ View.listStoreGetValue (context^.projectsStore) (head path)
  lift $ do
    View.listStoreClear (context^.issuesStore)
    writeIORef (context^.activeProject) (Just $ head path)
    writeIORef (context^.activeIssue) Nothing
    mapM_ (View.listStoreAppend (context^.issuesStore)) (projectEntity^.projectIssues)
    
displayIssues2 :: TreeView -> ContextIO ()    
displayIssues2 view = do
  context <- ask
  selection <- lift $ treeViewGetSelection view
  selectedRow <- lift $ treeSelectionGetSelected selection
  case selectedRow of
    Just iter -> displayIssues [listStoreIterToIndex iter]
    Nothing   -> return ()
    
writeCurrentIssue :: TreeView -> ContextIO ()
writeCurrentIssue view = do
  context <- ask
  selection <- lift $ treeViewGetSelection view
  selectedRow <- lift $ treeSelectionGetSelected selection
  case selectedRow of
    Just iter -> do
                   path <- lift $ View.treeModelSortConvertIterToChildIter (context^.sortedIssuesStore) iter
                   lift $ writeIORef (context^.activeIssue) (Just $ listStoreIterToIndex path)
    Nothing   -> return ()




addIssueToProject :: Dialog -> Int -> ContextIO ()
addIssueToProject dialog project = do
  context       <- ask
  activeRow     <- lift $ View.listStoreGetValue (context^.projectsStore) project
  lift $ widgetShow dialog
  response <-  lift $ dialogRun dialog
  case response of
    ResponseAccept -> buildIssue >>= addIssueHelper activeRow project
    _              -> return ()
  lift $ widgetHide dialog

addIssue :: Dialog -> ContextIO ()
addIssue dialog  = do
  context <- ask
  activeProject <- lift $ readIORef (context^.activeProject)
  case activeProject of
    Just project -> addIssueToProject dialog project
    Nothing      -> showNoProjectChosen

addIssueHelper :: Project -> Int -> Issue -> ContextIO ()
addIssueHelper activeRow project issue = do
  context <- ask
  let newActiveRow = activeRow & (projectIssues %~ (issue :))
  lift $ do
   View.listStoreClear (context^.issuesStore)
   mapM_ (View.listStoreAppend (context^.issuesStore)) (newActiveRow^.projectIssues)
   View.listStoreSetValue (context^.projectsStore) project newActiveRow

buildIssue :: ContextIO Issue
buildIssue = do
  context          <- ask
  lift $ do
    name           <- entryGetText $ context^.issueUiFieldsBundle.issueNameField
    priority       <- fromIntegral.round <$> spinButtonGetValue (context^.issueUiFieldsBundle.issuePriorityField)
    creationDate   <- getCurrentDate
    timestamp      <- getSystemSeconds
    trackingStatus <- toggleButtonGetActive $ context^.issueUiFieldsBundle.issueTrackingStatusField

    return Issue {
      _issueName               = name,
      _issuePriority           = priority,
      _issueCreationDate       = creationDate,
      _issueLastTrackTimestamp = timestamp,
      _issueTimeRecorded       = 0,
      _issueTrackingStatus     = trackingStatus
    }