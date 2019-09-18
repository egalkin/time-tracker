module UI.Controllers.IssueInterfaceController where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Model.Types(ContextIO, ThreadType(..))
import Model.Issue
import Model.Project
import Model.TypesLenses
import Control.Lens.Operators

import Data.IORef
import Control.Monad.Reader

import UI.Notifications
import Utils.TimeUtils
import Model.TrackedTime

updateIssue :: Issue -> IO Issue
updateIssue issue = do
  trackedTime <- countIssueTrackedSeconds issue
  timestamp   <- getSystemSeconds
  return $ issue { _issueTimeRecorded = trackedTime, _issueLastTrackTimestamp = timestamp}

displayIssues2 :: ThreadType -> TreePath -> ContextIO ()
displayIssues2 threadType path = do
  context <- ask
  projectEntity <- lift $ View.listStoreGetValue (context^.projectsStore) (head path)
  lift $ do
    writeIORef (context^.activeProject) (Just $ head path)
    updatedIssues <- mapM updateIssue (projectEntity^.projectIssues)
    let updatedProjectEntity = projectEntity & projectIssues .~ updatedIssues
    if threadType == TimeHelperThread then
      mapM_ (uncurry $ View.listStoreSetValue (context^.issuesStore)) (zip [0..] (updatedProjectEntity^.projectIssues))
    else do 
      View.listStoreClear (context^.issuesStore)
      mapM_ (View.listStoreAppend (context^.issuesStore)) (updatedProjectEntity^.projectIssues)
    View.listStoreSetValue (context^.projectsStore) (head path) updatedProjectEntity

displayIssues :: ThreadType -> ContextIO ()    
displayIssues threadType = do
  projectsView <- asks (^.projectsView)
  selection <- lift $ treeViewGetSelection projectsView
  selectedRow <- lift $ treeSelectionGetSelected selection
  case selectedRow of
    Just iter -> displayIssues2 threadType [listStoreIterToIndex iter]
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


addIssueHelper :: Project -> Int -> Either String Issue -> ContextIO ()
addIssueHelper activeRow project issue = do
  context <- ask
  case issue of 
    Right iss -> lift $ do
                          let newActiveRow = activeRow & (projectIssues %~ (iss :))
                          View.listStoreClear (context^.issuesStore)
                          mapM_ (View.listStoreAppend (context^.issuesStore)) (newActiveRow^.projectIssues)
                          View.listStoreSetValue (context^.projectsStore) project newActiveRow
    Left err  -> showCustomNotification err


buildIssue :: ContextIO (Either String Issue)
buildIssue = do
  context          <- ask
  lift $ do
    name           <- entryGetText $ context^.issueUiFieldsBundle.issueNameField
    priority       <- fromIntegral.round <$> spinButtonGetValue (context^.issueUiFieldsBundle.issuePriorityField)
    creationDate   <- getCurrentDate
    timestamp      <- getSystemSeconds
    trackingStatus <- toggleButtonGetActive $ context^.issueUiFieldsBundle.issueTrackingStatusField
    
    case name of
      []  -> return $ Left "Issue's name can't be empty"
      str -> return $ Right Issue {
              _issueName               = name,
              _issuePriority           = priority,
              _issueCreationDate       = creationDate,
              _issueLastTrackTimestamp = timestamp,
              _issueTimeRecorded       = 0,
              _issueTrackingStatus     = trackingStatus
            }