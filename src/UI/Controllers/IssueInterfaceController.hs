
-- | Module provides functions for "CRUD" actions with issues.
-- Helps display them on view and so on.
module UI.Controllers.IssueInterfaceController where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Data.IORef
import Data.Default

import Control.Monad.Reader
import Control.Lens.Operators

import Model.Types(ContextIO, ThreadType(..))
import Model.Issue
import Model.Project
import Model.TypesLenses
import UI.Notifications
import Utils.TimeUtils
import Utils.TrackedTimeUtils

-- | Update tracked time value and change track timestamp.
-- Used mostly for time-ticking effect.
updateIssueTiming :: Issue -> IO Issue
updateIssueTiming issue = do
  trackedTime <- countIssueTrackedSeconds issue
  timestamp   <- getSystemSeconds
  return $ issue { _issueTimeTracked = trackedTime, _issueLastTrackTimestamp = timestamp}

-- | Display all issues of current selected project.
displayIssues :: ThreadType -> ContextIO ()
displayIssues threadType = do
  context <- ask
  selection <- liftIO $ treeViewGetSelection (context^.projectsView)
  selectedRow <- liftIO $ treeSelectionGetSelected selection
  case selectedRow of
    Just iter -> do
                   issueIter <- liftIO $ treeModelSortConvertIterToChildIter (context^.sortedProjectsStore) iter
                   displayIssuesHelper threadType [listStoreIterToIndex issueIter]
    Nothing   -> return ()

-- | Updates and shows issues if currently selected project.
displayIssuesHelper :: ThreadType -> TreePath -> ContextIO ()
displayIssuesHelper threadType path = do
  context <- ask
  projectEntity <- liftIO $ View.listStoreGetValue (context^.projectsStore) (head path)
  liftIO $ do
    writeIORef (context^.activeProject) (Just $ head path)
    updatedIssues <- mapM updateIssueTiming (projectEntity^.projectIssues)
    let updatedProjectEntity = projectEntity & projectIssues .~ updatedIssues
    if threadType == TimeHelperThread then
      mapM_ (uncurry $ View.listStoreSetValue (context^.issuesStore)) (zip [0..] (updatedProjectEntity^.projectIssues))
    else do
      writeIORef (context^.activeIssue) Nothing
      View.listStoreClear (context^.issuesStore)
      mapM_ (View.listStoreAppend (context^.issuesStore)) (updatedProjectEntity^.projectIssues)
    View.listStoreSetValue (context^.projectsStore) (head path) updatedProjectEntity

-- | Writes information about current active issue.
writeActiveIssue :: ContextIO ()
writeActiveIssue = do
  context <- ask
  selection <- liftIO $ treeViewGetSelection (context^.issuesView)
  selectedRow <- liftIO $ treeSelectionGetSelected selection
  case selectedRow of
    Just iter -> liftIO $ do
      issueIter <- treeModelSortConvertIterToChildIter (context^.sortedIssuesStore) iter
      writeIORef (context^.activeIssue) (Just $ listStoreIterToIndex issueIter)
    Nothing   -> return ()

-- | Clear all issue of chosen project.
clearIssues :: ContextIO ()
clearIssues = do
  activeProjectRef <- asks (^.activeProject)
  actProject <- liftIO $ readIORef activeProjectRef
  case actProject of
    Just projectId -> clearIssueHelper projectId
    Nothing        -> showNoProjectChosen


-- | Clear issues view of chosen project.
clearIssueHelper :: Int -> ContextIO ()
clearIssueHelper projectId = do
  context <- ask
  liftIO $ do
    View.listStoreClear (context^.issuesStore)
    actProject <- View.listStoreGetValue (context^.projectsStore) projectId
    let newActiveProject = actProject & (projectIssues .~ [])
    View.listStoreSetValue (context^.projectsStore) projectId newActiveProject

-- | Removes chosen issue form project.
removeIssue :: ContextIO ()
removeIssue = do
  activeProjectRef <- asks (^.activeProject)
  actProject <- liftIO $ readIORef activeProjectRef
  case actProject of
    Just projectId -> removeIssueHelper projectId
    Nothing        -> showNoProjectChosen

-- | Removes chosen issue from project and view.
removeIssueHelper :: Int -> ContextIO ()
removeIssueHelper projectId = do
  context <- ask
  actIssue <- liftIO $ readIORef (context^.activeIssue)
  case actIssue of
    Just issueId -> liftIO $ do
      View.listStoreRemove (context^.issuesStore) issueId
      newIssues <- View.listStoreToList (context^.issuesStore)
      actProject <- View.listStoreGetValue (context^.projectsStore) projectId
      let newActiveProject = actProject & (projectIssues .~ newIssues)
      View.listStoreSetValue (context^.projectsStore) projectId newActiveProject
    Nothing      -> showNoIssueChosen

-- | Set information in dialog about issue on double click.
showIssue :: Issue -> ContextIO ()
showIssue issue = do
  fieldsBundle <- asks (^.issueUiFieldsBundle)
  liftIO $ do
    entrySetText (fieldsBundle^.issueNameField) (issue^.issueName)
    spinButtonSetValue (fieldsBundle^.issuePriorityField) (fromIntegral $ issue^.issuePriority)
    toggleButtonSetActive (fieldsBundle^.issueTrackingStatusField) (issue^.issueTrackingStatus)
    textViewGetBuffer (fieldsBundle^.issueDescriptionField) >>= flip textBufferSetText (issue^.issueDescription)

-- | Set issue fields in dialog to default.
resetIssueFieldsState :: ContextIO ()
resetIssueFieldsState = do
  fieldsBundle <- asks (^.issueUiFieldsBundle)
  liftIO $ do
    entrySetText (fieldsBundle^.issueNameField) (def :: String)
    spinButtonSetValue (fieldsBundle^.issuePriorityField) (def :: Double)
    toggleButtonSetActive (fieldsBundle^.issueTrackingStatusField) True
    textViewGetBuffer (fieldsBundle^.issueDescriptionField) >>= flip textBufferSetText (def :: String)

-- | Displays information about active project chosen issue.
-- Activate on issue double-click.
displayIssueInformation :: Dialog -> Button -> TreePath -> ContextIO ()
displayIssueInformation dialog btn path = do
  context <- ask
  actProject <- liftIO $ readIORef (context^.activeProject)
  liftIO $ buttonSetLabel btn "Update"
  case actProject of
    Just project -> showAndUpdateIssue dialog project path
    Nothing      -> showNoProjectChosen

-- | Shows information issue and performs it update.
showAndUpdateIssue :: Dialog -> Int -> TreePath -> ContextIO ()
showAndUpdateIssue dialog project path = do
  issStore   <- asks (^.issuesStore)
  issue      <- liftIO $ View.listStoreGetValue issStore (head path)
  showIssue issue
  resp       <-  liftIO $ dialogRun dialog
  case resp of
    ResponseAccept -> updateIssue issue >>= updateIssueHelper project path
    _              -> return ()
  liftIO $ widgetHide dialog
  resetIssueFieldsState

-- | Update issue information.
updateIssue :: Issue -> ContextIO (Either String Issue)
updateIssue issue = do
  fieldsBundle <- asks (^.issueUiFieldsBundle)
  updatedIssue <- constructIssue
  liftIO $ do
    name              <- entryGetText $ fieldsBundle^.issueNameField
    trackedTime       <- countIssueTrackedSeconds issue
    timestamp         <- getSystemSeconds
    case name of
      [] -> return $ Left "Issue's name can't be empty"
      _  -> return $ Right $ updatedIssue
              { _issueName               = name
              , _issueTimeTracked        = trackedTime
              , _issueCreationDate       = issue^.issueCreationDate
              , _issueLastTrackTimestamp = timestamp
              }

-- | Update information about issue in view.
updateIssueHelper :: Int -> TreePath -> Either String Issue -> ContextIO ()
updateIssueHelper project path issue = do
  context <- ask
  case issue of
    Right iss -> liftIO $ do
      actProject <- View.listStoreGetValue (context^.projectsStore)  project
      updatedIssue <- updateIssueTiming iss
      View.listStoreSetValue (context^.issuesStore) (head path) updatedIssue
      updatedIssues <- View.listStoreToList (context^.issuesStore)
      let newActiveProject = actProject & (projectIssues .~ updatedIssues)
      View.listStoreSetValue (context^.projectsStore) project newActiveProject
    Left err  -> showCustomNotification err

-- | Add new issue to active project.
addIssue :: Dialog -> Button -> ContextIO ()
addIssue dialog btn= do
  context <- ask
  actProject <- liftIO $ readIORef (context^.activeProject)
  liftIO $ buttonSetLabel btn "Create"
  case actProject of
    Just project -> addIssueToProject dialog project
    Nothing      -> showNoProjectChosen

-- | Construct new issue and add it to active project.
addIssueToProject :: Dialog -> Int -> ContextIO ()
addIssueToProject dialog project = do
  context       <- ask
  actProject    <- liftIO $ View.listStoreGetValue (context^.projectsStore) project
  liftIO $ widgetShow dialog
  resp <-  liftIO $ dialogRun dialog
  case resp of
    ResponseAccept -> buildIssue >>= addIssueHelper actProject project
    _              -> return ()
  liftIO $ widgetHide dialog

buildIssue :: ContextIO (Either String Issue)
buildIssue = do
  fieldsBundle <- asks (^.issueUiFieldsBundle)
  partBuiltIssue <- constructIssue
  liftIO $ do
    name           <- entryGetText $ fieldsBundle^.issueNameField
    case name of
      [] -> return $ Left "Issue's name can't be empty"
      _  -> return $ Right partBuiltIssue {
                _issueName               = name
            }

-- | Add new issue and displays it in view.
addIssueHelper :: Project -> Int -> Either String Issue -> ContextIO ()
addIssueHelper activeRow project issue = do
  context <- ask
  case issue of
    Right iss -> liftIO $ do
      let newActiveRow = activeRow & (projectIssues %~ (iss :))
      View.listStoreInsert (context^.issuesStore) 0 iss
      View.listStoreSetValue (context^.projectsStore) project newActiveRow
    Left err  -> showCustomNotification err

-- | Gets issue description from 'TextView'
getIssueDescriptionText :: TextView -> IO String
getIssueDescriptionText textView = do
  buffer <- textViewGetBuffer textView
  start  <- textBufferGetStartIter buffer
  end    <- textBufferGetEndIter buffer
  textBufferGetText buffer start end True

-- | Initialize default issues fields.
constructIssue :: ContextIO Issue
constructIssue = do
  fieldsBundle <- asks (^.issueUiFieldsBundle)
  liftIO $ do
    priority       <- round <$> spinButtonGetValue (fieldsBundle^.issuePriorityField)
    trackingStatus <- toggleButtonGetActive $ fieldsBundle^.issueTrackingStatusField
    description    <- getIssueDescriptionText (fieldsBundle^.issueDescriptionField)
    creationDate   <- getCurrentDate
    timestamp      <- getSystemSeconds
    return $ Issue 
           { _issueName               = ""
           , _issuePriority           = priority
           , _issueCreationDate       = creationDate
           , _issueLastTrackTimestamp = timestamp
           , _issueTimeTracked        = 0
           , _issueTrackingStatus     = trackingStatus
           , _issueDescription        = description
           }
