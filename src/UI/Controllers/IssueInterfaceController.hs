module UI.Controllers.IssueInterfaceController where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Model.Types(ContextIO, ThreadType(..))
import Model.Issue
import Model.Project
import Model.TypesLenses
import Control.Lens.Operators

import Data.IORef
import Data.Default
import Control.Monad.Reader

import UI.Notifications
import Utils.TimeUtils
import Utils.TrackedTimeUtils(countIssueTrackedSeconds, countIssueTrackedSecondsIgnoringStatus)
import Model.TrackedTime

updateIssueTiming :: Issue -> IO Issue
updateIssueTiming issue = do
  trackedTime <- countIssueTrackedSeconds issue
  timestamp   <- getSystemSeconds
  return $ issue { _issueTimeRecorded = trackedTime, _issueLastTrackTimestamp = timestamp}

displayIssuesHelper :: ThreadType -> TreePath -> ContextIO ()
displayIssuesHelper threadType path = do
  context <- ask
  projectEntity <- lift $ View.listStoreGetValue (context^.projectsStore) (head path)
  lift $ do
    writeIORef (context^.activeProject) (Just $ head path)
    updatedIssues <- mapM updateIssueTiming (projectEntity^.projectIssues)
    let updatedProjectEntity = projectEntity
    let updatedProjectEntity = projectEntity & projectIssues .~ updatedIssues
    if threadType == TimeHelperThread then
      mapM_ (uncurry $ View.listStoreSetValue (context^.issuesStore)) (zip [0..] (updatedProjectEntity^.projectIssues))
    else do
      View.listStoreClear (context^.issuesStore)
      mapM_ (View.listStoreAppend (context^.issuesStore)) (updatedProjectEntity^.projectIssues)
    View.listStoreSetValue (context^.projectsStore) (head path) updatedProjectEntity

writeActiveIssue :: ContextIO ()
writeActiveIssue = do
  context <- ask
  selection <- lift $ treeViewGetSelection (context^.issuesView)
  selectedRow <- lift $ treeSelectionGetSelected selection
  case selectedRow of
    Just iter -> lift $ do
                   issueIter <- treeModelSortConvertIterToChildIter (context^.sortedIssuesStore) iter
                   writeIORef (context^.activeIssue) (Just $ listStoreIterToIndex issueIter)
    Nothing   -> return ()

clearIssueHelper :: Int -> ContextIO ()
clearIssueHelper projectId = do
  context <- ask
  activeIssue <- lift $ readIORef (context^.activeIssue)
  lift $ do
    View.listStoreClear (context^.issuesStore)
    activeProject <- View.listStoreGetValue (context^.projectsStore) projectId
    let newActiveProject = activeProject & (projectIssues .~ [])
    View.listStoreSetValue (context^.projectsStore) projectId newActiveProject


clearIssues :: ContextIO ()
clearIssues = do
  activeProjectRef <- asks (^.activeProject)
  activeProject <- lift $ readIORef activeProjectRef
  case activeProject of
    Just projectId -> clearIssueHelper projectId
    Nothing        -> showNoProjectChosen

removeIssueHelper :: Int -> ContextIO ()
removeIssueHelper projectId = do
  context <- ask
  activeIssue <- lift $ readIORef (context^.activeIssue)
  case activeIssue of
    Just issueId -> lift $ do
                             View.listStoreRemove (context^.issuesStore) issueId
                             newIssues <- View.listStoreToList (context^.issuesStore)
                             activeProject <- View.listStoreGetValue (context^.projectsStore) projectId
                             let newActiveProject = activeProject & (projectIssues .~ newIssues)
                             View.listStoreSetValue (context^.projectsStore) projectId newActiveProject
    Nothing      -> showNoIssueChosen

removeIssue :: ContextIO ()
removeIssue = do
  activeProjectRef <- asks (^.activeProject)
  activeProject <- lift $ readIORef activeProjectRef
  case activeProject of
    Just projectId -> removeIssueHelper projectId
    Nothing        -> showNoProjectChosen

displayIssues :: ThreadType -> ContextIO ()
displayIssues threadType = do
  context <- ask
  selection <- lift $ treeViewGetSelection (context^.projectsView)
  selectedRow <- lift $ treeSelectionGetSelected selection
  case selectedRow of
    Just iter -> do
                   issueIter <- lift $ treeModelSortConvertIterToChildIter (context^.sortedProjectsStore) iter
                   displayIssuesHelper threadType [listStoreIterToIndex issueIter]
    Nothing   -> return ()

showIssue :: Issue -> ContextIO ()
showIssue issue = do
  fieldsBundle <- asks (^.issueUiFieldsBundle)
  lift $ do
    entrySetText (fieldsBundle^.issueNameField) (issue^.issueName)
    spinButtonSetValue (fieldsBundle^.issuePriorityField) (fromIntegral $ issue^.issuePriority)
    toggleButtonSetActive (fieldsBundle^.issueTrackingStatusField) (issue^.issueTrackingStatus)
    textViewGetBuffer (fieldsBundle^.issueDescriptionField) >>= flip textBufferSetText (issue^.issueDescription)

resetIssueFieldsState :: ContextIO ()
resetIssueFieldsState = do
  fieldsBundle <- asks (^.issueUiFieldsBundle)
  lift $ do
    entrySetText (fieldsBundle^.issueNameField) (def :: String)
    spinButtonSetValue (fieldsBundle^.issuePriorityField) (def :: Double)
    toggleButtonSetActive (fieldsBundle^.issueTrackingStatusField) True
    textViewGetBuffer (fieldsBundle^.issueDescriptionField) >>= flip textBufferSetText (def :: String)

showAndUpdateIssue :: Dialog -> Int -> TreePath -> ContextIO ()
showAndUpdateIssue dialog project path = do
  projectStore  <- asks (^.projectsStore)
  issuesStore   <- asks (^.issuesStore)
  issue         <- lift $ View.listStoreGetValue issuesStore (head path)
  showIssue issue
  response      <-  lift $ dialogRun dialog
  case response of
    ResponseAccept -> updateIssue issue >>= updateIssueHelper project path
    _              -> return ()
  lift $ widgetHide dialog
  resetIssueFieldsState

updateIssueHelper :: Int -> TreePath -> Either String Issue -> ContextIO ()
updateIssueHelper project path issue = do
  context <- ask
  case issue of
    Right iss -> lift $ do
                          activeProject <- View.listStoreGetValue (context^.projectsStore)  project
                          updatedIssue <- updateIssueTiming iss
                          View.listStoreSetValue (context^.issuesStore) (head path) updatedIssue
                          updatedIssues <- View.listStoreToList (context^.issuesStore)
                          let newActiveProject = activeProject & (projectIssues .~ updatedIssues)
                          View.listStoreSetValue (context^.projectsStore) project newActiveProject
    Left err  -> showCustomNotification err


displayIssueInformation :: Dialog -> Button -> TreePath -> ContextIO ()
displayIssueInformation dialog btn path = do
  context <- ask
  activeProject <- lift $ readIORef (context^.activeProject)
  lift $ buttonSetLabel btn "Update"
  case activeProject of
    Just project -> showAndUpdateIssue dialog project path
    Nothing      -> showNoProjectChosen


addIssue :: Dialog -> Button -> ContextIO ()
addIssue dialog btn= do
  context <- ask
  activeProject <- lift $ readIORef (context^.activeProject)
  lift $ buttonSetLabel btn "Create"
  case activeProject of
    Just project -> addIssueToProject dialog project
    Nothing      -> showNoProjectChosen


addIssueHelper :: Project -> Int -> Either String Issue -> ContextIO ()
addIssueHelper activeRow project issue = do
  context <- ask
  case issue of
    Right iss -> lift $ do
                          let newActiveRow = activeRow & (projectIssues %~ (iss :))
                          View.listStoreInsert (context^.issuesStore) 0 iss
                          View.listStoreSetValue (context^.projectsStore) project newActiveRow
    Left err  -> showCustomNotification err


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

getIssueDescriptionText :: TextView -> IO String
getIssueDescriptionText textView = do
  buffer <- textViewGetBuffer textView
  start  <- textBufferGetStartIter buffer
  end    <- textBufferGetEndIter buffer
  textBufferGetText buffer start end True

updateIssue :: Issue -> ContextIO (Either String Issue)
updateIssue issue = do
  fieldsBundle <- asks (^.issueUiFieldsBundle)
  lift $ do
    name           <- entryGetText $ fieldsBundle^.issueNameField
    priority       <- fromIntegral.round <$> spinButtonGetValue (fieldsBundle^.issuePriorityField)
    trackingStatus <- toggleButtonGetActive $ fieldsBundle^.issueTrackingStatusField
    description    <- getIssueDescriptionText (fieldsBundle^.issueDescriptionField)
    trackedTime    <- countIssueTrackedSecondsIgnoringStatus issue
    case name of
      []  -> return $ Left "Issue's name can't be empty"
      str -> return $ Right $ issue {
                _issueName           = name
              , _issuePriority       = priority
              , _issueTrackingStatus = trackingStatus
              , _issueDescription    = description
              , _issueTimeRecorded   = trackedTime
            }


buildIssue :: ContextIO (Either String Issue)
buildIssue = do
  fieldsBundle <- asks (^.issueUiFieldsBundle)
  lift $ do
    name           <- entryGetText $ fieldsBundle^.issueNameField
    priority       <- fromIntegral.round <$> spinButtonGetValue (fieldsBundle^.issuePriorityField)
    creationDate   <- getCurrentDate
    timestamp      <- getSystemSeconds
    trackingStatus <- toggleButtonGetActive $ fieldsBundle^.issueTrackingStatusField
    description    <- getIssueDescriptionText (fieldsBundle^.issueDescriptionField)

    case name of
      []  -> return $ Left "Issue's name can't be empty"
      str -> return $ Right Issue {
                _issueName               = name
              , _issuePriority           = priority
              , _issueCreationDate       = creationDate
              , _issueLastTrackTimestamp = timestamp
              , _issueTimeRecorded       = 0
              , _issueTrackingStatus     = trackingStatus
              , _issueDescription        = description
            }