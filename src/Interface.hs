module Interface where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import Data.IORef

import Control.Monad.Reader

import Types

import Control.Lens.Operators

import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Calendar

type ContextIO a = ReaderT InterfaceMainContext IO a

initInterface = do
  initGUI

  gui <- builderNew
  builderAddFromFile gui "interface.glade"

  win <- builderGetObject gui castToWindow "appWindow"
  on win objectDestroy mainQuit

  projectsView <- builderGetObject gui castToTreeView "projectsView"
  issuesView   <- builderGetObject gui castToTreeView "issuesView"


  interfaceMainContext <- buildMainContext gui projectsView issuesView

  addIssueDialog       <- initAddIssueDialog gui
  trackedTimeDialog    <- initTrackedTimeDialog gui

  insertButton   <- builderGetObject gui castToButton "insert"
  addIssueButton <- builderGetObject gui castToButton "issueButton"
  showTrackedTimeButton <- builderGetObject gui castToButton "trackedTimeButton"
  removeButton  <- builderGetObject gui castToButton "remove"
  clearButton   <- builderGetObject gui castToButton "clear"

  on insertButton buttonActivated $ runReaderT addProject interfaceMainContext
  on projectsView rowActivated $ \path row -> (runReaderT $ displayIssues path row) interfaceMainContext
  on addIssueButton buttonActivated $ (runReaderT $ addIssue addIssueDialog) interfaceMainContext
  on removeButton buttonActivated $ runReaderT removeProject interfaceMainContext
  on showTrackedTimeButton buttonActivated $ (runReaderT $ showTrackedTime trackedTimeDialog) interfaceMainContext


  widgetShowAll win
  mainGUI

convertSecondsToTrackedTime :: Int -> TrackedTime
convertSecondsToTrackedTime seconds = do
  let hours = seconds `div` secondsInHour
  TrackedTime { _hours = hours, _minutes = (seconds - hours * secondsInHour) `div` 60}


countIssueTrackedTime :: Issue -> IO TrackedTime
countIssueTrackedTime issue
  | issue^.issueTrackingStatus       = return $ convertSecondsToTrackedTime $ issue^.issueTimeRecorded
  | not (issue^.issueTrackingStatus) = do
      systemTime <- getSystemTime
      return $ convertSecondsToTrackedTime (issue^.issueTimeRecorded + ((fromIntegral $ systemSeconds systemTime) - issue^.issueLastTrackTimestamp))

showTrackedTime :: Dialog -> ContextIO ()
showTrackedTime dialog = do
  context <- ask
  lift $ do
    activeIssue <- readIORef (context^.activeIssue)
    issue <- View.listStoreGetValue (context^.issuesStore) activeIssue
    trackedTime <- countIssueTrackedTime issue
    set (context^.trackedTimeStatusbar) [entryText := show trackedTime ]
    widgetShow dialog
    dialogRun dialog
    widgetHide dialog


removeProject :: ContextIO ()
removeProject = do
  context <- ask
  lift $ do
    activeProject <- readIORef $ context^.activeProject
    View.listStoreRemove (context^.projectsStore) activeProject

addProject :: ContextIO ()
addProject = do
  context   <- ask
  project   <- buildProject
  lift $ View.listStoreInsert (context^.projectsStore) 0 project

buildProject :: ContextIO Project
buildProject = do
  context      <- ask
  lift $ do
    name         <- entryGetText (context^.projectUiFieldsBundle.projectNameField)
    creationDate <- getCurrentTime >>= return . utctDay

    return Project {
      _projectName         = name,
      _projectCreationDate = creationDate,
      _projectTimeRecorded = 0,
      _projectIssues       = []
    }

buildMainContext :: Builder
                 -> TreeView
                 -> TreeView
                 -> IO InterfaceMainContext
buildMainContext gui projectsView issuesView = do
  (projectsStore, issuesStore) <- initStores projectsView issuesView
  projectUiFieldsBundle        <- initProjectUiFieldBundle gui
  issueUiFieldsBundle          <- initIssueUiFieldBundle gui
  activeProject                <- newIORef 0
  activeIssue                  <- newIORef 0
  trackedTimeStatusbar         <- builderGetObject gui castToEntry "issueTimeTrackedStatus"

  return InterfaceMainContext {
    _projectsStore = projectsStore,
    _projectUiFieldsBundle = projectUiFieldsBundle,
    _issuesStore = issuesStore,
    _issueUiFieldsBundle = issueUiFieldsBundle,
    _activeProject = activeProject,
    _activeIssue = activeIssue,
    _trackedTimeStatusbar = trackedTimeStatusbar
  }

displayIssues :: TreePath -> TreeViewColumn -> ContextIO ()
displayIssues path row = do
  context <- ask
  projectEntity <- lift $ View.listStoreGetValue (context^.projectsStore) (head path)
  lift $ do
    View.listStoreClear (context^.issuesStore)
    writeIORef (context^.activeProject) (head path)
    mapM_ (View.listStoreInsert (context^.issuesStore) 0) (projectEntity^.projectIssues)

addIssue :: Dialog -> ContextIO ()
addIssue dialog  = do
  context <- ask
  lift $ do
    activeProject <- readIORef (context^.activeProject)
    activeRow     <- View.listStoreGetValue (context^.projectsStore) activeProject
    widgetShow dialog
    response <- dialogRun dialog
    case response of
      ResponseAccept -> do
        issue <- runReaderT buildIssue context
        (runReaderT $ addIssueHelper activeRow issue) context
      _              -> return ()
    widgetHide dialog

addIssueHelper :: Project -> Issue -> ContextIO ()
addIssueHelper activeRow issue = do
  context <- ask
  let newActiveRow = activeRow & (projectIssues %~ (issue :))
  lift $ do
   View.listStoreClear (context^.issuesStore)
   mapM_ (View.listStoreInsert (context^.issuesStore) 0) (newActiveRow^.projectIssues)
   currentActiveProject <- readIORef $ context^.activeProject
   View.listStoreSetValue (context^.projectsStore) currentActiveProject newActiveRow


initAddIssueDialog :: Builder -> IO Dialog
initAddIssueDialog gui = do
  addIssueDialog <- builderGetObject gui castToDialog "addIssueDialog"
  dialogAddButton addIssueDialog "Create" ResponseAccept
  return addIssueDialog

initTrackedTimeDialog :: Builder -> IO Dialog
initTrackedTimeDialog gui = do
  trackedTimeDialog <- builderGetObject gui castToDialog "trackedTimeDialog"
  dialogAddButton trackedTimeDialog stockOk ResponseOk
  return trackedTimeDialog

buildIssue :: ContextIO Issue
buildIssue = do
  context          <- ask
  lift $ do
    name           <- entryGetText $ context^.issueUiFieldsBundle.issueNameField
    priority       <- fromIntegral.round <$> (spinButtonGetValue $ context^.issueUiFieldsBundle.issuePriorityField)
    creationDate   <- getCurrentTime >>= return . utctDay
    timestamp      <- fromIntegral.systemSeconds <$> getSystemTime
    trackingStatus <- toggleButtonGetActive $ context^.issueUiFieldsBundle.issueTrackingStatusField

    return Issue {
      _issueName               = name,
      _issuePriority           = priority,
      _issueCreationDate       = creationDate,
      _issueLastTrackTimestamp = timestamp,
      _issueTimeRecorded       = 0,
      _issueTrackingStatus     = trackingStatus
    }

initProjectUiFieldBundle :: Builder -> IO ProjectUiFieldsBundle
initProjectUiFieldBundle gui = do
  projectNameField <- builderGetObject gui castToEntry "projectNameField"
  return ProjectUiFieldsBundle {_projectNameField = projectNameField}

initIssueUiFieldBundle :: Builder -> IO IssueUiFieldsBundle
initIssueUiFieldBundle gui = do
  issueNameField          <- builderGetObject gui castToEntry "issueNameField"
  issuePriorityField      <- builderGetObject gui castToSpinButton "issuePriorityField"
  issueStartTrackingField <- builderGetObject gui castToCheckButton "issueTrackingStatusField"

  return IssueUiFieldsBundle {
    _issueNameField                = issueNameField,
    _issuePriorityField            = issuePriorityField,
    _issueTrackingStatusField      = issueStartTrackingField
  }


initStores :: TreeViewClass view
             => view
             -> view
             -> IO (View.ListStore Project, View.ListStore Issue)
initStores projectsView issuesView = do
  projectStore <- storeImpl
  issuesStore  <- storeImpl
  View.treeViewSetModel projectsView projectStore
  View.treeViewSetModel issuesView issuesStore
  setupProjectsView projectsView projectStore
  setupIssuesView issuesView issuesStore
  return (projectStore, issuesStore)

setupIssuesView view model = do
  View.treeViewSetHeadersVisible view True

  nameCol      <- View.treeViewColumnNew
  priorityCol  <- View.treeViewColumnNew
  createdAtCol <- View.treeViewColumnNew
  recordedCol  <- View.treeViewColumnNew

  View.treeViewColumnSetTitle nameCol "Issue name"
  View.treeViewColumnSetTitle priorityCol "Priority"
  View.treeViewColumnSetTitle createdAtCol "Created"
  View.treeViewColumnSetTitle recordedCol "Time recorded"

  rendererNameCol      <- View.cellRendererTextNew
  rendererPriorityCol  <- View.cellRendererTextNew
  rendererCreatedAtCol <- View.cellRendererTextNew
  rendererRecorderCol  <- View.cellRendererTextNew

  View.cellLayoutPackStart nameCol rendererNameCol True
  View.cellLayoutPackStart priorityCol rendererPriorityCol True
  View.cellLayoutPackStart createdAtCol rendererCreatedAtCol True
  View.cellLayoutPackStart recordedCol rendererRecorderCol True

  View.cellLayoutSetAttributes nameCol rendererNameCol model $ \row -> [ View.cellText := row^.issueName ]
  View.cellLayoutSetAttributes priorityCol rendererPriorityCol model $ \row -> [ View.cellText := show $ row^.issuePriority ]
  View.cellLayoutSetAttributes createdAtCol rendererRecorderCol model $ \row -> [ View.cellText := show $ row^.issueLastTrackTimestamp]
  View.cellLayoutSetAttributes createdAtCol rendererCreatedAtCol model $ \row -> [ View.cellText := show $ row^.issueCreationDate ]

  View.treeViewAppendColumn view nameCol
  View.treeViewAppendColumn view priorityCol
  View.treeViewAppendColumn view createdAtCol
  View.treeViewAppendColumn view recordedCol


setupProjectsView view model = do
  View.treeViewSetHeadersVisible view True

  -- add three columns
  nameCol <- View.treeViewColumnNew
  createdAtCol <- View.treeViewColumnNew
  recordedCol <- View.treeViewColumnNew


  View.treeViewColumnSetTitle nameCol "Project name"
  View.treeViewColumnSetTitle createdAtCol "Created"
  View.treeViewColumnSetTitle recordedCol "Total time recorded"

  nameRender      <- View.cellRendererTextNew
  createAtdRender <- View.cellRendererTextNew
  recordedRender  <- View.cellRendererTextNew

  View.cellLayoutPackStart nameCol nameRender True
  View.cellLayoutPackStart createdAtCol createAtdRender True
  View.cellLayoutPackStart recordedCol recordedRender True

  View.cellLayoutSetAttributes nameCol nameRender model $ \row -> [ View.cellText := row^.projectName ]
  View.cellLayoutSetAttributes createdAtCol createAtdRender model $ \row -> [ View.cellText := show $ row^.projectCreationDate ]
  View.cellLayoutSetAttributes recordedCol recordedRender model $ \row -> [ View.cellText := show $ row^.projectTimeRecorded ]

  View.treeViewAppendColumn view nameCol
  View.treeViewAppendColumn view createdAtCol
  View.treeViewAppendColumn view recordedCol


storeImpl =
  View.listStoreNew []


secondsInHour = 3600