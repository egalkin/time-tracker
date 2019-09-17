module Interface where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import Data.IORef

import Control.Monad.Reader

import Types

import Control.Lens.Operators

import IssueParser

import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Calendar

import Text.Megaparsec.Error

import ProjectInterfaceController
import IssueInterfaceController

import Data.Either
import PrimaryInterface
import TypesLenses
import Notifications

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
  fileChooserDialog    <- initFileChooserDialog win

  insertButton   <- builderGetObject gui castToButton "insert"
  addIssueButton <- builderGetObject gui castToButton "issueButton"
  showTrackedTimeButton <- builderGetObject gui castToButton "trackedTimeButton"
  removeButton  <- builderGetObject gui castToButton "remove"
  clearButton   <- builderGetObject gui castToButton "clear"

  parseItem     <- builderGetObject gui castToMenuItem "parseIssues"

  on parseItem menuItemActivated $ (runReaderT $ openFileChooser fileChooserDialog) interfaceMainContext
  on insertButton buttonActivated $ runReaderT addProject interfaceMainContext
  on projectsView rowActivated $ \path row -> (runReaderT $ displayIssues path row) interfaceMainContext
  on issuesView rowActivated $ \path row -> (runReaderT $ writeCurrentIssue path row) interfaceMainContext
  on addIssueButton buttonActivated $ (runReaderT $ addIssue addIssueDialog) interfaceMainContext
  on removeButton buttonActivated $ runReaderT removeProject interfaceMainContext
  on clearButton buttonActivated $ runReaderT clearProjects interfaceMainContext
  on showTrackedTimeButton buttonActivated $ (runReaderT $ showTrackedTime trackedTimeDialog) interfaceMainContext

  widgetShowAll win
  mainGUI

initFileChooserDialog :: Window -> IO FileChooserDialog
initFileChooserDialog win = do
  dialog <- fileChooserDialogNew
              (Just $ "Demo of the standard dialog to select "
                         ++ "an existing file")
              (Just win)
              FileChooserActionOpen
              [("gtk-cancel"
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]
  isfilt <- fileFilterNew
  fileFilterAddPattern isfilt "*.is"
  fileFilterSetName isfilt "Files with issues data"

  fileChooserAddFilter dialog isfilt
  return dialog

fileChooserOpener :: FileChooserDialog -> Int -> ContextIO ()
fileChooserOpener dialog project = do
  lift $ widgetShow dialog
  response <- lift $ dialogRun dialog
  case response of
    ResponseAccept -> do Just fileName    <- lift $ fileChooserGetFilename dialog
                         parsedIssuesData <- lift $ parseIssues fileName
                         handleParsedData parsedIssuesData project

    ResponseCancel -> return ()
    ResponseDeleteEvent -> return ()
  lift $ widgetHide dialog

openFileChooser :: FileChooserDialog -> ContextIO ()
openFileChooser dialog = do
  context <- ask
  activeProject <- lift $ readIORef (context^.activeProject)
  case activeProject of
    Just project -> fileChooserOpener dialog project
    Nothing      -> showNoProjectChosen

handleParsedData :: [Either (ParseError Char Dec) Issue] -> Int -> ContextIO ()
handleParsedData issues project = do
  context <- ask
  activeRow     <- lift $ View.listStoreGetValue (context^.projectsStore) project
  let (_, correctlyParsedIssues) = partitionEithers issues
  currentTime <- lift (fromIntegral.systemSeconds <$> getSystemTime)
  lift $ putStrLn $ show correctlyParsedIssues
  let newActiveRow = activeRow & (projectIssues %~ (++ (map (issueLastTrackTimestamp.~currentTime) correctlyParsedIssues)))
  lift $ putStrLn $ show $ newActiveRow^.projectIssues
  lift $ do
    View.treeStoreClear (context^.issuesStore)
    mapM_ (View.treeStoreInsert (context^.issuesStore) [] 0) (newActiveRow^.projectIssues)
    View.listStoreSetValue (context^.projectsStore) project newActiveRow
