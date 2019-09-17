module FileParsingController
     ( initFileChooserDialog
     , openFileChooser
     )where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import Data.IORef
import Data.Either

import Model.Types(ContextIO)
import Model.Issue
import Model.Project
import Model.TypesLenses
import Control.Lens.Operators
import Text.Megaparsec.Error
import IssueParser
import TimeUtils
import Notifications

import Control.Monad.Reader

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
  currentTime <- lift getSystemSeconds
  lift $ putStrLn $ show correctlyParsedIssues
  let newActiveRow = activeRow & (projectIssues %~ (++ (map (issueLastTrackTimestamp.~currentTime) correctlyParsedIssues)))
  lift $ putStrLn $ show $ newActiveRow^.projectIssues
  lift $ do
    View.treeStoreClear (context^.issuesStore)
    mapM_ (View.treeStoreInsert (context^.issuesStore) [] 0) (newActiveRow^.projectIssues)
    View.listStoreSetValue (context^.projectsStore) project newActiveRow

