module UI.Controllers.FileParsingController
     ( initFileChooserDialog
     , importProjects
     , importIssues
     )where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Data.IORef
import Data.Either

import Model.Types(ContextIO)
import Model.Issue
import Model.Project
import Model.TypesLenses
import Control.Lens.Operators
import Text.Megaparsec.Error
import Parsers.IssueParser
import Parsers.ProjectParser
import Utils.TimeUtils
import UI.Notifications
import Data.Maybe
import Control.Monad.Reader

initFileChooserDialog :: Window -> IO FileChooserDialog
initFileChooserDialog win =
  fileChooserDialogNew
              (Just $ "Demo of the standard dialog to select "
                         ++ "an existing file")
              (Just win)
              FileChooserActionOpen
              [("gtk-cancel"
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]

setFileExtensionFilter :: FileChooserDialog -> (String, String) -> IO FileChooserDialog
setFileExtensionFilter dialog (extension, exName) = do
  filter <- fileFilterNew
  fileFilterAddPattern filter extension
  fileFilterSetName filter exName
  fileChooserSetFilter dialog filter
  return dialog

importProjects :: FileChooserDialog -> ContextIO ()
importProjects dialog = do
  lift $ setFileExtensionFilter dialog projectsExtension
  lift $ widgetShow dialog
  response <- lift $ dialogRun dialog
  case response of
    ResponseAccept -> do Just fileName      <- lift $ fileChooserGetFilename dialog
                         parsedProjectsData <- lift $ parseProjectsFromFile fileName
                         handleParsedProjectsData parsedProjectsData
    ResponseCancel -> return ()
    ResponseDeleteEvent -> return ()
  lift $ widgetHide dialog


parseIssuesFromFile :: FileChooserDialog -> Int -> ContextIO ()
parseIssuesFromFile dialog project = do
  lift $ setFileExtensionFilter dialog issuesExtension
  lift $ widgetShow dialog
  response <- lift $ dialogRun dialog
  case response of
    ResponseAccept -> do Just fileName    <- lift $ fileChooserGetFilename dialog
                         parsedIssuesData <- lift $ parseIssues fileName
                         handleParsedData parsedIssuesData project

    ResponseCancel -> return ()
    ResponseDeleteEvent -> return ()
  lift $ widgetHide dialog

importIssues :: FileChooserDialog -> ContextIO ()
importIssues dialog = do
  context <- ask
  lift $ putStrLn "Hai"
  activeProject <- lift $ readIORef (context^.activeProject)
  case activeProject of
    Just project -> parseIssuesFromFile dialog project
    Nothing      -> showNoProjectChosen

handleParsedProjectsData :: Either (ParseError Char Dec) [Project] -> ContextIO ()
handleParsedProjectsData parsedData = do
  context <- ask
  case parsedData of
    Left err       -> return ()
    Right projects -> lift $ mapM_ (View.listStoreAppend (context^.projectsStore)) projects

handleParsedData :: [Either (ParseError Char Dec) Issue] -> Int -> ContextIO ()
handleParsedData issues project = do
  context <- ask
  activeRow     <- lift $ View.listStoreGetValue (context^.projectsStore) project
  let (_, correctlyParsedIssues) = partitionEithers issues
  let newActiveRow = activeRow & (projectIssues %~ (++correctlyParsedIssues))
  lift $ do
    View.listStoreClear (context^.issuesStore)
    mapM_ (View.listStoreAppend (context^.issuesStore)) (newActiveRow^.projectIssues)
    View.listStoreSetValue (context^.projectsStore) project newActiveRow

issuesExtension = ("*.is", "Files with issues data")

projectsExtension = ("*.proj", "Files with project data")