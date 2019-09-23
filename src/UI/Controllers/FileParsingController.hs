
-- | Module provide functions for choosing and handling imported from file data.
module UI.Controllers.FileParsingController
     ( initFileChooserDialog
     , importProjects
     , importIssues
     )where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Data.IORef
import Data.Either

import Control.Lens.Operators
import Control.Monad.Reader

import Text.Megaparsec.Error

import Model.Types(ContextIO)
import Model.Issue
import Model.Project
import Model.TypesLenses
import UI.Notifications
import Parsers.IssueParser
import Parsers.ProjectParser


-- | Type represents file extension.
type Extension = String

-- | Init dialog for import source file choosing.
initFileChooserDialog :: Window -> IO FileChooserDialog
initFileChooserDialog win =
  fileChooserDialogNew
              (Just "Chose import file")
              (Just win)
              FileChooserActionOpen
              [("gtk-cancel"
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]

-- | Sets filter on shown in dialogs files
setFileExtensionFilter :: FileChooserDialog -> Extension -> IO FileChooserDialog
setFileExtensionFilter dialog extension = do
  filt <- fileFilterNew
  fileFilterAddPattern filt extension
  fileChooserSetFilter dialog filt
  return dialog

-- | Import projects from chosen file.
importProjects :: FileChooserDialog -> ContextIO ()
importProjects dialog = do
  void $ liftIO $ setFileExtensionFilter dialog projectsExtension
  liftIO $ widgetShow dialog
  resp <- liftIO $ dialogRun dialog
  case resp of
    ResponseAccept -> do 
      Just fileName      <- liftIO $ fileChooserGetFilename dialog
      parsedProjectsData <- liftIO $ parseProjectsFromFile fileName
      handleParsedProjectsData parsedProjectsData
    _              -> return ()
  liftIO $ widgetHide dialog

-- | Handles correctly parsed data and ignores invalid's one.
handleParsedProjectsData :: Either (ParseError Char Dec) [Project] -> ContextIO ()
handleParsedProjectsData parsedData = do
  context <- ask
  case parsedData of
    Left _         -> return ()
    Right projects -> liftIO $ mapM_ (View.listStoreAppend (context^.projectsStore)) projects

-- | Imports issues from file.
importIssues :: FileChooserDialog -> ContextIO ()
importIssues dialog = do
  context <- ask
  actProject <- liftIO $ readIORef (context^.activeProject)
  case actProject of
    Just project -> parseIssuesFromFile dialog project
    Nothing      -> showNoProjectChosen

-- | Parse imported issues and add them to state.
parseIssuesFromFile :: FileChooserDialog -> Int -> ContextIO ()
parseIssuesFromFile dialog project = do
  void $ liftIO $ setFileExtensionFilter dialog issuesExtension
  liftIO $ widgetShow dialog
  resp <- liftIO $ dialogRun dialog
  case resp of
    ResponseAccept -> do 
      Just fileName    <- liftIO $ fileChooserGetFilename dialog
      parsedIssuesData <- liftIO $ parseIssues fileName
      handleParsedIssuesData parsedIssuesData project
    _              -> return ()
  liftIO $ widgetHide dialog

-- | Handles correctly parsed data and ignores invalid's one.
handleParsedIssuesData :: [Either (ParseError Char Dec) Issue] -> Int -> ContextIO ()
handleParsedIssuesData issues project = do
  context <- ask
  activeRow     <- liftIO $ View.listStoreGetValue (context^.projectsStore) project
  let (_, correctlyParsedIssues) = partitionEithers issues
  let newActiveRow = activeRow & (projectIssues %~ (++correctlyParsedIssues))
  liftIO $ do
    View.listStoreClear (context^.issuesStore)
    mapM_ (View.listStoreAppend (context^.issuesStore)) (newActiveRow^.projectIssues)
    View.listStoreSetValue (context^.projectsStore) project newActiveRow

-- | Return extension for issues file.
issuesExtension :: Extension
issuesExtension = "*.is"

-- | Return extension for projects file.
projectsExtension :: Extension
projectsExtension = "*.proj"