
-- | Module provides functions for "CRUD" actions with issues.
-- Helps display them on view and so on.
module UI.Controllers.ProjectInterfaceController where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Model.Types(ContextIO)
import Model.Project
import Model.TypesLenses
import UI.Notifications
import Utils.TimeUtils

import Data.IORef

import Control.Monad.Reader
import Control.Lens.Operators


-- | Add new project to app state and view.
addProject :: ContextIO ()
addProject = do
  context   <- ask
  project   <- buildProject
  case project of 
    Right pr -> liftIO $ void $ View.listStoreAppend (context^.projectsStore) pr
    Left err -> showCustomNotification err              

-- | Remove all projects.
clearProjects :: ContextIO ()
clearProjects = do
  context <- ask
  liftIO $ do
    View.listStoreClear (context^.projectsStore)
    View.listStoreClear (context^.issuesStore)
    writeIORef (context^.activeProject) Nothing

-- | Builds new project.
buildProject :: ContextIO (Either String Project)
buildProject = do
  context      <- ask
  liftIO $ do
    name         <- entryGetText (context^.projectUiFieldsBundle.projectNameField)
    creationDate <- getCurrentDate
    case name of
      [] -> return $ Left "Project name can't be empty"
      _  -> return $ Right Project {
                             _projectName         = name
                           , _projectCreationDate = creationDate
                           , _projectIssues       = []
                           }

-- | Removes active project.
removeProject :: ContextIO ()
removeProject = do
  context <- ask
  currentActiveProject <- liftIO $ readIORef $ context^.activeProject
  case currentActiveProject of
    Just project -> liftIO $ do
                      View.listStoreRemove (context^.projectsStore) project
                      View.listStoreClear (context^.issuesStore)
    Nothing      -> showNoProjectChosen
