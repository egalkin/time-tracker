module UI.Controllers.ProjectInterfaceController where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import UI.PrimaryInterface
import Control.Monad.Reader
import Control.Lens.Operators
import Data.IORef
import Utils.TimeUtils
import Model.Types(ContextIO)
import Model.Project
import Model.TypesLenses
import UI.Notifications

clearProjects :: ContextIO ()
clearProjects = do
  context <- ask
  lift $ do
    View.listStoreClear (context^.issuesStore)
    View.listStoreClear (context^.projectsStore)
    writeIORef (context^.activeProject) Nothing
    writeIORef (context^.activeIssue) Nothing


addProject :: ContextIO ()
addProject = do
  context   <- ask
  project   <- buildProject
  lift $ do
    View.listStoreAppend (context^.projectsStore) project
    return ()

buildProject :: ContextIO Project
buildProject = do
  context      <- ask
  lift $ do
    name         <- entryGetText (context^.projectUiFieldsBundle.projectNameField)
    creationDate <- getCurrentDate

    return Project {
      _projectName         = name,
      _projectCreationDate = creationDate,
      _projectTimeRecorded = 0,
      _projectIssues       = []
    }


removeProject :: ContextIO ()
removeProject = do
  context <- ask
  currentActiveProject <- lift $ readIORef $ context^.activeProject
  case currentActiveProject of
    Just project -> lift $ do
                      View.listStoreRemove (context^.projectsStore) project
                      View.listStoreClear (context^.issuesStore)
    Nothing      -> showNoProjectChosen
