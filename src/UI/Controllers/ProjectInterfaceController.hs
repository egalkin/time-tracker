module UI.Controllers.ProjectInterfaceController where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

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


addProject :: ContextIO ()
addProject = do
  context   <- ask
  project   <- buildProject
  case project of 
    Right pr -> lift $ do
                         View.listStoreAppend (context^.projectsStore) pr
                         return ()
    Left err -> showCustomNotification err              

buildProject :: ContextIO (Either String Project)
buildProject = do
  context      <- ask
  lift $ do
    name         <- entryGetText (context^.projectUiFieldsBundle.projectNameField)
    creationDate <- getCurrentDate
    case name of
      []   -> return $ Left "Project name can't be empty"
      str  -> return $ Right Project {
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
