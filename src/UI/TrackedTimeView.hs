module UI.TrackedTimeView where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Model.Types(ContextIO, Message)
import Model.TrackedTime
import Model.TypesLenses

import Data.IORef

import Control.Monad.Reader
import Control.Lens.Operators

import UI.Notifications

showTrackedTime :: Dialog -> Message -> TrackedTime -> ContextIO ()
showTrackedTime dialog message trackedTime = do
  context <- ask
  lift $ do
    contextId <- statusbarGetContextId (context^.notificationStatusbar) ""
    statusbarPush (context^.trackedTimeStatusbar) contextId (message ++ show trackedTime)
    widgetShow dialog
    dialogRun dialog
    widgetHide dialog
    statusbarPop (context^.trackedTimeStatusbar) contextId

showProjectTrackedTime :: Dialog -> ContextIO ()
showProjectTrackedTime dialog = do
  context <- ask
  activeProject <- lift $ readIORef (context^.activeProject)
  case activeProject of
    Just projectId -> do
                        project <- lift $ View.listStoreGetValue (context^.projectsStore) projectId
                        trackedTime <- lift $ countProjectTrackedTime project
                        showTrackedTime dialog "Project time tracked: " trackedTime
    Nothing        -> showNoProjectChosen