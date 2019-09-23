
-- | This module provides functions for showing
-- projects tracked time.
module UI.TrackedTimeView 
     ( showProjectTrackedTime
     ) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Control.Monad.Reader
import Control.Lens.Operators

import Data.IORef

import Model.Types(ContextIO, Message)
import Model.TrackedTime
import Model.TypesLenses
import UI.Notifications
import Utils.TrackedTimeUtils

-- | Shows tracked time.
showTrackedTime :: Dialog -> Message -> TrackedTime -> ContextIO ()
showTrackedTime dialog message trackedTime = do
  context <- ask
  liftIO $ do
    contextId <- statusbarGetContextId (context^.notificationStatusbar) ""
    void $ statusbarPush (context^.trackedTimeStatusbar) contextId (message ++ show trackedTime)
    widgetShow dialog
    void $ dialogRun dialog
    widgetHide dialog
    statusbarPop (context^.trackedTimeStatusbar) contextId

-- | Shows project totally tracked time.
showProjectTrackedTime :: Dialog -> ContextIO ()
showProjectTrackedTime dialog = do
  context <- ask
  actProject <- liftIO $ readIORef (context^.activeProject)
  case actProject of
    Just projectId -> do
                        project <- liftIO $ View.listStoreGetValue (context^.projectsStore) projectId
                        trackedTime <- liftIO $ countProjectTrackedTime project
                        showTrackedTime dialog "Project time tracked: " trackedTime
    Nothing        -> showNoProjectChosen