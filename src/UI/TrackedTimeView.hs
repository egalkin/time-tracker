module UI.TrackedTimeView where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

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
    statusbarPush (context^.trackedTimeStatusbar) contextId ("Project time tracked: " ++ show trackedTime)
    widgetShow dialog
    dialogRun dialog
    widgetHide dialog
    statusbarPop (context^.trackedTimeStatusbar) contextId
  
  
showIssueTrackedTime  :: Dialog -> ContextIO ()
showIssueTrackedTime dialog = do
  context <- ask
  activeIssue <- lift $ readIORef (context^.activeIssue)
  case activeIssue of
    Just issueId -> do
                      issue <- lift $ View.listStoreGetValue (context^.issuesStore) issueId
                      trackedTime <- lift $ countIssueTrackedTime issue
                      showTrackedTime dialog "Issue time tracked: " trackedTime
    Nothing      -> showNoIssueChosen

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