module UI.TrackedTimeView where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import Model.Types(ContextIO)
import Model.TrackedTime
import Model.TypesLenses

import Data.IORef

import Control.Monad.Reader
import Control.Lens.Operators

import UI.Notifications

showIssueTrackedTime  :: Dialog -> ContextIO ()
showIssueTrackedTime dialog = do
  context <- ask
  activeIssue <- lift $ readIORef (context^.activeIssue)
  case activeIssue of
    Just issueId -> lift $ do
                      issue <- View.listStoreGetValue (context^.issuesStore) issueId
                      trackedTime <- countIssueTrackedTime issue
                      contextId <- statusbarGetContextId (context^.notificationStatusbar) ""
                      statusbarPush (context^.trackedTimeStatusbar) contextId ("Issue time tracked: " ++ show trackedTime)
                      widgetShow dialog
                      dialogRun dialog
                      widgetHide dialog
                      statusbarPop (context^.trackedTimeStatusbar) contextId
    Nothing      -> showNoIssueChosen

showProjectTrackedTime :: Dialog -> ContextIO ()
showProjectTrackedTime dialog = do
  context <- ask
  activeProject <- lift $ readIORef (context^.activeProject)
  case activeProject of
    Just projectId -> lift $ do
                        project <- View.listStoreGetValue (context^.projectsStore) projectId
                        trackedTime <- countProjectTrackedTime project
                        contextId <- statusbarGetContextId (context^.notificationStatusbar) ""
                        statusbarPush (context^.trackedTimeStatusbar) contextId ("Project time tracked: " ++ show trackedTime)
                        widgetShow dialog
                        dialogRun dialog
                        widgetHide dialog
                        statusbarPop (context^.trackedTimeStatusbar) contextId
    Nothing        -> showNoProjectChosen