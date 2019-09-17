{-# LANGUAGE TemplateHaskell #-}

module TrackedTime where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import Model.Types(ContextIO)
import Model.Issue
import Model.Project
import Model.TypesLenses
import TimeUtils
import Notifications

import Data.Semigroup
import Data.List (foldl')
import Control.Lens
import Control.Lens.Operators
import Control.Monad.Reader
import Data.IORef

data TrackedTime = TrackedTime {
    _hours   :: Int
  , _minutes :: Int
}

instance Show TrackedTime where
  show (TrackedTime hr mi) = show hr <> ":" <> showSeconds mi
    where
      showSeconds mi = 
        case mi `div` 10 of
        0   -> "0" ++ show mi
        _   -> show mi
        
trackedTimeToInt :: TrackedTime -> Int
trackedTimeToInt time = _hours time * 3600 + _minutes time * 60

makeLenses ''TrackedTime

convertSecondsToTrackedTime :: Int -> IO TrackedTime
convertSecondsToTrackedTime seconds = do
  let hours = seconds `div` secondsInHour
  return $ TrackedTime { _hours = hours, _minutes = (seconds - hours * secondsInHour) `div` 60}

countIssueTrackedSeconds :: Issue -> IO Int
countIssueTrackedSeconds issue
  | not (issue^.issueTrackingStatus) = return $ issue^.issueTimeRecorded
  | issue^.issueTrackingStatus       = do
      currentTimestamp <- getSystemSeconds
      return $ issue^.issueTimeRecorded + (currentTimestamp - issue^.issueLastTrackTimestamp)

countIssueTrackedTime :: Issue -> IO TrackedTime
countIssueTrackedTime issue = countIssueTrackedSeconds issue >>= convertSecondsToTrackedTime

countProjectTrackedTime :: Project -> IO TrackedTime
countProjectTrackedTime project =
  foldl' ioIntPlus (pure 0) (map countIssueTrackedSeconds (project^.projectIssues)) >>= convertSecondsToTrackedTime
    where
      ioIntPlus :: IO Int -> IO Int -> IO Int
      ioIntPlus accum ioInt = do
        s1 <- accum
        s2 <- ioInt
        return (s1 + s2)


showIssueTrackedTime  :: Dialog -> ContextIO ()
showIssueTrackedTime dialog = do
  context <- ask
  activeIssue <- lift $ readIORef (context^.activeIssue)
  case activeIssue of
    Just issueId -> lift $ do
                      issue <- View.treeStoreGetValue (context^.issuesStore) [issueId]
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