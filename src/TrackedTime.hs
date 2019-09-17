{-# LANGUAGE TemplateHaskell #-}

module TrackedTime where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import Types(Issue, ContextIO)
import TypesLenses
import TimeUtils
import Notifications

import Data.Semigroup
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

convertSecondsToTrackedTime :: Int -> TrackedTime
convertSecondsToTrackedTime seconds = do
  let hours = seconds `div` secondsInHour
  TrackedTime { _hours = hours, _minutes = (seconds - hours * secondsInHour) `div` 60}


countIssueTrackedTime :: Issue -> IO TrackedTime
countIssueTrackedTime issue
  | not (issue^.issueTrackingStatus) = return $ convertSecondsToTrackedTime $ issue^.issueTimeRecorded
  | issue^.issueTrackingStatus       = do
      currentTimestamp <- getSystemSeconds
      return $ convertSecondsToTrackedTime (issue^.issueTimeRecorded + (currentTimestamp - issue^.issueLastTrackTimestamp))


showTrackedTime  :: Dialog -> ContextIO ()
showTrackedTime dialog = do
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
