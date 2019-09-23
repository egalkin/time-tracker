
-- | Module provides functions for work with notifications. 
module UI.Notifications
     ( showNoIssueChosen
     , showNoProjectChosen 
     , showCustomNotification    
     ) where

import Graphics.UI.Gtk

import Control.Lens.Operators
import Control.Monad.Reader

import Model.Types(Message, ContextIO)
import Model.TypesLenses

-- | Shows notification with given message.
showNotification :: Message -> ContextIO ()
showNotification message = do
  context <- ask
  liftIO $ do
    contextId <- statusbarGetContextId (context^.notificationStatusbar) ""
    void $ statusbarPush (context^.notificationStatusbar) contextId message
    widgetShow (context^.notificationDialog)
    void $ dialogRun (context^.notificationDialog)
    widgetHide (context^.notificationDialog)
    statusbarPop (context^.notificationStatusbar) contextId

-- | Shows notification warning in case of
-- issue no chose.
showNoIssueChosen :: ContextIO ()
showNoIssueChosen = showNotification "Issue not chosen!"

-- | Shows notification warning in case of
-- project no chose.
showNoProjectChosen :: ContextIO ()
showNoProjectChosen = showNotification "Project not chosen!"

-- | Shows notification with custom message.
showCustomNotification :: Message -> ContextIO ()
showCustomNotification = showNotification