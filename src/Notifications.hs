module Notifications
     ( showNoIssueChosen
     , showNoProjectChosen     
     )where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import Types(Message, ContextIO)
import TypesLenses
import Control.Lens.Operators

import Control.Monad.Reader

showNotification :: Message -> ContextIO ()
showNotification message = do
  context <- ask
  lift $ do
    contextId <- statusbarGetContextId (context^.notificationStatusbar) ""
    statusbarPush (context^.notificationStatusbar) contextId message
    widgetShow (context^.notificationDialog)
    dialogRun (context^.notificationDialog)
    widgetHide (context^.notificationDialog)
    statusbarPop (context^.notificationStatusbar) contextId


showNoIssueChosen :: ContextIO ()
showNoIssueChosen = showNotification "Issue not chosen!"

showNoProjectChosen :: ContextIO ()
showNoProjectChosen = showNotification "Project not chosen!"