module UI.Dialogs where

import Graphics.UI.Gtk

initIssueDialog :: Builder -> IO (Dialog, Button)
initIssueDialog gui = do
  issueDialog       <- builderGetObject gui castToDialog "addIssueDialog"
  issueActionButton <- dialogAddButton issueDialog "Create" ResponseAccept
  return (issueDialog, issueActionButton) 

initTrackedTimeDialog :: Builder -> IO Dialog
initTrackedTimeDialog gui = do
  trackedTimeDialog <- builderGetObject gui castToDialog "trackedTimeDialog"
  dialogAddButton trackedTimeDialog stockOk ResponseOk
  return trackedTimeDialog