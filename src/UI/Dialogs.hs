module UI.Dialogs where

import Graphics.UI.Gtk

import Control.Monad (void)

-- | Init dialog that shows and handle entered issue data.
initIssueDialog :: Builder -> IO (Dialog, Button)
initIssueDialog gui = do
  issueDialog       <- builderGetObject gui castToDialog "addIssueDialog"
  issueActionButton <- dialogAddButton issueDialog "Create" ResponseAccept
  dialogSetDefaultResponse issueDialog ResponseAccept
  return (issueDialog, issueActionButton)

-- | Init dialog that shows active project tracked time. 
initTrackedTimeDialog :: Builder -> IO Dialog
initTrackedTimeDialog gui = do
  trackedTimeDialog <- builderGetObject gui castToDialog "trackedTimeDialog"
  void $ dialogAddButton trackedTimeDialog stockOk ResponseOk
  dialogSetDefaultResponse trackedTimeDialog ResponseAccept
  return trackedTimeDialog