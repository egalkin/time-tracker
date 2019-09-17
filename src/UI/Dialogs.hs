module UI.Dialogs where

import Graphics.UI.Gtk

initAddIssueDialog :: Builder -> IO Dialog
initAddIssueDialog gui = do
  addIssueDialog <- builderGetObject gui castToDialog "addIssueDialog"
  dialogAddButton addIssueDialog "Create" ResponseAccept
  return addIssueDialog

initTrackedTimeDialog :: Builder -> IO Dialog
initTrackedTimeDialog gui = do
  trackedTimeDialog <- builderGetObject gui castToDialog "trackedTimeDialog"
  dialogAddButton trackedTimeDialog stockOk ResponseOk
  return trackedTimeDialog