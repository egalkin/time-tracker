{-# LANGUAGE RecordWildCards #-}

-- | This model set up view model for displaying issues data.
module UI.IssueInterfaceView
     ( initIssueUiFieldBundle
     , setupIssuesView
     ) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Control.Monad(void)
import Control.Lens.Operators

import Model.Types(IssueUiFieldsBundle(..))
import Model.Issue
import Model.TypesLenses
import Utils.TrackedTimeUtils(convertSecondsToTrackedTime)
import Utils.ViewUtils

-- | Set up issues TreeView configuration.
setupIssuesView :: TreeViewClass view
                  => view
                  -> ListStore Issue
                  -> TypedTreeModelSort Issue
                  -> IO ()
setupIssuesView view store sortedStore = do

  View.treeViewSetHeadersVisible view True

  mapSortFunctionsToIds store sortedStore 1 (^.issueName)
  mapSortFunctionsToIds store sortedStore 2 (^.issuePriority)
  mapSortFunctionsToIds store sortedStore 3 (^.issueCreationDate)
  mapSortFunctionsToIds store sortedStore 4 (^.issueTimeTracked)
  
  nameCol      <- View.treeViewColumnNew
  priorityCol  <- View.treeViewColumnNew
  createdAtCol <- View.treeViewColumnNew
  recordedCol  <- View.treeViewColumnNew

  View.treeViewColumnSetTitle nameCol "Issue name"
  View.treeViewColumnSetTitle priorityCol "Priority"
  View.treeViewColumnSetTitle createdAtCol "Created"
  View.treeViewColumnSetTitle recordedCol "Time recorded"

  renderNameCol      <- View.cellRendererTextNew
  renderPriorityCol  <- View.cellRendererTextNew
  renderCreatedAtCol <- View.cellRendererTextNew
  renderRecordedCol  <- View.cellRendererTextNew

  View.cellLayoutPackStart nameCol renderNameCol True
  View.cellLayoutPackStart priorityCol renderPriorityCol True
  View.cellLayoutPackStart createdAtCol renderCreatedAtCol True
  View.cellLayoutPackStart recordedCol renderRecordedCol True

  setModelsFields nameCol renderNameCol store sortedStore (^.issueName)
  setModelsFields priorityCol renderPriorityCol store sortedStore (show . (^.issuePriority))
  setModelsFields createdAtCol renderCreatedAtCol store sortedStore (show . (^.issueCreationDate) )
  setModelsFields recordedCol renderRecordedCol store sortedStore (show . convertSecondsToTrackedTime. (^.issueTimeTracked) )

  void $ View.treeViewAppendColumn view nameCol
  void $ View.treeViewAppendColumn view priorityCol
  void $ View.treeViewAppendColumn view createdAtCol
  void $ View.treeViewAppendColumn view recordedCol

  View.treeViewColumnSetSortColumnId nameCol 1
  View.treeViewColumnSetSortColumnId priorityCol 2
  View.treeViewColumnSetSortColumnId createdAtCol 3
  View.treeViewColumnSetSortColumnId recordedCol 4

-- | Initialize fields with issue input fields.
-- Used for issue creating.
initIssueUiFieldBundle :: Builder -> IO IssueUiFieldsBundle
initIssueUiFieldBundle gui = do
  _issueNameField           <- builderGetObject gui castToEntry "issueNameField"
  _issuePriorityField       <- builderGetObject gui castToSpinButton "issuePriorityField"
  _issueTrackingStatusField <- builderGetObject gui castToCheckButton "issueTrackingStatusField"
  _issueDescriptionField    <- builderGetObject gui castToTextView "issueDescriptionField"

  return IssueUiFieldsBundle {..}


