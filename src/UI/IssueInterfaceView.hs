module UI.IssueInterfaceView
     ( initIssueUiFieldBundle
     , setupIssuesView
     )where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Model.Types(IssueUiFieldsBundle(..))
import Model.Issue
import Model.TypesLenses
import Model.TrackedTime
import Control.Lens.Operators

setupIssuesView :: TreeViewClass view
                  => view
                  -> ListStore Issue
                  -> TypedTreeModelSort Issue
                  -> IO ()
setupIssuesView view issuesStore sortedIssueStore = do

  View.treeViewSetHeadersVisible view True

  mapSortFunctionsToIds issuesStore sortedIssueStore 1 (^.issueName)
  mapSortFunctionsToIds issuesStore sortedIssueStore 2 (^.issuePriority)
  mapSortFunctionsToIds issuesStore sortedIssueStore 3 (^.issueCreationDate)
  
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

  mapModelsFields nameCol renderNameCol issuesStore sortedIssueStore (^.issueName)
  mapModelsFields priorityCol renderPriorityCol issuesStore sortedIssueStore (show . (^.issuePriority))
  mapModelsFields createdAtCol renderCreatedAtCol issuesStore sortedIssueStore (show . (^.issueCreationDate) )
  mapModelsFields recordedCol renderRecordedCol issuesStore sortedIssueStore (show . convertSecondsToTrackedTime. (^.issueTimeRecorded) )


  View.treeViewAppendColumn view nameCol
  View.treeViewAppendColumn view priorityCol
  View.treeViewAppendColumn view createdAtCol
  View.treeViewAppendColumn view recordedCol

  View.treeViewColumnSetSortColumnId nameCol 1
  View.treeViewColumnSetSortColumnId priorityCol 2
  View.treeViewColumnSetSortColumnId createdAtCol 3

mapModelsFields :: TreeViewColumn
                  -> CellRendererText
                  -> ListStore Issue
                  -> TypedTreeModelSort Issue
                  -> (Issue -> String)
                  -> IO ()
mapModelsFields col render model sortedModel displayFunc =
  View.cellLayoutSetAttributeFunc col render sortedModel $ \iter -> do
       cIter <- View.treeModelSortConvertIterToChildIter sortedModel iter
       issue <- View.treeModelGetRow model cIter
       set render [View.cellText := displayFunc issue]


mapSortFunctionsToIds :: Ord a
                        => ListStore Issue
                        -> TypedTreeModelSort Issue
                        -> Int
                        -> (Issue -> a)
                        -> IO()
mapSortFunctionsToIds issuesStore sortedIssueStore funcId compareField = 
  View.treeSortableSetSortFunc sortedIssueStore funcId $ \iter1 iter2 -> do
      issue1 <- View.customStoreGetRow issuesStore iter1
      issue2 <- View.customStoreGetRow issuesStore iter2
      return (compare (compareField issue1) (compareField issue2))       



initIssueUiFieldBundle :: Builder -> IO IssueUiFieldsBundle
initIssueUiFieldBundle gui = do
  issueNameField          <- builderGetObject gui castToEntry "issueNameField"
  issuePriorityField      <- builderGetObject gui castToSpinButton "issuePriorityField"
  issueStartTrackingField <- builderGetObject gui castToCheckButton "issueTrackingStatusField"

  return IssueUiFieldsBundle {
    _issueNameField                = issueNameField,
    _issuePriorityField            = issuePriorityField,
    _issueTrackingStatusField      = issueStartTrackingField
  }


