
-- | Module encapsulate some functions for view model configuration.
-- Used in 'IssueInterfaceView' and 'ProjectsInterfaceView".
module Utils.ViewUtils
     ( setModelsFields
     , mapSortFunctionsToIds
     ) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

-- | Set display way for model fields
setModelsFields :: TreeViewColumn
                  -> CellRendererText
                  -> ListStore a
                  -> TypedTreeModelSort a
                  -> (a -> String)
                  -> IO ()
setModelsFields col render model sortedModel displayFunc =
  View.cellLayoutSetAttributeFunc col render sortedModel $ \iter -> do
       cIter <- View.treeModelSortConvertIterToChildIter sortedModel iter
       issue <- View.treeModelGetRow model cIter
       set render [View.cellText := displayFunc issue]

-- | Map column sorting way to int.
mapSortFunctionsToIds :: Ord b
                        => ListStore a
                        -> TypedTreeModelSort a
                        -> Int
                        -> (a -> b)
                        -> IO()
mapSortFunctionsToIds issuesStore sortedIssueStore funcId compareField = 
  View.treeSortableSetSortFunc sortedIssueStore funcId $ \iter1 iter2 -> do
      issue1 <- View.customStoreGetRow issuesStore iter1
      issue2 <- View.customStoreGetRow issuesStore iter2
      return (compare (compareField issue1) (compareField issue2))       
