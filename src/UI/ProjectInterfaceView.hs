{-# LANGUAGE RecordWildCards #-}

-- | This model set up view model for displaying projects data.
module UI.ProjectInterfaceView 
     ( initProjectUiFieldBundle
     , setupProjectsView
     ) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Control.Monad(void)
import Control.Lens.Operators

import Model.Types(ProjectUiFieldsBundle(..))
import Model.Project
import Model.TypesLenses
import Utils.ViewUtils


-- | Set up projects TreeView configuration.
setupProjectsView :: TreeViewClass view
                  => view
                  -> ListStore Project
                  -> TypedTreeModelSort Project
                  -> IO ()
setupProjectsView view store sortedStore = do

  View.treeViewSetHeadersVisible view True

  mapSortFunctionsToIds store sortedStore 1 (^.projectName)
  mapSortFunctionsToIds store sortedStore 2 (^.projectCreationDate)

  nameCol <- View.treeViewColumnNew
  createdAtCol <- View.treeViewColumnNew
 
  View.treeViewColumnSetTitle nameCol "Project name"
  View.treeViewColumnSetTitle createdAtCol "Created"

  renderNameCol      <- View.cellRendererTextNew
  renderCreatedAtCol <- View.cellRendererTextNew

  View.cellLayoutPackStart nameCol renderNameCol True
  View.cellLayoutPackStart createdAtCol renderCreatedAtCol True

  setModelsFields nameCol renderNameCol store sortedStore (^.projectName)
  setModelsFields createdAtCol renderCreatedAtCol store sortedStore (show . (^.projectCreationDate))

  void $ View.treeViewAppendColumn view nameCol
  void $ View.treeViewAppendColumn view createdAtCol
  
  View.treeViewColumnSetSortColumnId nameCol 1
  View.treeViewColumnSetSortColumnId createdAtCol 2

-- | Initialize fields with project input fields.
-- Used for project creating.
initProjectUiFieldBundle :: Builder -> IO ProjectUiFieldsBundle
initProjectUiFieldBundle gui = do
  _projectNameField <- builderGetObject gui castToEntry "projectNameField"
  return ProjectUiFieldsBundle {..}
