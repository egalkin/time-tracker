module UI.ProjectInterfaceView 
     ( initProjectUiFieldBundle
     , setupProjectsView
     ) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Model.Types(ProjectUiFieldsBundle(..))
import Model.Project
import Model.TypesLenses
import Control.Lens.Operators
import Utils.ViewUtils

setupProjectsView :: TreeViewClass view
                  => view
                  -> ListStore Project
                  -> TypedTreeModelSort Project
                  -> IO ()
setupProjectsView view projectsStore sortedProjectsStore = do

  View.treeViewSetHeadersVisible view True

  mapSortFunctionsToIds projectsStore sortedProjectsStore 1 (^.projectName)
  mapSortFunctionsToIds projectsStore sortedProjectsStore 2 (^.projectCreationDate)

  nameCol <- View.treeViewColumnNew
  createdAtCol <- View.treeViewColumnNew
 
  View.treeViewColumnSetTitle nameCol "Project name"
  View.treeViewColumnSetTitle createdAtCol "Created"

  renderNameCol      <- View.cellRendererTextNew
  renderCreatedAtCol <- View.cellRendererTextNew

  View.cellLayoutPackStart nameCol renderNameCol True
  View.cellLayoutPackStart createdAtCol renderCreatedAtCol True


  mapModelsFields nameCol renderNameCol projectsStore sortedProjectsStore (^.projectName)
  mapModelsFields createdAtCol renderCreatedAtCol projectsStore sortedProjectsStore (show . (^.projectCreationDate))

  View.treeViewAppendColumn view nameCol
  View.treeViewAppendColumn view createdAtCol
  
  View.treeViewColumnSetSortColumnId nameCol 1
  View.treeViewColumnSetSortColumnId createdAtCol 2

initProjectUiFieldBundle :: Builder -> IO ProjectUiFieldsBundle
initProjectUiFieldBundle gui = do
  projectNameField <- builderGetObject gui castToEntry "projectNameField"
  return ProjectUiFieldsBundle {_projectNameField = projectNameField}
