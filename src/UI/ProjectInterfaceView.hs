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

setupProjectsView :: TreeViewClass view
                  => view
                  -> ListStore Project
                  -> IO ()
setupProjectsView view model = do
  View.treeViewSetHeadersVisible view True

  nameCol <- View.treeViewColumnNew
  createdAtCol <- View.treeViewColumnNew
  recordedCol <- View.treeViewColumnNew


  View.treeViewColumnSetTitle nameCol "Project name"
  View.treeViewColumnSetTitle createdAtCol "Created"
  View.treeViewColumnSetTitle recordedCol "Total time recorded"

  nameRender      <- View.cellRendererTextNew
  createAtdRender <- View.cellRendererTextNew
  recordedRender  <- View.cellRendererTextNew

  View.cellLayoutPackStart nameCol nameRender True
  View.cellLayoutPackStart createdAtCol createAtdRender True
  View.cellLayoutPackStart recordedCol recordedRender True

  View.cellLayoutSetAttributes nameCol nameRender model $ \row -> [ View.cellText := row^.projectName ]
  View.cellLayoutSetAttributes createdAtCol createAtdRender model $ \row -> [ View.cellText := show $ row^.projectCreationDate ]
  View.cellLayoutSetAttributes recordedCol recordedRender model $ \row -> [ View.cellText := show $ row^.projectTimeRecorded ]

  View.treeViewAppendColumn view nameCol
  View.treeViewAppendColumn view createdAtCol
  View.treeViewAppendColumn view recordedCol
  return ()

initProjectUiFieldBundle :: Builder -> IO ProjectUiFieldsBundle
initProjectUiFieldBundle gui = do
  projectNameField <- builderGetObject gui castToEntry "projectNameField"
  return ProjectUiFieldsBundle {_projectNameField = projectNameField}
