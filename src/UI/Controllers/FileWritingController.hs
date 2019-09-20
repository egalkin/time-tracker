module UI.Controllers.FileWritingController where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Model.Types(ContextIO)
import Control.Monad.Reader
import Control.Lens.Operators
import Model.TypesLenses
import Utils.ExportUtils

initFolderChooserDialog :: Window -> IO FileChooserDialog
initFolderChooserDialog win =
  fileChooserDialogNew
              (Just $ "Demo of the standard dialog to select "
                         ++ "an existing file")
              (Just win)
              FileChooserActionSelectFolder
              [("gtk-cancel"
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]


exportProjects :: FileChooserDialog -> ContextIO ()
exportProjects dialog = do
  response <- lift $ dialogRun dialog
  case response of
    ResponseAccept -> do Just folderName <- lift $ fileChooserGetFilename dialog
                         writeProjects folderName
    ResponseCancel -> return ()
    ResponseDeleteEvent -> return ()
  lift $ widgetHide dialog

writeProjects :: FilePath -> ContextIO ()
writeProjects folderPath = do
  context <- ask
  projects <- lift $ View.listStoreToList (context^.projectsStore)
  lift $ writeFile (folderPath ++ "\\exportData.proj") (concatMap show projects)