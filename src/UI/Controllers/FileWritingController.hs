
-- | Module provide functions for projects data export.
module UI.Controllers.FileWritingController
     ( exportProjects
     , initFolderChooserDialog
     ) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Control.Monad.Reader
import Control.Lens.Operators

import System.FilePath.Posix

import Model.Types(ContextIO)
import Model.TypesLenses
import Utils.ExportUtils()

-- | Init dialog for export folder choosing.
initFolderChooserDialog :: Window -> IO FileChooserDialog
initFolderChooserDialog win =
  fileChooserDialogNew
              (Just "Chose export folder")
              (Just win)
              FileChooserActionSelectFolder
              [("gtk-cancel"
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]

-- | Export current projects data into file.
exportProjects :: FileChooserDialog -> ContextIO ()
exportProjects dialog = do
  resp <- liftIO $ dialogRun dialog
  case resp of
    ResponseAccept -> do 
      Just folderName <- liftIO $ fileChooserGetFilename dialog
      writeProjects folderName
    _              -> return ()
  liftIO $ widgetHide dialog

-- | Write project to chosen folder.
-- For export file specified name 'exportData.proj'
writeProjects :: FilePath -> ContextIO ()
writeProjects folderPath = do
  context <- ask
  projects <- liftIO $ View.listStoreToList (context^.projectsStore)
  liftIO $ writeFile (folderPath ++ [pathSeparator] ++"exportData.proj") (concatMap show projects)