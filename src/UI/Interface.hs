module UI.Interface where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as View

import Control.Monad.Reader

import UI.Controllers.ProjectInterfaceController
import UI.Controllers.IssueInterfaceController
import UI.PrimaryInterface
import Model.TypesLenses
import UI.Notifications
import UI.Controllers.FileParsingController
import UI.Controllers.FileWritingController
import UI.Dialogs
import Model.TrackedTime
import UI.TrackedTimeView
import Model.Types(ThreadType(..))

import System.Directory
import Control.Concurrent

import Utils.TimeUtils

initInterface = do
  initGUI

  gui <- builderNew
  builderAddFromFile gui "interface.glade"

  win <- builderGetObject gui castToWindow "appWindow"

  projectsView <- builderGetObject gui castToTreeView "projectsView"
  issuesView   <- builderGetObject gui castToTreeView "issuesView"

  interfaceMainContext <- buildMainContext gui projectsView issuesView
  
  on win objectDestroy $ runReaderT saveStateAndQuit interfaceMainContext

  (issueDialog, issueActionButton) <- initIssueDialog gui
  trackedTimeDialog                <- initTrackedTimeDialog gui
  fileChooserDialog                <- initFileChooserDialog win
  folderChooserDialog              <- initFolderChooserDialog win

  insertButton                 <- builderGetObject gui castToButton "insert"
  addIssueButton               <- builderGetObject gui castToButton "issueButton"
  showProjectTrackedTimeButton <- builderGetObject gui castToButton "projectTrackedTimeButton"
  removeIssueButton            <- builderGetObject gui castToButton "removeIssueButton"
  clearIssuesButton            <- builderGetObject gui castToButton "clearIssuesButton"
  removeButton                 <- builderGetObject gui castToButton "remove"
  clearButton                  <- builderGetObject gui castToButton "clear"


  exportProjectsItem   <- builderGetObject gui castToMenuItem "exportProjectsItem"
  importProjectsItem   <- builderGetObject gui castToMenuItem "importProjectsItem"
  importIssuesItem     <- builderGetObject gui castToMenuItem "importIssuesItem"

  on exportProjectsItem menuItemActivated
    $ (runReaderT $ exportProjects folderChooserDialog) interfaceMainContext

  on importProjectsItem menuItemActivated
    $ (runReaderT $ importProjects fileChooserDialog) interfaceMainContext

  on importIssuesItem menuItemActivated
    $ (runReaderT $ importIssues fileChooserDialog) interfaceMainContext

  on insertButton buttonActivated
    $ runReaderT addProject interfaceMainContext

  on projectsView cursorChanged
    $ runReaderT (displayIssues GtkThread) interfaceMainContext

  on issuesView cursorChanged
    $ runReaderT writeActiveIssue interfaceMainContext

  on removeIssueButton buttonActivated
    $ runReaderT removeIssue interfaceMainContext

  on clearIssuesButton buttonActivated
    $ runReaderT clearIssues interfaceMainContext

  on issuesView rowActivated
    $ \path _ -> (runReaderT $ displayIssueInformation issueDialog issueActionButton path) interfaceMainContext

  on addIssueButton buttonActivated
    $ (runReaderT $ addIssue issueDialog issueActionButton) interfaceMainContext

  on removeButton buttonActivated
    $ runReaderT removeProject interfaceMainContext

  on clearButton buttonActivated
    $ runReaderT clearProjects interfaceMainContext

  on showProjectTrackedTimeButton buttonActivated
    $ (runReaderT $ showProjectTrackedTime trackedTimeDialog) interfaceMainContext

  forkIO $
   sequence_ $ repeat ( do
   threadDelay 1000000
   postGUISync $ runReaderT (displayIssues TimeHelperThread) interfaceMainContext
   )

  widgetShowAll win
  mainGUI
