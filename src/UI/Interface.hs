-- | Module that provides entry point to app.
-- All behaviour logic sets here.
module UI.Interface
     ( initApp
     ) where

import Graphics.UI.Gtk

import Control.Monad.Reader
import Control.Concurrent

import Model.Types(ThreadType(..))
import UI.PrimaryInterface
import UI.Dialogs
import UI.TrackedTimeView
import UI.Controllers.ProjectInterfaceController
import UI.Controllers.IssueInterfaceController
import UI.Controllers.FileParsingController
import UI.Controllers.FileWritingController


-- | Inits app and set all behaviour logic.
initApp :: IO ()
initApp = do
  void initGUI

  gui <- builderNew
  builderAddFromFile gui "interface.glade"

  win <- builderGetObject gui castToWindow "appWindow"

  projectsViewItem <- builderGetObject gui castToTreeView "projectsView"
  issuesViewItem   <- builderGetObject gui castToTreeView "issuesView"

  interfaceMainContext <- buildMainContext gui projectsViewItem issuesViewItem
  
  void $ on win objectDestroy $ runReaderT saveStateAndQuit interfaceMainContext

  (issueDialog, issueActionButton) <- initIssueDialog gui
  trackedTimeDialog                <- initTrackedTimeDialog gui
  fileChooserDialog                <- initFileChooserDialog win
  folderChooserDialog              <- initFolderChooserDialog win

  addProjectButton             <- builderGetObject gui castToButton "addProjectButton"
  removeProjectButton          <- builderGetObject gui castToButton "removeProjectButton"
  clearProjectsButton          <- builderGetObject gui castToButton "clearProjectsButton"
  addIssueButton               <- builderGetObject gui castToButton "addIssueButton"
  removeIssueButton            <- builderGetObject gui castToButton "removeIssueButton"
  clearIssuesButton            <- builderGetObject gui castToButton "clearIssuesButton"
  showProjectTrackedTimeButton <- builderGetObject gui castToButton "projectTrackedTimeButton"


  exportProjectsItem   <- builderGetObject gui castToMenuItem "exportProjectsItem"
  importProjectsItem   <- builderGetObject gui castToMenuItem "importProjectsItem"
  importIssuesItem     <- builderGetObject gui castToMenuItem "importIssuesItem"

  void $ on exportProjectsItem menuItemActivated
    $ (runReaderT $ exportProjects folderChooserDialog) interfaceMainContext

  void $ on importProjectsItem menuItemActivated
    $ (runReaderT $ importProjects fileChooserDialog) interfaceMainContext

  void $ on importIssuesItem menuItemActivated
    $ (runReaderT $ importIssues fileChooserDialog) interfaceMainContext

  void $ on addProjectButton buttonActivated
    $ runReaderT addProject interfaceMainContext

  void $ on removeProjectButton buttonActivated
    $ runReaderT removeProject interfaceMainContext

  void $ on clearProjectsButton buttonActivated
    $ runReaderT clearProjects interfaceMainContext

  void $ on showProjectTrackedTimeButton buttonActivated
    $ (runReaderT $ showProjectTrackedTime trackedTimeDialog) interfaceMainContext

  void $ on projectsViewItem cursorChanged
    $ runReaderT (displayIssues GtkThread) interfaceMainContext

  void $ on addIssueButton buttonActivated
    $ (runReaderT $ addIssue issueDialog issueActionButton) interfaceMainContext

  void $ on removeIssueButton buttonActivated
    $ runReaderT removeIssue interfaceMainContext

  void $ on clearIssuesButton buttonActivated
    $ runReaderT clearIssues interfaceMainContext

  void $ on issuesViewItem cursorChanged
    $ runReaderT writeActiveIssue interfaceMainContext

  void $ on issuesViewItem rowActivated
    $ \path _ -> (runReaderT $ displayIssueInformation issueDialog issueActionButton path) interfaceMainContext


  -- | This block responsible for time ticking show.
  -- Won't cause any data race, because postGUISync
  -- sends action to main Gtk thread.
  void $ forkIO $
    sequence_ $ repeat ( do
      threadDelay 500000
      postGUISync $ runReaderT (displayIssues TimeHelperThread) interfaceMainContext
   )

  widgetShowAll win
  mainGUI

