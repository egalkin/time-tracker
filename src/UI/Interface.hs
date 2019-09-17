module UI.Interface where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as View

import Control.Monad.Reader

import UI.Controllers.ProjectInterfaceController
import UI.Controllers.IssueInterfaceController
import UI.PrimaryInterface
import Model.TypesLenses
import UI.Notifications
import UI.Controllers.FileParsingController
import UI.Dialogs
import Model.TrackedTime
import UI.TrackedTimeView

import System.Directory

initInterface = do
  initGUI

  gui <- builderNew
  builderAddFromFile gui "interface.glade"

  win <- builderGetObject gui castToWindow "appWindow"

  projectsView <- builderGetObject gui castToTreeView "projectsView"
  issuesView   <- builderGetObject gui castToTreeView "issuesView"

  interfaceMainContext <- buildMainContext gui projectsView issuesView
  
  on win objectDestroy $ runReaderT saveStateAndQuit interfaceMainContext

  addIssueDialog       <- initAddIssueDialog gui
  trackedTimeDialog    <- initTrackedTimeDialog gui
  fileChooserDialog    <- initFileChooserDialog win

  insertButton   <- builderGetObject gui castToButton "insert"
  addIssueButton <- builderGetObject gui castToButton "issueButton"
  showIssueTrackedTimeButton <- builderGetObject gui castToButton "issueTrackedTimeButton"
  showProjectTrackedTimeButton <- builderGetObject gui castToButton "projectTrackedTimeButton"
  removeButton  <- builderGetObject gui castToButton "remove"
  clearButton   <- builderGetObject gui castToButton "clear"


  parseItem     <- builderGetObject gui castToMenuItem "parseIssues"

  on parseItem menuItemActivated $ (runReaderT $ openFileChooser fileChooserDialog) interfaceMainContext
  on insertButton buttonActivated $ runReaderT addProject interfaceMainContext
  on projectsView rowActivated $ \path row -> (runReaderT $ displayIssues path row) interfaceMainContext
  on issuesView rowActivated $ \path row -> (runReaderT $ writeCurrentIssue path row) interfaceMainContext
  on addIssueButton buttonActivated $ (runReaderT $ addIssue addIssueDialog) interfaceMainContext
  on removeButton buttonActivated $ runReaderT removeProject interfaceMainContext
  on clearButton buttonActivated $ runReaderT clearProjects interfaceMainContext
  on showIssueTrackedTimeButton buttonActivated $ (runReaderT $ showIssueTrackedTime trackedTimeDialog) interfaceMainContext
  on showProjectTrackedTimeButton buttonActivated $ (runReaderT $ showProjectTrackedTime trackedTimeDialog) interfaceMainContext

  widgetShowAll win
  mainGUI
