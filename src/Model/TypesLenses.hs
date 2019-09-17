{-# LANGUAGE TemplateHaskell #-}

module Model.TypesLenses where

import Control.Lens
import Model.Types
import Model.Project
import Model.Issue

makeLenses ''Project
makeLenses ''ProjectUiFieldsBundle
makeLenses ''Issue
makeLenses ''IssueUiFieldsBundle
makeLenses ''InterfaceMainContext
