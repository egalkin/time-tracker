{-# LANGUAGE TemplateHaskell #-}

-- | Module contains lenses for all Data types.
module Model.TypesLenses where

import Model.Types
import Model.Project
import Model.Issue
import Model.TrackedTime

import Control.Lens

makeLenses ''Project
makeLenses ''ProjectUiFieldsBundle
makeLenses ''Issue
makeLenses ''IssueUiFieldsBundle
makeLenses ''InterfaceMainContext
makeLenses ''TrackedTime
