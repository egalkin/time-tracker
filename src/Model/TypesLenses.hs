{-# LANGUAGE TemplateHaskell #-}

-- | Module contains lenses for all Data types.
module Model.TypesLenses where

import Control.Lens
import Model.Types
import Model.Project
import Model.Issue
import Model.TrackedTime

makeLenses ''Project
makeLenses ''ProjectUiFieldsBundle
makeLenses ''Issue
makeLenses ''IssueUiFieldsBundle
makeLenses ''InterfaceMainContext
makeLenses ''TrackedTime
