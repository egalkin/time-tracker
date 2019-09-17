{-# LANGUAGE TemplateHaskell #-}

module TypesLenses where

import Control.Lens
import Types

makeLenses ''Project
makeLenses ''ProjectUiFieldsBundle
makeLenses ''Issue
makeLenses ''IssueUiFieldsBundle
makeLenses ''InterfaceMainContext
