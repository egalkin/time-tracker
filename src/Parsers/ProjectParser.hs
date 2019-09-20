{-# LANGUAGE RecordWildCards #-}

-- | Module provides parser for projects parsing.
module Parsers.ProjectParser 
     ( parseProjectsFromFile
     )where

import Model.Project
import Parsers.IssueParser
import Parsers.BaseParsers

import Text.Megaparsec
import Text.Megaparsec.String

import Control.Monad (void)

-- | Parse project.
project :: Parser Project
project = do
  _projectName         <- letterString
  void $ char '|'
  _projectCreationDate <- creationDate
  void $ char ':'
  void $ some newline
  _projectIssues       <- parseProjectIssues
  return Project {..}

-- | Parse projects.
parseProjects :: Parser [Project]
parseProjects = many project

-- | Parse projects data from file.
parseProjectsFromFile :: FilePath -> IO (Either (ParseError Char Dec) [Project])
parseProjectsFromFile path = do
  contents <- readFile path
  return $ parse parseProjects "" contents 