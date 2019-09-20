{-# LANGUAGE RecordWildCards #-}

module Parsers.ProjectParser 
     ( parseProjectsFromFile
     )where


import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

import Data.Time.Format
import Data.Time.Calendar

import Control.Applicative
import Control.Monad (void)

import Model.Project
import Parsers.IssueParser
import Parsers.BaseParsers

project :: Parser Project
project = do
  _projectName         <- letterString
  char '|'
  _projectCreationDate <- creationDate
  char ':'
  some newline
  _projectIssues       <- parseProjectIssues
  return Project {..}

parseProjects :: Parser [Project]
parseProjects = many project

parseProjectsFromFile :: FilePath -> IO (Either (ParseError Char Dec) [Project])
parseProjectsFromFile path = do
  contents <- readFile path
  return $ parse parseProjects "" contents 