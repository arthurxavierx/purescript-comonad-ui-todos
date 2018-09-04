module Todos.Persistence where

import Prelude

import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Todos.Model (TasksModel, tasksInit)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

newtype PersistedTasks = PersistedTasks TasksModel
derive instance newtypePersistedTasks :: Newtype PersistedTasks _
derive newtype instance readForeignPersistedTasks :: ReadForeign PersistedTasks
derive newtype instance writeForeignPersistedTasks :: WriteForeign PersistedTasks

save :: String -> TasksModel -> Effect Unit
save key tasks = do
  let model = PersistedTasks tasks
  setItem key (writeJSON model) =<< localStorage =<< window

load :: String -> Effect TasksModel
load key = do
  modelM <- getItem key =<< localStorage =<< window
  pure $ fromMaybe tasksInit do
    PersistedTasks tasks <- hush <<< readJSON =<< modelM
    pure $ tasks

keyCofree :: String
keyCofree = "todos_Cofree"

keyMoore :: String
keyMoore = "todos_Moore"

keyStore :: String
keyStore = "todos_Store"
