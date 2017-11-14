module Todos.Persistence where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (localStorage)
import DOM.WebStorage.Storage (getItem, setItem)
import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Todos.Model (TasksModel, tasksInit)

newtype PersistedTasks = PersistedTasks TasksModel
derive instance newtypePersistedTasks :: Newtype PersistedTasks _
derive newtype instance readForeignPersistedTasks :: ReadForeign PersistedTasks
derive newtype instance writeForeignPersistedTasks :: WriteForeign PersistedTasks

save :: forall eff. String -> TasksModel -> Eff (dom :: DOM | eff) Unit
save key tasks = do
  let model = PersistedTasks tasks
  setItem key (writeJSON model) =<< localStorage =<< window

load :: forall eff. String -> Eff (dom :: DOM | eff) TasksModel
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
