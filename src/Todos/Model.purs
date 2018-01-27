module Todos.Model where

import Data.Array (length)

type Task =
  { id :: Int
  , description :: String
  , done :: Boolean
  }

type TasksModel = Array Task

tasksInit :: TasksModel
tasksInit = [{ id: 0, description: "Test", done: false }]

type AppModel =
  { field :: String
  , uid :: Int
  }

appInit :: TasksModel -> AppModel
appInit tasks = { field: "", uid: length tasks }


-- | The `GlobalModel` type describes a model of the whole state of the application, as
-- | used by the moore machine example, which models the Elm architecture.
type GlobalModel =
  { field :: String
  , uid :: Int
  , tasks :: TasksModel
  }

globalInit :: TasksModel -> GlobalModel
globalInit tasks = { field: "", uid: length tasks, tasks }
