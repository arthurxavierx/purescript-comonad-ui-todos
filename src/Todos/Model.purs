module Todos.Model where

import Data.Array (length)

type Task =
  { id :: Int
  , description :: String
  , done :: Boolean
  }

type TasksModel = Array Task

type AppModel =
  { field :: String
  , uid :: Int
  }

appInit :: TasksModel -> AppModel
appInit tasks = { field: "", uid: length tasks }

tasksInit :: TasksModel
tasksInit = [{ id: 0, description: "Test", done: false }]
