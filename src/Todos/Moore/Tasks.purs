module Todos.Moore.Tasks where

import Prelude

import Data.Array (filter)
import Data.Foldable (fold)
import Data.Machine.Moore (ComooreT, action)
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (TasksModel, Task)
import UI.React (ReactUI)

data Input
  = AddTask Task
  | RemoveTask Int
  | ToggleDone Int

type Action m = ComooreT Input m

tasksComponent :: forall m eff. Monad m => TasksModel -> ReactUI eff (Action m)
tasksComponent model send =
  D.div [ P.className "Tasks" ] $ fold $ model <#> \task ->
    [ D.div
        [ P.className (if task.done then "Task done" else "Task") ]
        [ D.span
            [ P.onClick \_ -> send $ pure $ action (ToggleDone task.id)
            ]
            [ D.text task.description ]
        , D.button
            [ P._type "button"
            , P.onClick \_ -> send $ pure $ action (RemoveTask task.id)
            ]
            [ D.text "×" ]
        ]
    ]

tasksUpdate :: TasksModel -> Input -> TasksModel
tasksUpdate model input =
  case input of
    AddTask task -> [task] <> model
    RemoveTask id -> filter ((_ /= id) <<< _.id) model
    ToggleDone id -> model <#> \task ->
      if task.id == id then task { done = not task.done } else task
