module Todos.Moore.Tasks where

import Prelude

import DOM (DOM)
import Data.Array (filter)
import Data.Foldable (fold)
import Data.Machine.Moore (ComooreT, action)
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (TasksModel, Task)
import Todos.Persistence (keyMoore, save) as Persistence
import UI.React (ReactUI)

data Input
  = AddTask Task
  | RemoveTask Int
  | ToggleDone Int

type Action m = ComooreT Input m

tasksComponent :: forall m eff. Monad m => TasksModel -> ReactUI (dom :: DOM | eff) (Action m)
tasksComponent model send =
  D.div [ P.className "Tasks" ] $ fold $ model <#> \task ->
    [ D.div
        [ P.className (if task.done then "Task done" else "Task") ]
        [ D.span
            [ P.onClick \_ -> send $ saveAndAction (ToggleDone task.id)
            ]
            [ D.text task.description ]
        , D.button
            [ P._type "button"
            , P.onClick \_ -> send $ saveAndAction (RemoveTask task.id)
            ]
            [ D.text "×" ]
        ]
    ]
  where
    saveAndAction input = do
      Persistence.save Persistence.keyMoore (tasksUpdate model input)
      pure $ action input

tasksUpdate :: TasksModel -> Input -> TasksModel
tasksUpdate model input =
  case input of
    AddTask task -> [task] <> model
    RemoveTask id -> filter ((_ /= id) <<< _.id) model
    ToggleDone id -> model <#> \task ->
      if task.id == id then task { done = not task.done } else task
