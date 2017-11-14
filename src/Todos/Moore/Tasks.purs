module Todos.Moore.Tasks where

import Prelude

import Data.Foldable (fold)
import Data.Machine.Moore (Comoore, Moore, action, unfoldMoore)
import Data.Tuple (Tuple(..))
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (TasksModel, Task)
import UI.React (ReactComponent, ReactUI)

data Input = AddTask Task | ToggleDone Int

type Space = Moore Input
type Action = Comoore Input

tasksComponent :: forall eff. TasksModel -> ReactComponent eff Space Action
tasksComponent init = unfoldMoore (\model -> Tuple (render model) (update model)) init
  where
    update :: TasksModel -> Input -> TasksModel
    update model input =
      case input of
        AddTask task -> [task] <> model
        ToggleDone id -> model <#> \task ->
          if task.id == id then task { done = not task.done } else task

    render :: TasksModel -> ReactUI eff Action
    render model send =
      D.div' $ fold $ model <#> \({ id, description, done }) ->
        [ D.div
            [ P.onClick \_ -> send $ pure $ action (ToggleDone id)
            , P.style
                { textDecoration: if done then "line-through" else "none"
                }
            ]
            [ D.text description
            ]
        ]
