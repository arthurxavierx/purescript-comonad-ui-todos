module Todos.Store.Tasks where

import Prelude

import Control.Comonad.Store (Store, store)
import Control.Monad.State (State, modify)
import Data.Array (filter)
import Data.Foldable (fold)
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (TasksModel)
import UI.React (ReactComponent, ReactUI)

type Space = Store TasksModel
type Action = State TasksModel

tasksComponent :: TasksModel -> ReactComponent Space Action
tasksComponent init = store render init
  where
    render :: TasksModel -> ReactUI (Action Unit)
    render model send =
      D.div [ P.className "Tasks" ] $ fold $ model <#> \task ->
        [ D.div
            [ P.className (if task.done then "Task done" else "Task") ]
            [ D.span
                [ P.onClick \_ -> send $ pure (toggleDone task.id)
                ]
                [ D.text task.description ]
            , D.button
                [ P._type "button"
                , P.onClick \_ -> send $ pure (removeTask task.id)
                ]
                [ D.text "×" ]
            ]
        ]

toggleDone :: Int -> Action Unit
toggleDone id = void $ modify $ map \task ->
  if task.id == id then task { done = not task.done } else task

removeTask :: Int -> Action Unit
removeTask id = void $ modify $ filter ((_ /= id) <<< _.id)
