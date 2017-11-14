module Todos.Store.Tasks where

import Prelude

import Control.Comonad.Store (Store, store)
import Control.Monad.State (State, modify)
import Data.Foldable (fold)
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (TasksModel)
import UI.React (ReactComponent, ReactUI)

type Space = Store TasksModel
type Action = State TasksModel

tasksComponent :: forall eff. TasksModel -> ReactComponent eff Space Action
tasksComponent init = store render init
  where
    render :: TasksModel -> ReactUI eff Action
    render model send =
      D.div' $ fold $ model <#> \({ id, description, done }) ->
        [ D.div
            [ P.onClick \_ -> send $ pure $ modify $ map \task ->
                if task.id == id then task { done = not task.done } else task
            , P.style
                { textDecoration: if done then "line-through" else "none"
                }
            ]
            [ D.text description
            ]
        ]
