module Todos.Store.App where

import Prelude

import Control.Comonad (extract, (=>>))
import Control.Comonad.Store (StoreT(StoreT), pos)
import Control.Monad.State (StateT, get, modify, put)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Array (filter, length)
import Data.Tuple (Tuple(..))
import React as R
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (AppModel, TasksModel, Task, appInit)
import Todos.Persistence (keyStore, save) as Persistence
import Todos.Store.Tasks as Tasks
import UI (liftUIT)
import UI.React (ReactComponent, ReactUI)
import Unsafe.Coerce (unsafeCoerce)

type AppSpace = StoreT AppModel Tasks.Space
type AppAction = StateT AppModel Tasks.Action

appComponent :: forall eff. TasksModel -> ReactComponent (dom :: DOM | eff) AppSpace AppAction
appComponent tasksInit =
  StoreT $
    Tuple
      (Tasks.tasksComponent tasksInit =>> render <<< liftUIT)
      (appInit tasksInit)

  where
    render :: ReactComponent (dom :: DOM | eff) Tasks.Space AppAction -> AppModel -> ReactUI (dom :: DOM | eff) AppAction
    render child model send =
      D.form
        [ P.onSubmit \event -> send do
            _ <- R.preventDefault event
            let newTask = { id: model.uid, description: model.field, done: false }
            Persistence.save Persistence.keyStore ([newTask] <> pos child)
            pure $ createTask newTask
        ]
        [ D.input
            [ P._type "text"
            , P.value model.field
            , P.onChange \event -> send do
                let value = (unsafeCoerce event).target.value
                pure $ modify (_ { field = value })
            ]
            []
        , D.button' [ D.text $ "Add" ]
        , extract child send
        , D.span' [ D.text $ show $ length $ filter _.done (pos child) ]
        ]

createTask :: Task -> AppAction Unit
createTask task = do
  model <- get
  lift $ modify $ append [task]
  put (model { field = "", uid = model.uid + 1 })
