module Todos.Store.App where

import Prelude

import Control.Comonad (extract, (=>>))
import Control.Comonad.Store (StoreT(StoreT), pos)
import Control.Comonad.Trans.Class (lower)
import Control.Monad.State (StateT, get, modify, put)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Array (filter, length)
import Data.Tuple (Tuple(..))
import React as R
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (AppModel, TasksModel, appInit)
import Todos.Persistence (keyStore, save) as Persistence
import Todos.Store.Tasks as Tasks
import UI as UI
import UI.React (ReactComponent, ReactUI)
import Unsafe.Coerce (unsafeCoerce)

type AppSpace = StoreT AppModel Tasks.Space
type AppAction = StateT AppModel Tasks.Action

appComponent :: forall eff. TasksModel -> ReactComponent (dom :: DOM | eff) AppSpace AppAction
appComponent tasksInit =
  StoreT (Tuple (Tasks.tasksComponent tasksInit =>> render <<< UI.liftComponentT) (appInit tasksInit))
  =>>
    UI.effect \component -> do
      Persistence.save Persistence.keyStore (pos $ lower component)

  where
    render :: ReactComponent (dom :: DOM | eff) Tasks.Space AppAction -> AppModel -> ReactUI (dom :: DOM | eff) AppAction
    render child model send =
      D.form
        [ P.className "App"
        , P.onSubmit \event -> send do
            _ <- R.preventDefault event
            pure createTask
        ]
        [ D.input
            [ P._type "text"
            , P.placeholder "What needs to be done?"
            , P.value model.field
            , P.onChange \event -> send do
                let value = (unsafeCoerce event).target.value
                pure $ modify (_ { field = value })
            ]
            []
        , extract child send
        , D.small'
            [ D.text $ show (length $ filter _.done (pos child)) <> " tasks completed"
            ]
        ]

createTask :: AppAction Unit
createTask = do
  model <- get
  lift $ modify $ append [{ id: model.uid, description: model.field, done: false }]
  put (model { field = "", uid = model.uid + 1 })
