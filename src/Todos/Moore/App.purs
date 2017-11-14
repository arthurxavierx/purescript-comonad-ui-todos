module Todos.Moore.App where

import Prelude

import Control.Comonad (extract, (=>>))
import Control.Monad.Trans.Class (lift)
import Data.Machine.Moore (ComooreT, MooreT, action, unfoldMooreT)
import Data.Tuple (Tuple(..))
import React as R
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (AppModel, TasksModel, appInit)
import Todos.Moore.Tasks as Tasks
import UI (liftUIT)
import UI.React (ReactComponent, ReactUI)
import Unsafe.Coerce (unsafeCoerce)

data AppInput = ChangeField String | IncrementUID

type AppSpace = MooreT AppInput Tasks.Space
type AppAction = ComooreT AppInput Tasks.Action

appComponent :: forall eff. TasksModel -> ReactComponent eff AppSpace AppAction
appComponent tasksInit =
  unfoldMooreT
    step
    (Tuple (Tasks.tasksComponent tasksInit) (appInit tasksInit))

  where
    step (Tuple childComponent model) =
      childComponent
      =>> \child -> Tuple (render child model) (Tuple child <<< update model)

    update :: AppModel -> AppInput -> AppModel
    update model input =
      case input of
        ChangeField field -> model { field = field }
        IncrementUID -> model { uid = model.uid + 1 }

    render :: ReactComponent eff Tasks.Space Tasks.Action -> AppModel -> ReactUI eff AppAction
    render child model send =
      D.form
        [ P.onSubmit \event -> send do
            _ <- R.preventDefault event
            pure $ createTask model
        ]
        [ D.input
            [ P._type "text"
            , P.value model.field
            , P.onChange \event -> send do
                let value = (unsafeCoerce event).target.value
                pure $ action (ChangeField value)
            ]
            []
        , D.button' [ D.text $ "Add" ]
        , extract (liftUIT child) send
        ]

createTask :: AppModel -> AppAction Unit
createTask model = do
  action (ChangeField "")
  action IncrementUID
  lift $ action (Tasks.AddTask { id: model.uid, description: model.field, done: false })
