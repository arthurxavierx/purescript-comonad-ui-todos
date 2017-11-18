module Todos.Moore.App where

import Prelude

import DOM (DOM)
import Data.Array (filter, length)
import Data.Machine.Moore (Comoore, Moore, action, mapAction, unfoldMoore)
import Data.Tuple (Tuple(Tuple), fst)
import React as R
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (GlobalModel, TasksModel, globalInit)
import Todos.Moore.Tasks as Tasks
import Todos.Persistence (keyMoore, save) as Persistence
import UI.React (ReactComponent, ReactUI, ReactEff)
import Unsafe.Coerce (unsafeCoerce)

data AppInput
  = ChangeField String
  | IncrementUID
  | TasksAction Tasks.Input

type AppSpace = Moore AppInput
type AppAction = Comoore AppInput

appComponent :: forall eff. TasksModel -> ReactComponent (dom :: DOM | eff) AppSpace AppAction
appComponent tasksInit = unfoldMoore step (globalInit tasksInit)
  where
    step model =
      Tuple
        (render model)
        (fst <<< update model)

    update :: GlobalModel -> AppInput -> Tuple GlobalModel (ReactEff (dom :: DOM | eff) Unit)
    update model input = Tuple newModel save
      where
        newModel =
          case input of
            ChangeField field -> model { field = field }
            IncrementUID -> model { uid = model.uid + 1 }
            TasksAction tasksInput ->
              model { tasks = Tasks.tasksUpdate model.tasks tasksInput }

        save = Persistence.save Persistence.keyMoore newModel.tasks

    render :: GlobalModel -> ReactUI (dom :: DOM | eff) AppAction
    render model send =
      D.form
        [ P.className "App"
        , P.onSubmit \event -> send do
            _ <- R.preventDefault event
            pure $ createTask model
        ]
        [ D.input
            [ P._type "text"
            , P.placeholder "What needs to be done?"
            , P.value model.field
            , P.onChange \event -> send do
                let value = (unsafeCoerce event).target.value
                pure $ action (ChangeField value)
            ]
            []
        , Tasks.tasksComponent model.tasks (send <<< map (mapAction TasksAction))
        , D.small'
            [ D.text $ show (length $ filter _.done model.tasks) <> " tasks completed"
            ]
        ]

createTask :: GlobalModel -> AppAction Unit
createTask model = do
  action (ChangeField "")
  action IncrementUID
  action (TasksAction $ Tasks.AddTask { id: model.uid, description: model.field, done: false })
