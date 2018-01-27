module Todos.Moore.App where

import Prelude

import DOM (DOM)
import Data.Array (filter, length)
import Data.Machine.Moore (Comoore, Moore, action, mapAction, unfoldMoore)
import Data.Tuple (Tuple(Tuple))
import React as R
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (GlobalModel, TasksModel, Task, globalInit)
import Todos.Moore.Tasks as Tasks
import Todos.Persistence (keyMoore, save) as Persistence
import UI.React (ReactComponent, ReactUI)
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
        (update model)

    update :: GlobalModel -> AppInput -> GlobalModel
    update model input =
      case input of
        ChangeField field -> model { field = field }
        IncrementUID -> model { uid = model.uid + 1 }
        TasksAction tasksInput ->
          model { tasks = Tasks.tasksUpdate model.tasks tasksInput }

    render :: GlobalModel -> ReactUI (dom :: DOM | eff) (AppAction Unit)
    render model send =
      D.form
        [ P.className "App"
        , P.onSubmit \event -> send do
            _ <- R.preventDefault event
            let newTask = { id: model.uid, description: model.field, done: false }
            Persistence.save Persistence.keyMoore ([newTask] <> model.tasks)
            pure $ createTask newTask model
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

createTask :: Task -> GlobalModel -> AppAction Unit
createTask newTask model = do
  action (ChangeField "")
  action IncrementUID
  action (TasksAction $ Tasks.AddTask newTask)
