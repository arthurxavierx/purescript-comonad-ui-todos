module Todos.Cofree.App where

import Prelude

import Control.Comonad (extract, (=>>))
import Control.Comonad.Cofree.Trans (CofreeT, unfoldCofreeT)
import Control.Comonad.Pairing (select)
import Control.Comonad.Trans.Class (lower)
import Control.Monad.Free.Trans (FreeT, liftFreeT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Functor.Pairing (class Pairing)
import Data.Tuple (Tuple(..))
import React as R
import React.DOM as D
import React.DOM.Props as P
import Todos.Cofree.Tasks as Tasks
import Todos.Model (AppModel, TasksModel, appInit)
import Todos.Persistence (keyCofree, save) as Persistence
import UI as UI
import UI.React (ReactComponent, ReactUI)
import Unsafe.Coerce (unsafeCoerce)

type AppSpace = CofreeT AppInterpreter Tasks.Space
type AppAction = FreeT AppQuery Tasks.Action

appComponent :: forall eff. TasksModel -> ReactComponent (dom :: DOM | eff) AppSpace AppAction
appComponent tasksInit =
  unfoldCofreeT step (Tuple (Tasks.tasksComponent tasksInit) (appInit tasksInit))
  =>>
    UI.effect \component ->
      select (lower component) $
        Tasks.getTasks $ Persistence.save Persistence.keyCofree

  where
    step (Tuple childComponent model) =
      childComponent
      =>> \child -> Tuple (render (UI.liftComponentT child) model) (Tuple child <$> eval model)

    eval :: AppModel -> AppInterpreter AppModel
    eval model = AppInterpreter
      { changeField: \field -> model { field = field }
      , incrementUID: model { uid = model.uid + 1 }
      }

    render :: ReactComponent (dom :: DOM | eff) Tasks.Space AppAction -> AppModel -> ReactUI (dom :: DOM | eff) AppAction
    render child model send =
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
                pure $ changeField value
            ]
            []
        , extract child send
        , select child $ Tasks.getDones \dones ->
            D.small' [ D.text $ show dones <> " tasks completed" ]
        ]

--------------------------------------------------------------------------------

data AppQuery a
  = ChangeField String a
  | IncrementUID a
derive instance functorAppQuery :: Functor AppQuery

changeField :: String -> AppAction Unit
changeField field = liftFreeT $ ChangeField field unit

incrementUID :: AppAction Unit
incrementUID = liftFreeT $ IncrementUID unit

createTask :: AppModel -> AppAction Unit
createTask model = do
  changeField ""
  incrementUID
  lift $ Tasks.addTask { id: model.uid, description: model.field, done: false }

newtype AppInterpreter a = AppInterpreter
  { changeField :: String -> a
  , incrementUID :: a
  }
derive instance functorAppInterpreter :: Functor AppInterpreter

instance pairAppQueryAppInterpreter :: Pairing AppQuery AppInterpreter where
  pair f (ChangeField field a) (AppInterpreter i) = f a (i.changeField field)
  pair f (IncrementUID a) (AppInterpreter i) = f a i.incrementUID
