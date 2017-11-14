module Todos.Cofree.Tasks where

import Prelude

import Control.Comonad.Cofree (Cofree, unfoldCofree)
import Control.Monad.Free (Free, liftF)
import Data.Array (filter, length)
import Data.Foldable (fold)
import Data.Functor.Pairing (class Pairing)
import Data.Tuple (Tuple(..), fst, snd)
import React.DOM as D
import React.DOM.Props as P
import Todos.Model (TasksModel, Task)
import UI.React (ReactComponent, ReactUI)

data Query a
  = ToggleDone Int a
  | AddTask Task a
  | GetDones (Int -> a)
derive instance functorQuery :: Functor Query

toggleDone :: Int -> Action Unit
toggleDone id = liftF $ ToggleDone id unit

addTask :: Task -> Action Unit
addTask task = liftF $ AddTask task unit

getDones :: forall a. (Int -> a) -> Action a
getDones f = liftF $ GetDones f

newtype Interpreter a = Interpreter
  { toggleDone :: Int -> a
  , addTask :: Task -> a
  , getDones :: Tuple Int a
  }
derive instance functorInterpreter :: Functor Interpreter

instance pairQueryInterpreter :: Pairing Query Interpreter where
  pair f (ToggleDone id a) (Interpreter i) = f a (i.toggleDone id)
  pair f (AddTask task a) (Interpreter i) = f a (i.addTask task)
  pair f (GetDones g) (Interpreter i) = f (g (fst i.getDones)) (snd i.getDones)

--------------------------------------------------------------------------------

type Space = Cofree Interpreter
type Action = Free Query

tasksComponent :: forall eff. TasksModel -> ReactComponent eff Space Action
tasksComponent init = unfoldCofree render eval init
  where
    eval :: TasksModel -> Interpreter TasksModel
    eval tasks = Interpreter
      { toggleDone: \id ->
          tasks <#> \task ->
            if task.id == id then task { done = not task.done } else task
      , addTask: \task -> [task] <> tasks
      , getDones: Tuple (length $ filter _.done tasks) tasks
      }

    render :: TasksModel -> ReactUI eff Action
    render model send =
      D.div' $ fold $ model <#> \({ id, description, done }) ->
        [ D.div
            [ P.onClick \_ -> send $ pure (toggleDone id)
            , P.style
                { textDecoration: if done then "line-through" else "none"
                }
            ]
            [ D.text description
            ]
        ]
