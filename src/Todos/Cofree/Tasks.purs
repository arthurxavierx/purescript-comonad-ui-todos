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
      , removeTask: \id -> filter ((_ /= id) <<< _.id) tasks
      , getDones: Tuple (length $ filter _.done tasks) tasks
      , getTasks: Tuple tasks tasks
      }

    render :: TasksModel -> ReactUI eff Action
    render model send =
      D.div [ P.className "Tasks" ] $ fold $ model <#> \task ->
        [ D.div
            [ P.className (if task.done then "Task done" else "Task") ]
            [ D.span
                [ P.onClick \_ -> send $ pure (toggleDone task.id) ]
                [ D.text task.description ]
            , D.button
                [ P._type "button"
                , P.onClick \_ -> send $ pure (removeTask task.id)
                ]
                [ D.text "×" ]
            ]
        ]

--------------------------------------------------------------------------------

data Query a
  = ToggleDone Int a
  | AddTask Task a
  | RemoveTask Int a
  | GetTasks (Array Task -> a)
  | GetDones (Int -> a)
derive instance functorQuery :: Functor Query

toggleDone :: Int -> Action Unit
toggleDone id = liftF $ ToggleDone id unit

addTask :: Task -> Action Unit
addTask task = liftF $ AddTask task unit

removeTask :: Int -> Action Unit
removeTask id = liftF $ RemoveTask id unit

getDones :: forall a. (Int -> a) -> Action a
getDones f = liftF $ GetDones f

getTasks :: forall a. (Array Task -> a) -> Action a
getTasks f = liftF $ GetTasks f

newtype Interpreter a = Interpreter
  { toggleDone :: Int -> a
  , addTask :: Task -> a
  , removeTask :: Int -> a
  , getDones :: Tuple Int a
  , getTasks :: Tuple (Array Task) a
  }
derive instance functorInterpreter :: Functor Interpreter

instance pairQueryInterpreter :: Pairing Query Interpreter where
  pair f (ToggleDone id a) (Interpreter i) = f a (i.toggleDone id)
  pair f (AddTask task a) (Interpreter i) = f a (i.addTask task)
  pair f (RemoveTask task a) (Interpreter i) = f a (i.removeTask task)
  pair f (GetDones g) (Interpreter i) = f (g (fst i.getDones)) (snd i.getDones)
  pair f (GetTasks g) (Interpreter i) = f (g (fst i.getTasks)) (snd i.getTasks)
