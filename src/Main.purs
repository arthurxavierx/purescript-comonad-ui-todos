module Main where

import Prelude

import Control.Comonad (class Comonad)
import Control.Monad.Eff (Eff)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Document (body) as DOM
import DOM.HTML.Types (htmlElementToElement) as DOM
import DOM.HTML.Window (document) as DOM
import Data.Functor.Pairing (class Pairing)
import Data.Traversable (for_)
import React as R
import React.DOM (div', h3', text)
import ReactDOM (render)
import Todos.Cofree.App (appComponent) as Cofree
import Todos.Moore.App (appComponent) as Moore
import Todos.Persistence (keyCofree, keyMoore, keyStore, load) as Persistence
import Todos.Store.App (appComponent) as Store
import UI.React (ReactComponent)
import UI.React as UI.React

main :: forall eff. Eff (dom :: DOM.DOM | eff) Unit
main = do
  body <- DOM.window >>= DOM.document >>= DOM.body
  ui <- loadUI
  for_ body (render ui <<< DOM.htmlElementToElement)

  where
    loadUI = do
      tasksCofree <- Persistence.load Persistence.keyCofree
      tasksMoore <- Persistence.load Persistence.keyMoore
      tasksStore <- Persistence.load Persistence.keyStore
      pure $
        div'
          [ h3' [ text "Cofree" ]
          , toReact $ Cofree.appComponent tasksCofree
          , h3' [ text "Moore" ]
          , toReact $ Moore.appComponent tasksMoore
          , h3' [ text "Store" ]
          , toReact $ Store.appComponent tasksStore
          ]

toReact :: forall w m eff. Comonad w => Pairing m w => ReactComponent eff w m -> R.ReactElement
toReact = flip R.createFactory {} <<< UI.React.explore
