module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToNonElementParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.NonElementParentNode (getElementById) as DOM
import DOM.Node.Types (ElementId(..)) as DOM
import Data.Traversable (for_)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Todos.Cofree.App (appComponent) as Cofree
import Todos.Moore.App (appComponent) as Moore
import Todos.Persistence (keyCofree, keyMoore, keyStore, load) as Persistence
import Todos.Store.App (appComponent) as Store
import UI.React (toReact)

main :: forall eff. Eff (dom :: DOM.DOM | eff) Unit
main = do
  document <- DOM.window >>= DOM.document
  appDiv <- DOM.getElementById (DOM.ElementId "app") (DOM.htmlDocumentToNonElementParentNode document)
  ui <- loadUI
  for_ appDiv (render ui)

  where
    loadUI = do
      tasksCofree <- Persistence.load Persistence.keyCofree
      tasksMoore <- Persistence.load Persistence.keyMoore
      tasksStore <- Persistence.load Persistence.keyStore
      pure $
        D.div
          [ P.className "Container" ]
          [ D.div [ P.className "AppContainer" ]
              [ D.h4' [ D.text "Cofree comonad" ]
              , toReact $ Cofree.appComponent tasksCofree
              ]
          , D.div [ P.className "AppContainer" ]
              [ D.h4' [ D.text "Moore machine" ]
              , toReact $ Moore.appComponent tasksMoore
              ]
          , D.div [ P.className "AppContainer" ]
              [ D.h4' [ D.text "Store comonad" ]
              , toReact $ Store.appComponent tasksStore
              ]
          ]
