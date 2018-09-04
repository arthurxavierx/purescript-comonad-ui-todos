module Main where

import Prelude

import Data.Traversable (for_)
import Effect (Effect)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Todos.Cofree.App (appComponent) as Cofree
import Todos.Moore.App (appComponent) as Moore
import Todos.Persistence (keyCofree, keyMoore, keyStore, load) as Persistence
import Todos.Store.App (appComponent) as Store
import UI.React (toReact)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM

main :: Effect Unit
main = do
  document <- DOM.window >>= DOM.document
  appDiv <- DOM.getElementById "app" (DOM.toNonElementParentNode document)
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
