module Exercise2.App.Component.DogBreedList where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Exercise2.App.Data.DogBreedListDTO (DogBreedDisplayArray)
import Halogen as H
import Halogen.HTML as HTML
import Halogen.HTML.Events as Events
import Halogen.Hooks as Hooks

--------------------------------------------------------------------------------

data Message = ViewBreed String (Maybe String)

component :: forall q m. MonadAff m => H.Component q DogBreedDisplayArray Message m
component = Hooks.component \{ outputToken } displayArray -> Hooks.do
  Hooks.pure do
    HTML.div_
      [ HTML.ul_ $ displayArray <#> \(breed /\ subBreeds) -> 
          HTML.li
            [ Events.onClick \_ -> do
                Hooks.raise outputToken (ViewBreed breed Nothing)
            ]
            (
              [ HTML.text breed
              ] <> guard (not $ Array.null subBreeds)
                [ HTML.ul_ $ subBreeds <#> \subBreed ->
                    HTML.li
                      [ Events.onClick \_ -> do
                          Hooks.raise outputToken (ViewBreed breed (Just subBreed))
                      ]
                      [HTML.text subBreed]
                ]
            )
      ]

--------------------------------------------------------------------------------
