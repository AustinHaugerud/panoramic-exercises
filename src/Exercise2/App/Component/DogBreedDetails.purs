module Exercise2.App.Component.DogBreedDetails where

import Prelude

import Affjax.Web as Affjax
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Exercise2.App.Data.DogBreedImagesDTO (URL)
import Exercise2.App.Data.ResourceError (ResourceError(..))
import Halogen as H
import Halogen.HTML as HTML
import Halogen.HTML.Events as Events
import Halogen.HTML.Properties as Properties
import Halogen.Hooks as Hooks
import Unsafe.Reference (UnsafeRefEq(..))

--------------------------------------------------------------------------------

data Message = ReturnToList

type ImageUrls = Maybe (Either ResourceError (Array URL))

component :: forall q m. H.Component q ImageUrls Message m 
component = Hooks.component \{ outputToken } urls' -> Hooks.do
  page /\ pageId <- Hooks.useState 0

  let
    imagesPerPage = 20
    numUrls = maybe 0 (either (const 0) Array.length) urls'
    numPages = numUrls `div` imagesPerPage

    -- Technically the urls input can change in length after init,
    -- so it's good to ensure our page number is sensible
    effectivePage = clamp 0 numPages page

    urlIndex = effectivePage * imagesPerPage

    overview =
      [ HTML.text 
          ( show numUrls 
          <> " total images. Viewing [" 
          <> show (urlIndex + 1)
          <> ", "
          <> show (clamp 0 numUrls (urlIndex + 20))
          <> "]"
          )
      , HTML.button
          [ Events.onClick \_ -> do
              Hooks.raise outputToken ReturnToList
          ]
          [HTML.text "Go back"]
      , HTML.button
          [ Events.onClick \_ -> do
              Hooks.put pageId (effectivePage - 1)
              pure unit
          , Properties.disabled (effectivePage == 0)
          ]
          [HTML.text "Prev Page"]
      , HTML.button
          [ Events.onClick \_ -> do
              Hooks.put pageId (effectivePage + 1)
          , Properties.disabled (effectivePage >= numPages)
          ]
          [HTML.text "Next page"]
      ]

  pageUrls <- Hooks.captures { effectivePage, urlsRef: UnsafeRefEq urls' }
    (\mv -> Hooks.useMemo mv
        \_ -> map (Array.slice urlIndex (urlIndex + imagesPerPage - 1)) <$> urls'
    )

  Hooks.pure do
    HTML.div_
      $  overview
      <> [HTML.br_]
      <> case pageUrls of
          Nothing -> [HTML.text "Loading..."]
          Just (Left (RequestError err)) -> [HTML.text (Affjax.printError err)]
          Just (Left (JsonError err)) -> [HTML.text (printJsonDecodeError err)]
          Just (Right urls) ->
            (urls <#> \url -> HTML.img [Properties.src url])
      <> [HTML.br_]
      <> overview

--------------------------------------------------------------------------------
