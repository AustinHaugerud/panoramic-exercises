module Exercise2.App where

import Prelude

import Affjax.Web as Affjax
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Exercise2.App.Component.DogBreedDetails (Message(..))
import Exercise2.App.Component.DogBreedDetails as DogBreedDetails
import Exercise2.App.Component.DogBreedList (Message(..))
import Exercise2.App.Component.DogBreedList as DogBreedList
import Exercise2.App.Data.ResourceError (ResourceError(..))
import Exercise2.App.Hook.UseDogBreedImageUrlCache (useDogBreedImageUrlCache)
import Exercise2.App.Hook.UseDogBreedList (useDogBreedList)
import Halogen as H
import Halogen.HTML as HTML
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

data AppState = ViewBreeds | ViewBreedDetails String (Maybe String)

_dogBreedList = Proxy :: Proxy "dogBreedList"

_dogBreedDetails = Proxy :: Proxy "dogBreedDetails"

app :: forall q i o m. MonadAff m => H.Component q i o m
app = Hooks.component \_ _ -> Hooks.do
  dogBreedList' <- useDogBreedList
  state /\ stateId <- Hooks.useState ViewBreeds
  imagesUrlCache /\ suggestToCache <- useDogBreedImageUrlCache

  let
    handleListSelect (ViewBreed breed subBreed') = do
      suggestToCache (breed /\ subBreed')
      Hooks.put stateId (ViewBreedDetails breed subBreed')

    handleReturn ReturnToList = do
      Hooks.put stateId ViewBreeds

  Hooks.pure do
    case dogBreedList' of
      Nothing -> HTML.text "Loading..."
      Just (Left (RequestError err)) -> HTML.text (Affjax.printError err)
      Just (Left (JsonError err)) -> HTML.text (printJsonDecodeError err)
      Just (Right dogBreeds) ->
        case state of
          ViewBreeds ->
            HTML.slot _dogBreedList unit DogBreedList.component dogBreeds handleListSelect
          ViewBreedDetails breed subBreed' ->
            let
              k = breed /\ subBreed'
              resource' = join (Map.lookup k imagesUrlCache.imageUrls)
            in
              HTML.slot _dogBreedDetails unit DogBreedDetails.component resource' handleReturn

--------------------------------------------------------------------------------
