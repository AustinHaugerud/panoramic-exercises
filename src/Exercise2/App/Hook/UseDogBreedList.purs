module Exercise2.App.Hook.UseDogBreedList where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web as Affjax
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Exercise2.App.Data.DogBreedListDTO (DogBreedDisplayArray, DogBreedListDTO, toDisplayArray)
import Exercise2.App.Data.ResourceError (ResourceError(..))
import Halogen.Hooks (class HookNewtype, type (<>), Hook, UseEffect, UseState)
import Halogen.Hooks as Hooks

--------------------------------------------------------------------------------

foreign import data UseDogBreedList :: Hooks.HookType

type UseDogBreedList'
  =  UseState (Maybe (Either ResourceError DogBreedDisplayArray))
  <> UseEffect
  <> Hooks.Pure

instance HookNewtype UseDogBreedList UseDogBreedList'

useDogBreedList :: forall m. MonadAff m => Hook m UseDogBreedList (Maybe (Either ResourceError DogBreedDisplayArray))
useDogBreedList = Hooks.wrap hook
  where
  hook :: Hook m UseDogBreedList' _
  hook = Hooks.do
    resource /\ resourceId <- Hooks.useState Nothing

    Hooks.useLifecycleEffect do
      result <- liftAff (lmap RequestError <$> Affjax.get json "/breeds/list/all")
      let 
        decoded = result >>= (lmap JsonError <<< decodeJson @DogBreedListDTO <<< _.body)
        displayReady = toDisplayArray <$> decoded
      Hooks.put resourceId (Just displayReady)
      pure Nothing

    Hooks.pure resource

--------------------------------------------------------------------------------
