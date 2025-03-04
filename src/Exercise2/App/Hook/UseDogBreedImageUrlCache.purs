module Exercise2.App.Hook.UseDogBreedImageUrlCache where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web as Affjax
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Lens (set)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Exercise2.App.Data.DogBreedImagesDTO (URL, DogBreedImagesDTO)
import Exercise2.App.Data.ResourceError (ResourceError(..))
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

foreign import data UseDogBreedImageUrlCache :: Hooks.HookType

type LoadState = Maybe (Either ResourceError (Array URL))

-- Breed and possibly sub-breed
type Key = String /\ Maybe String

type CacheState =
  { imageUrls :: Map Key LoadState
  }

type UseDogBreedImageUrlCache' = UseState CacheState <> UseEffect <> Hooks.Pure

instance HookNewtype UseDogBreedImageUrlCache UseDogBreedImageUrlCache'

-- | Returns a cache for image urls, and a procedure
-- | that can indicate to the cache a resource is needed.
-- | If the resource is already being loaded or is loaded sucessfully,
-- | it will ignore the indication.
useDogBreedImageUrlCache :: forall m. MonadAff m => Hook m UseDogBreedImageUrlCache (CacheState /\ (Key -> HookM m Unit))
useDogBreedImageUrlCache = Hooks.wrap hook
  where
  _imageUrls = prop (Proxy @"imageUrls")

  hook :: Hook m UseDogBreedImageUrlCache' _
  hook = Hooks.do
    cache /\ cacheId <- Hooks.useState { imageUrls: Map.empty }

    let
      indicateNeeded k@(breed /\ subBreed') = do
        let
          continue = case Map.lookup k cache.imageUrls of
            -- Okay to retry after a failure
            Just (Just (Left _)) -> true
            -- Never loaded
            Nothing -> true
            _ -> false

        when continue do
          Hooks.modify_ cacheId (set (_imageUrls <<< at k) (Just Nothing)) -- Indicate now loading

          let url = "/breed/" <> breed <> foldMap ("/" <> _) subBreed' <> "/images"
          result <- liftAff (lmap RequestError <$> Affjax.get json url)
          let
            decoded = result >>= (lmap JsonError <<< decodeJson @DogBreedImagesDTO <<< _.body)
            urls' = _.message <$> decoded
          Hooks.modify_ cacheId (set (_imageUrls <<< at k) (Just (Just urls')))

    Hooks.pure (cache /\ indicateNeeded)

--------------------------------------------------------------------------------
