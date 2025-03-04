module Exercise2.App.Data.DogBreedListDTO where

import Prelude

import Data.Array as Array
import Data.Lens (_2, over)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object, toUnfoldable)

--------------------------------------------------------------------------------

type DogBreedListDTO =
  { message :: Object (Array String)
  }

type DogBreedDisplayArray = Array (String /\ Array String)

toDisplayArray :: DogBreedListDTO -> DogBreedDisplayArray
toDisplayArray
  =   map (over _2 Array.sort) -- sort sub breeds
  <<< Array.sortBy (comparing fst) -- sort by major breed first
  <<< toUnfoldable
  <<< _.message

--------------------------------------------------------------------------------
