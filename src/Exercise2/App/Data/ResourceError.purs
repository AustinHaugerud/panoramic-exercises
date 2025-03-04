module Exercise2.App.Data.ResourceError where

import Affjax.Web as Affjax
import Data.Argonaut.Decode (JsonDecodeError)

--------------------------------------------------------------------------------

data ResourceError
  = RequestError Affjax.Error
  | JsonError JsonDecodeError

--------------------------------------------------------------------------------
