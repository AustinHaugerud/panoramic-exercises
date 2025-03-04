module Exercise1.App.Data.MaritalStatus where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

--------------------------------------------------------------------------------

-- This type helps us avoid boolean blindness issues
data MaritalStatus = Single | Married

derive instance Eq MaritalStatus
derive instance Ord MaritalStatus

derive instance Generic MaritalStatus _

instance Show MaritalStatus where
  show = genericShow

--------------------------------------------------------------------------------
