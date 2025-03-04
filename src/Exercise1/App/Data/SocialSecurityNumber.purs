module Exercise1.App.Data.SocialSecurityNumber where

import Exercise1.App.Data.BoundedInt (BoundedInt)

--------------------------------------------------------------------------------

-- We're modelling the number of digits
-- in each component, and how each must be
-- at least 1. Similar to the phone number type
-- there are edge cases rules like how SSNs can't
-- start with 666 that we're not accounting for here,
-- because of the notably higher effort, but it's probably
-- possible.
type SocialSecurityNumber =
  { areaNumber :: BoundedInt 1 899
  , groupNumber :: BoundedInt 1 99
  , serialNumber :: BoundedInt 1 9999
  }

--------------------------------------------------------------------------------
