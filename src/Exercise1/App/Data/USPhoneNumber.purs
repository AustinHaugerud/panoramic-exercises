module Exercise1.App.Data.USPhoneNumber where

import Exercise1.App.Data.BoundedInt (BoundedInt)

--------------------------------------------------------------------------------

-- There are some special rules surrounding
-- numbers like 911 that we're not accounting
-- for here, since accounting for these "gaps"
-- at the type level is more complex. It is
-- likely possible to use rows
-- for a type level ordered list of disjoint
-- intervals, but we'll consider that out of scope.
--
-- Concerning the area and office codes, we're just
-- modelling how numbers starting with 0 or 1 are illegal.
type USPhoneNumber =
  { areaCode :: BoundedInt 200 999
  , centralOfficeCode :: BoundedInt 200 999
  , subscriberNumber :: BoundedInt 0 9999
  }

--------------------------------------------------------------------------------
