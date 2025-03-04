module Exercise1.App.Data.Person
  ( Form(..)
  , PersonF
  , PersonForm
  , PersonProps
  , PersonValidated
  , Validated(..)
  , overPropState
  , validatePersonForm
  , viewPropState
  )
  where

import Prelude

import Exercise1.App.Data.MaritalStatus (MaritalStatus)
import Exercise1.App.Data.SocialSecurityNumber (SocialSecurityNumber)
import Exercise1.App.Data.USPhoneNumber (USPhoneNumber)
import Control.Comonad.Store (Store, peeks, pos, seeks)
import Data.Either (Either)
import Data.Lens (Getter', Setter', over, view)
import Data.Newtype (class Newtype, unwrap)
import Data.String.NonEmpty (NonEmptyString)


--------------------------------------------------------------------------------

-- For each property we consider a structure for a raw
-- input type,and a final validated type. Any other possible considerations
-- like a validation error type can be detailed by `f` itself.
-- This row is handy not only for functions that might have a `Row.Cons`
-- constraint, but potentially to be interpreted by something
-- like `Variant` (coproduct) instead. If we needed to consider
-- one of the props in mutually exclusive fashion some code dependent
-- on absractions like heterogenous folding can still potentially work.
type PersonProps :: (Type -> Type -> Type) -> Row Type
type PersonProps f = 
  ( firstName :: f String NonEmptyString
  , lastName :: f String NonEmptyString
  , socialSecurityNumber :: f String SocialSecurityNumber
  , maritalStatus :: f MaritalStatus MaritalStatus
  , usPhoneNumber :: f String USPhoneNumber
  )

type PersonF f = Record (PersonProps f)

--------------------------------------------------------------------------------

-- `Store` here lets us choose when we want to
-- actually run validation, versus something like say `Tuple s (StateT s (Either e) v)` which
-- requires us to evaluate immediately always. It also grants
-- us some flexibility on different validation methods independent
-- of managing the input state.
newtype Form e s v = Form (Store s (Either e v))

derive instance Newtype (Form e s v) _

derive instance Functor (Form e s)

--------------------------------------------------------------------------------

-- Essentially like `Const`, we could use that if we flipped
-- the argument order for the passed structure `f` for the
-- person data, but I figure this is clearer in intent anyhow.
newtype Validated :: forall k. k -> Type -> Type
newtype Validated s v = Validated v

derive instance Newtype (Validated s v) _

--------------------------------------------------------------------------------

-- Type for editing
type PersonForm e = PersonF (Form e)

-- Working with the form state is still pretty easy
overPropState :: forall e s v. Setter' (PersonForm e) (Form e s v) -> (s -> s) -> PersonForm e -> PersonForm e
overPropState setter f = over setter setFormState 
  where
  setFormState (Form store) = Form (seeks f store)

viewPropState :: forall e s v. Getter' (PersonForm e) (Form e s v) -> PersonForm e -> s
viewPropState getter = pos <<< unwrap <<< view getter

-- Type for fully formed
type PersonValidated = PersonF Validated

--------------------------------------------------------------------------------

-- For these kinds of record manipulations using functions like `hmap`
-- from the heterogeneous library as well as something like a `sequenceRecord`
-- function can be helpful to make things quick and easy. Since we're only doing
-- this once however we'll pass on implementing `sequenceRecord`.
validatePersonForm :: forall e. PersonForm e -> Either e PersonValidated
validatePersonForm form = ado
  firstName <- validate form.firstName
  lastName <- validate form.lastName
  socialSecurityNumber <- validate form.socialSecurityNumber
  maritalStatus <- validate form.maritalStatus
  usPhoneNumber <- validate form.usPhoneNumber
  in
    { firstName
    , lastName
    , socialSecurityNumber
    , maritalStatus
    , usPhoneNumber
    }
  where
  validate :: forall s v. Form e s v -> Either e (Validated s v)
  validate (Form store) = Validated <$> peeks identity store

--------------------------------------------------------------------------------
