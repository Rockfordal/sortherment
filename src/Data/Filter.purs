module Data.Filter where

import Prelude

-- | The three filters which can be applied to the list of tasks.
data Filter = All | Active | Completed

instance eqFilter :: Eq Filter where
  eq All       All       = true
  eq Active    Active    = true
  eq Completed Completed = true
  eq _         _         = false

showFilter :: Filter -> String
showFilter All = "Alla"
showFilter Active = "Aktiva"
showFilter Completed = "Utförda"
