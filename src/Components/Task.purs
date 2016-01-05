module Components.Task where

import Prelude

import qualified Thermite as T

import qualified React.DOM as R
import qualified React.DOM.Props as RP

import Unsafe.Coerce

-- | Actions for the task component
data TaskAction
  = ChangeCompleted Boolean
  | RemoveTask

-- | The state for the task component
type Task =
  { completed :: Boolean
  , description :: String
  , quantity :: Int
  }

initialTask :: String -> Task
initialTask s = { completed: false, description: s, quantity: 1 }

-- | A `Spec` for the task component.
taskSpec :: forall eff props. T.Spec eff Task props TaskAction
taskSpec = T.simpleSpec performAction render
  where
  -- Renders the current state of the component as a collection of React elements.
  render :: T.Render Task props TaskAction
  render dispatch _ s _ =
    [ R.tr' <<< map (R.td' <<< pure) $
        [ R.input [ RP._type "checkbox"
                  , RP.className "checkbox"
                  , RP.checked (if s.completed then "checked" else "")
                  , RP.title "Markera som klar"
                  , RP.onChange \e -> dispatch (ChangeCompleted (unsafeCoerce e).target.checked)
                  ] []
        , R.text s.description
        , R.text $ show s.quantity
        , R.a [ RP.className "btn btn-danger pull-right"
              , RP.title "Ta bort"
              , RP.onClick \_ -> dispatch RemoveTask
              ]
              [ R.text "✖" ]
        ]
    ]

  -- Updates the state in response to an action.
  --
  -- _Note_: this component can only see actions of type `TaskAction`, but the `RemoveTask` action
  -- is ignored here: it will be handled by the parent component.
  performAction :: T.PerformAction eff Task props TaskAction
  performAction (ChangeCompleted b)   _ state k = k $ state { completed = b }
  performAction _                     _ _ _ = pure unit
