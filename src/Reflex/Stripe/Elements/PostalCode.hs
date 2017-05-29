module Reflex.Stripe.Elements.PostalCode where

import Control.Concurrent.MVar (putMVar, takeMVar)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Functor (void)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Language.Javascript.JSaddle
  ( FromJSVal, fromJSVal, fromJSValUnchecked, maybeNullOrUndefined', unsafeGetProp, makeObject, JSVal, js1, js2, MonadJSM, liftJSM )
import Reflex.Dom
  ( (=:), DomBuilder, DomBuilderSpace, Element, Event, EventResult, elAttr', ffilter, ffor, HasMountStatus, getMountStatus, MountState(Mounted)
  , getMountStatus, Performable, PerformEvent, performEvent_, TriggerEvent, uniqDyn, updated
  )
import Reflex.Stripe.Elements.Object (StripeElements(StripeElements), _stripeElements_object, _stripeElements_nextElementId)
import Reflex.Stripe.Elements.Types (IsStripeElement, stripeElement, getStripeElementEvent, StripeElementConfig, StripeElementError)

import Reflex.Stripe.Utils (consoleLog)

-- |Structure containing the underlying reference to and Reflex signals for a Stripe postal code element.
data StripePostalCodeElement t m = StripePostalCodeElement
  { _stripePostalCodeElement_domElement :: Element EventResult (DomBuilderSpace m) t
  , _stripePostalCodeElement_stripeElement :: JSVal
  }

makeLenses ''StripePostalCodeElement

instance IsStripeElement (StripePostalCodeElement t m) where
  stripeElement = _stripePostalCodeElement_stripeElement

-- |A Stripe CVC field.
stripePostalCodeElement
  :: forall t m.
     ( DomBuilder t m
     , MonadIO m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m)
     , HasMountStatus t m
     )
  => StripeElements -> StripeElementConfig -> m (StripePostalCodeElement t m)
stripePostalCodeElement (StripeElements { _stripeElements_object, _stripeElements_nextElementId }) config = do
  elementId <- liftIO $ takeMVar _stripeElements_nextElementId
  liftIO $ putMVar _stripeElements_nextElementId (succ elementId)
  let elementIdStr = "reflex-stripe-element-" <> (pack . show) elementId
  (_stripePostalCodeElement_domElement, _) <- elAttr' "div" ("id" =: elementIdStr) (pure ())
  _stripePostalCodeElement_stripeElement <- liftJSM $ _stripeElements_object ^. js2 ("create" :: Text) ("postalCode" :: Text) config
  whenMounted <- ffilter (== Mounted) . updated . uniqDyn <$> getMountStatus
  performEvent_ . ffor whenMounted $ \ _ ->
    void . liftJSM $
      _stripePostalCodeElement_stripeElement ^. js1 ("mount" :: Text) ("#" <> elementIdStr)
  pure $ StripePostalCodeElement {..}

-- |Structure holding details of a change event for a postal code element.
data StripePostalCodeElementChange = StripePostalCodeElementChange
  { _stripePostalCodeElementChange_empty :: Bool
  -- ^Whether the element is now empty (has no value)
  , _stripePostalCodeElementChange_complete :: Bool
  -- ^Whether the element is now complete (has a valid value)
  , _stripePostalCodeElementChange_error :: Maybe StripeElementError
  -- ^The current validation error, if any.
  , _stripePostalCodeElementChange_value :: Text
  -- ^The current value in the postal code field.
  }
  deriving (Eq, Show)

instance FromJSVal StripePostalCodeElementChange where
  fromJSVal = maybeNullOrUndefined' $ \ jsv -> do
    consoleLog jsv
    o <- makeObject jsv
    _stripePostalCodeElementChange_empty    <- fromJSValUnchecked =<< unsafeGetProp "empty" o
    _stripePostalCodeElementChange_complete <- fromJSValUnchecked =<< unsafeGetProp "complete" o
    _stripePostalCodeElementChange_error    <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "error" o
    _stripePostalCodeElementChange_value    <- fromJSValUnchecked =<< unsafeGetProp "value" o
    pure StripePostalCodeElementChange {..}

-- |Get an Event which fires each time the state of a Stripe postal code element changes.
getStripePostalCodeElementOnChange :: (MonadJSM m, TriggerEvent t m) => StripePostalCodeElement t m -> m (Event t StripePostalCodeElementChange)
getStripePostalCodeElementOnChange = getStripeElementEvent "change" fromJSValUnchecked

