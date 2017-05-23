module Reflex.Stripe.Elements.Card where

import Control.Concurrent.MVar (putMVar, takeMVar)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Default (Default, def)
import Data.Functor (void)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified JavaScript.Object.Internal as Obj
import Language.Javascript.JSaddle (JSVal, js1, js2, MonadJSM, liftJSM, ToJSVal, toJSVal, val)
import Reflex.Dom
  ( (=:), DomBuilder, DomBuilderSpace, Element, EventResult, elAttr', ffilter, ffor, MonadMountStatus, getMountStatus, MountState(Mounted), getMountStatus
  , Performable, PerformEvent, performEvent_, uniqDyn, updated
  )
import Reflex.Stripe.Elements.Types (IsStripeElement, stripeElement, StripeElementConfig, stripeElementConfigToObject)
import Reflex.Stripe.Object (Stripe(Stripe), _stripe_elements, _stripe_nextElementId)

-- |Configuration for a Stripe combined-style card element (type @card@)
data StripeCardElementConfig = StripeCardElementConfig
  { _stripeCardElementConfig_stripeElementConfig :: StripeElementConfig
  -- ^Configuration information shared between types of Stripe element
  , _stripeCardElementConfig_hidePostalCode :: Bool
  -- ^Whether the postal code should ne shown in the combined element
  , _stripeCardElementConfig_postalCodeValue :: Maybe Text
  -- ^If the postal code element is shown, an initial value to prefill it with.
  }

makeLenses ''StripeCardElementConfig

instance ToJSVal StripeCardElementConfig where
  toJSVal (StripeCardElementConfig {..}) = do
    o@(Obj.Object oJsv) <- stripeElementConfigToObject _stripeCardElementConfig_stripeElementConfig

    toJSVal _stripeCardElementConfig_hidePostalCode >>= \ jsv -> Obj.setProp "hidePostalCode" jsv o
    toJSVal _stripeCardElementConfig_postalCodeValue >>= \ jsv -> do
      value@(Obj.Object valueJsv) <- Obj.create
      Obj.setProp "postalCode" jsv value
      Obj.setProp "value" valueJsv o

    pure oJsv

instance Default StripeCardElementConfig where
  def = StripeCardElementConfig def False Nothing

-- |Structure containing the underlying reference to and Reflex signals for a Stripe combined card element.
data StripeCardElement t m = StripeCardElement
  { _stripeCardElement_domElement :: Element EventResult (DomBuilderSpace m) t
  , _stripeCardElement_stripeElement :: JSVal
  }

makeLenses ''StripeCardElement

instance IsStripeElement (StripeCardElement t m) where
  stripeElement = _stripeCardElement_stripeElement

-- |A Stripe combined card, expiry, CVV, and optionally postal code field.
stripeCardElement
  :: forall t m.
     ( DomBuilder t m
     , MonadIO m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m)
     , MonadMountStatus t m
     )
  => Stripe -> StripeCardElementConfig -> m (StripeCardElement t m)
stripeCardElement (Stripe { _stripe_elements, _stripe_nextElementId }) config = do
  elementId <- liftIO $ takeMVar _stripe_nextElementId
  liftIO $ putMVar _stripe_nextElementId (succ elementId)
  let elementIdStr = "reflex-stripe-element-" <> (pack . show) elementId
  (_stripeCardElement_domElement, _) <- elAttr' "div" ("id" =: elementIdStr) (pure ())
  _stripeCardElement_stripeElement <- liftJSM $ val _stripe_elements ^. js2 ("create" :: Text) ("card" :: Text) config
  whenMounted <- ffilter (== Mounted) . updated . uniqDyn <$> getMountStatus
  performEvent_ . ffor whenMounted $ \ _ ->
    void . liftJSM $
      val _stripeCardElement_stripeElement ^. js1 ("mount" :: Text) ("#" <> elementIdStr)
  pure $ StripeCardElement {..}


