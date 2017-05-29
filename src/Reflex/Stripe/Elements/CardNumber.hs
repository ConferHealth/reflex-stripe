module Reflex.Stripe.Elements.CardNumber where

import Control.Concurrent.MVar (putMVar, takeMVar)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Functor (void)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Language.Javascript.JSaddle (JSVal, js1, js2, MonadJSM, liftJSM)
import Reflex.Dom
  ( (=:), DomBuilder, DomBuilderSpace, Element, EventResult, elAttr', ffilter, ffor, HasMountStatus, getMountStatus, MountState(Mounted)
  , getMountStatus, Performable, PerformEvent, performEvent_, uniqDyn, updated
  )
import Reflex.Stripe.Elements.Object (StripeElements(StripeElements), _stripeElements_object, _stripeElements_nextElementId)
import Reflex.Stripe.Elements.Types (IsStripeElement, stripeElement, StripeElementConfig)

-- |Structure containing the underlying reference to and Reflex signals for a Stripe card number element.
data StripeCardNumberElement t m = StripeCardNumberElement
  { _stripeCardNumberElement_domElement :: Element EventResult (DomBuilderSpace m) t
  , _stripeCardNumberElement_stripeElement :: JSVal
  }

makeLenses ''StripeCardNumberElement

instance IsStripeElement (StripeCardNumberElement t m) where
  stripeElement = _stripeCardNumberElement_stripeElement

-- |A Stripe number field.
stripeCardNumberElement
  :: forall t m.
     ( DomBuilder t m
     , MonadIO m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m)
     , HasMountStatus t m
     )
  => StripeElements -> StripeElementConfig -> m (StripeCardNumberElement t m)
stripeCardNumberElement (StripeElements { _stripeElements_object, _stripeElements_nextElementId }) config = do
  elementId <- liftIO $ takeMVar _stripeElements_nextElementId
  liftIO $ putMVar _stripeElements_nextElementId (succ elementId)
  let elementIdStr = "reflex-stripe-element-" <> (pack . show) elementId
  (_stripeCardNumberElement_domElement, _) <- elAttr' "div" ("id" =: elementIdStr) (pure ())
  _stripeCardNumberElement_stripeElement <- liftJSM $ _stripeElements_object ^. js2 ("create" :: Text) ("cardNumber" :: Text) config
  whenMounted <- ffilter (== Mounted) . updated . uniqDyn <$> getMountStatus
  performEvent_ . ffor whenMounted $ \ _ ->
    void . liftJSM $
      _stripeCardNumberElement_stripeElement ^. js1 ("mount" :: Text) ("#" <> elementIdStr)
  pure $ StripeCardNumberElement {..}
