module Reflex.Stripe.Elements.CardExpiry where

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

-- |Structure containing the underlying reference to and Reflex signals for a Stripe card expiry element.
data StripeCardExpiryElement t m = StripeCardExpiryElement
  { _stripeCardExpiryElement_domElement :: Element EventResult (DomBuilderSpace m) t
  , _stripeCardExpiryElement_stripeElement :: JSVal
  }

makeLenses ''StripeCardExpiryElement

instance IsStripeElement (StripeCardExpiryElement t m) where
  stripeElement = _stripeCardExpiryElement_stripeElement

-- |A Stripe expiry field.
stripeCardExpiryElement
  :: forall t m.
     ( DomBuilder t m
     , MonadIO m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m)
     , HasMountStatus t m
     )
  => StripeElements -> StripeElementConfig -> m (StripeCardExpiryElement t m)
stripeCardExpiryElement (StripeElements { _stripeElements_object, _stripeElements_nextElementId }) config = do
  elementId <- liftIO $ takeMVar _stripeElements_nextElementId
  liftIO $ putMVar _stripeElements_nextElementId (succ elementId)
  let elementIdStr = "reflex-stripe-element-" <> (pack . show) elementId
  (_stripeCardExpiryElement_domElement, _) <- elAttr' "div" ("id" =: elementIdStr) (pure ())
  _stripeCardExpiryElement_stripeElement <- liftJSM $ _stripeElements_object ^. js2 ("create" :: Text) ("cardExpiry" :: Text) config
  whenMounted <- ffilter (== Mounted) . updated . uniqDyn <$> getMountStatus
  performEvent_ . ffor whenMounted $ \ _ ->
    void . liftJSM $
      _stripeCardExpiryElement_stripeElement ^. js1 ("mount" :: Text) ("#" <> elementIdStr)
  pure $ StripeCardExpiryElement {..}
