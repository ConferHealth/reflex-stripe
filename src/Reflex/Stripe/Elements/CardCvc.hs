module Reflex.Stripe.Elements.CardCvc where

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

-- |Structure containing the underlying reference to and Reflex signals for a Stripe card CVC element.
data StripeCardCvcElement t m = StripeCardCvcElement
  { _stripeCardCvcElement_domElement :: Element EventResult (DomBuilderSpace m) t
  , _stripeCardCvcElement_stripeElement :: JSVal
  }

makeLenses ''StripeCardCvcElement

instance IsStripeElement (StripeCardCvcElement t m) where
  stripeElement = _stripeCardCvcElement_stripeElement

-- |A Stripe CVC field.
stripeCardCvcElement
  :: forall t m.
     ( DomBuilder t m
     , MonadIO m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m)
     , HasMountStatus t m
     )
  => StripeElements -> StripeElementConfig -> m (StripeCardCvcElement t m)
stripeCardCvcElement (StripeElements { _stripeElements_object, _stripeElements_nextElementId }) config = do
  elementId <- liftIO $ takeMVar _stripeElements_nextElementId
  liftIO $ putMVar _stripeElements_nextElementId (succ elementId)
  let elementIdStr = "reflex-stripe-element-" <> (pack . show) elementId
  (_stripeCardCvcElement_domElement, _) <- elAttr' "div" ("id" =: elementIdStr) (pure ())
  _stripeCardCvcElement_stripeElement <- liftJSM $ _stripeElements_object ^. js2 ("create" :: Text) ("cardCvc" :: Text) config
  whenMounted <- ffilter (== Mounted) . updated . uniqDyn <$> getMountStatus
  performEvent_ . ffor whenMounted $ \ _ ->
    void . liftJSM $
      _stripeCardCvcElement_stripeElement ^. js1 ("mount" :: Text) ("#" <> elementIdStr)
  pure $ StripeCardCvcElement {..}
