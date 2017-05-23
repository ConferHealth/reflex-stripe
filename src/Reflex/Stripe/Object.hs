module Reflex.Stripe.Object where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Language.Javascript.JSaddle (JSM, JSVal, jsg1, js0, val)

data Stripe = Stripe
  { _stripe_object :: JSVal
  , _stripe_elements :: JSVal
  , _stripe_nextElementId :: MVar Integer
  }

initStripe :: Text -> JSM Stripe
initStripe apiToken = do
  _stripe_object <- jsg1 ("Stripe" :: Text) apiToken
  _stripe_elements <- val _stripe_object ^. js0 ("elements" :: Text)
  _stripe_nextElementId <- liftIO $ newMVar 0
  pure $ Stripe {..}
