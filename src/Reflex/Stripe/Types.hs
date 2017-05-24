module Reflex.Stripe.Types where

import Control.Lens ((^.))
import Data.Text (Text)
import Language.Javascript.JSaddle (FromJSVal, fromJSVal, fromJSValUnchecked, js)

-- |Structure representing a Stripe error - https://stripe.com/docs/api#errors
data StripeError = StripeError
  { _stripeError_type :: Text
  , _stripeError_charge :: Maybe Text
  , _stripeError_message :: Maybe Text
  , _stripeError_code :: Maybe Text
  , _stripeError_declineCode :: Maybe Text
  , _stripeError_param :: Maybe Text
  }

instance FromJSVal StripeError where
  fromJSVal jsv = do
    _stripeError_type        <- fromJSValUnchecked =<< jsv ^. js ("type" :: Text)
    _stripeError_charge      <- fromJSVal          =<< jsv ^. js ("charge" :: Text)
    _stripeError_message     <- fromJSVal          =<< jsv ^. js ("message" :: Text)
    _stripeError_code        <- fromJSVal          =<< jsv ^. js ("code" :: Text)
    _stripeError_declineCode <- fromJSVal          =<< jsv ^. js ("decline_code" :: Text)
    _stripeError_param       <- fromJSVal          =<< jsv ^. js ("param" :: Text)
    pure . Just $ StripeError {..}

data StripeToken = StripeToken -- FIXME

instance FromJSVal StripeToken where
  fromJSVal _ = do
    fail "FIXME unimplemented StripeToken fromjsval"
