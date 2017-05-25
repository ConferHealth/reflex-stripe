module Reflex.Stripe.Types where

import Control.Lens ((^.))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Language.Javascript.JSaddle (FromJSVal, fromJSVal, fromJSValUnchecked, js, JSVal)

-- |Structure representing a Stripe error - https://stripe.com/docs/api#errors
data StripeError = StripeError
  { _stripeError_type        :: Text
  , _stripeError_charge      :: Maybe Text
  , _stripeError_message     :: Maybe Text
  , _stripeError_code        :: Maybe Text
  , _stripeError_declineCode :: Maybe Text
  , _stripeError_param       :: Maybe Text
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

data StripeTokenType
  = StripeTokenCard
  | StripeTokenBankAccount
  deriving (Eq, Show)

instance FromJSVal StripeTokenType where
  fromJSVal jsv =
    fmap (id :: Text -> Text) (fromJSValUnchecked jsv) >>= \ case
      "card"         -> pure $ Just StripeTokenCard
      "bank_account" -> pure $ Just StripeTokenBankAccount
      _              -> pure Nothing

data StripeToken = StripeToken
  { _stripeToken_id       :: Text
  , _stripeToken_card     :: Maybe JSVal
  , _stripeToken_clientIp :: Maybe Text
  , _stripeToken_created  :: UTCTime
  , _stripeToken_liveMode :: Bool
  , _stripeToken_type     :: StripeTokenType
  , _stripeToken_used     :: Bool
  }

instance FromJSVal StripeToken where
  fromJSVal jsv = do
    _stripeToken_id       <- fromJSValUnchecked =<< jsv ^. js ("id" :: Text)
    _stripeToken_card     <- fromJSValUnchecked =<< jsv ^. js ("card" :: Text)
    _stripeToken_clientIp <- fromJSValUnchecked =<< jsv ^. js ("client_ip" :: Text)
    _stripeToken_created  <- utcTimeFromJSVal   =<< jsv ^. js ("created" :: Text)
    _stripeToken_liveMode <- fromJSValUnchecked =<< jsv ^. js ("live_mode" :: Text)
    _stripeToken_type     <- fromJSValUnchecked =<< jsv ^. js ("type" :: Text)
    _stripeToken_used     <- fromJSValUnchecked =<< jsv ^. js ("used" :: Text)
    pure . Just $ StripeToken {..}
    where
      utcTimeFromJSVal = fmap (posixSecondsToUTCTime . fromInteger . (truncate :: Double -> Integer)) . fromJSValUnchecked
