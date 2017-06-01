module Reflex.Stripe.Types where

import Control.Lens.TH (makeLenses, makePrisms)
import Data.JSString.Text (textFromJSString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Traversable (for)
import Language.Javascript.JSaddle (FromJSVal, fromJSVal, fromJSValUnchecked, JSM, JSVal, listProps, makeObject, unsafeGetProp, maybeNullOrUndefined')

-- |Structure representing a Stripe error - https://stripe.com/docs/api#errors
data StripeError = StripeError
  { _stripeError_type        :: Text
  , _stripeError_charge      :: Maybe Text
  , _stripeError_message     :: Maybe Text
  , _stripeError_code        :: Maybe Text
  , _stripeError_declineCode :: Maybe Text
  , _stripeError_param       :: Maybe Text
  }
  deriving (Eq, Show)

makeLenses ''StripeError

instance FromJSVal StripeError where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv -> do
      o <- makeObject jsv
      _stripeError_type        <- fromJSValUnchecked =<< unsafeGetProp "type" o
      _stripeError_charge      <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "charge" o
      _stripeError_message     <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "message" o
      _stripeError_code        <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "code" o
      _stripeError_declineCode <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "decline_code" o
      _stripeError_param       <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "param" o
      pure StripeError {..}

data StripeCheck
  = StripeCheckPass
  | StripeCheckFail
  | StripeCheckUnavailable
  | StripeCheckUnchecked
  deriving (Eq, Show)

makePrisms ''StripeCheck

instance FromJSVal StripeCheck where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv ->
      fmap (id :: Text -> Text) (fromJSValUnchecked jsv) >>= \ case
        "pass"        -> pure StripeCheckPass
        "fail"        -> pure StripeCheckFail
        "unavailable" -> pure StripeCheckUnavailable
        "unchecked"   -> pure StripeCheckUnchecked
        other         -> fail $ "Unexpected StripeCheck value " <> unpack other

data StripeCard = StripeCard
  { _stripeCard_id                     :: Text
  , _stripeCard_account                :: Maybe Text
  , _stripeCard_addressCity            :: Maybe Text
  , _stripeCard_addressCountry         :: Maybe Text
  , _stripeCard_addressLine1           :: Maybe Text
  , _stripeCard_addressLine1Check      :: Maybe StripeCheck
  , _stripeCard_addressLine2           :: Maybe Text
  , _stripeCard_addressState           :: Maybe Text
  , _stripeCard_addressZip             :: Maybe Text
  , _stripeCard_addressZipCheck        :: Maybe StripeCheck
  , _stripeCard_availablePayoutMethods :: [Text]
  , _stripeCard_brand                  :: Maybe Text
  , _stripeCard_country                :: Maybe Text
  , _stripeCard_currency               :: Maybe Text
  , _stripeCard_customer               :: Maybe Text
  , _stripeCard_cvcCheck               :: Maybe StripeCheck
  , _stripeCard_defaultForCurrency     :: Maybe Bool
  , _stripeCard_dynamicLast4           :: Maybe Text
  , _stripeCard_expMonth               :: Maybe Int
  , _stripeCard_expYear                :: Maybe Int
  , _stripeCard_fingerprint            :: Maybe Text
  , _stripeCard_funding                :: Maybe Text
  , _stripeCard_last4                  :: Maybe Text
  , _stripeCard_metadata               :: Map Text Text
  , _stripeCard_name                   :: Maybe Text
  , _stripeCard_recipient              :: Maybe Text
  , _stripeCard_tokenizationMethod     :: Maybe Text
  }
  deriving (Eq, Show)

makeLenses ''StripeCard

instance FromJSVal StripeCard where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv -> do
      o <- makeObject jsv
      _stripeCard_id                     <- fromJSValUnchecked =<< unsafeGetProp "id" o
      _stripeCard_account                <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "account" o
      _stripeCard_addressCity            <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "address_city" o
      _stripeCard_addressCountry         <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "address_country" o
      _stripeCard_addressLine1           <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "address_line1" o
      _stripeCard_addressLine1Check      <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "address_line1Check" o
      _stripeCard_addressLine2           <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "address_line2" o
      _stripeCard_addressState           <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "address_state" o
      _stripeCard_addressZip             <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "address_zip" o
      _stripeCard_addressZipCheck        <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "address_zip_check" o
      _stripeCard_availablePayoutMethods <- fmap (fromMaybe []) $ maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "available_payout_methods" o
      _stripeCard_brand                  <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "brand" o
      _stripeCard_country                <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "country" o
      _stripeCard_currency               <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "currency" o
      _stripeCard_customer               <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "customer" o
      _stripeCard_cvcCheck               <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "cvc_check" o
      _stripeCard_defaultForCurrency     <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "default_for_currency" o
      _stripeCard_dynamicLast4           <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "dynamic_last4" o
      _stripeCard_expMonth               <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "exp_month" o
      _stripeCard_expYear                <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "exp_year" o
      _stripeCard_fingerprint            <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "fingerprint" o
      _stripeCard_funding                <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "funding" o
      _stripeCard_last4                  <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "last4" o
      _stripeCard_metadata               <- fmap (fromMaybe Map.empty) $ maybeNullOrUndefined' mapFromJSVal =<< unsafeGetProp "metadata" o
      _stripeCard_name                   <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "name" o
      _stripeCard_recipient              <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "recipient" o
      _stripeCard_tokenizationMethod     <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "tokenization_method" o
      pure StripeCard {..}
    where
      mapFromJSVal :: JSVal -> JSM (Map Text Text)
      mapFromJSVal jsv = do
        o <- makeObject jsv
        ps <- listProps o
        vs <- for ps $ \ p -> unsafeGetProp p o >>= fromJSValUnchecked
        pure . Map.fromList $ map textFromJSString ps `zip` vs

data StripeTokenType
  = StripeTokenCard
  | StripeTokenBankAccount
  deriving (Eq, Show)

makePrisms ''StripeTokenType

instance FromJSVal StripeTokenType where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv ->
      fmap (id :: Text -> Text) (fromJSValUnchecked jsv) >>= \ case
        "card"         -> pure StripeTokenCard
        "bank_account" -> pure StripeTokenBankAccount
        other          -> fail $ "Unexpected StripeTokenType value " <> unpack other

data StripeToken = StripeToken
  { _stripeToken_id       :: Text
  , _stripeToken_card     :: Maybe StripeCard
  , _stripeToken_clientIp :: Maybe Text
  , _stripeToken_created  :: UTCTime
  , _stripeToken_liveMode :: Bool
  , _stripeToken_type     :: StripeTokenType
  , _stripeToken_used     :: Bool
  }
  deriving (Eq, Show)

makeLenses ''StripeToken

instance FromJSVal StripeToken where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv -> do
      o <- makeObject jsv
      _stripeToken_id       <- fromJSValUnchecked =<< unsafeGetProp "id" o
      _stripeToken_card     <- fromJSValUnchecked =<< unsafeGetProp "card" o
      _stripeToken_clientIp <- fromJSValUnchecked =<< unsafeGetProp "client_ip" o
      _stripeToken_created  <- utcTimeFromJSVal   =<< unsafeGetProp "created" o
      _stripeToken_liveMode <- fromJSValUnchecked =<< unsafeGetProp "livemode" o
      _stripeToken_type     <- fromJSValUnchecked =<< unsafeGetProp "type" o
      _stripeToken_used     <- fromJSValUnchecked =<< unsafeGetProp "used" o
      pure StripeToken {..}
    where
      utcTimeFromJSVal = fmap (posixSecondsToUTCTime . fromInteger . (truncate :: Double -> Integer)) . fromJSValUnchecked
