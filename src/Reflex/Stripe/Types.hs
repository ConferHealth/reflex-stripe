module Reflex.Stripe.Types where

import Control.Lens ((^.))
import Data.Functor (void)
import Data.JSString.Text (textFromJSString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Traversable (for)
import Language.Javascript.JSaddle (FromJSVal, fromJSVal, fromJSValUnchecked, JSM, JSVal, listProps, makeObject, unsafeGetProp, maybeNullOrUndefined', jsg, js1)

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

instance FromJSVal StripeError where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv -> do
      o <- makeObject jsv
      _stripeError_type        <- fromJSValUnchecked =<< unsafeGetProp "type" o
      _stripeError_charge      <- fromJSVal          =<< unsafeGetProp "charge" o
      _stripeError_message     <- fromJSVal          =<< unsafeGetProp "message" o
      _stripeError_code        <- fromJSVal          =<< unsafeGetProp "code" o
      _stripeError_declineCode <- fromJSVal          =<< unsafeGetProp "decline_code" o
      _stripeError_param       <- fromJSVal          =<< unsafeGetProp "param" o
      pure StripeError {..}

data StripeCheck
  = StripeCheckPass
  | StripeCheckFail
  | StripeCheckUnavailable
  | StripeCheckUnchecked
  deriving (Eq, Show)

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

instance FromJSVal StripeCard where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv -> do
      o <- makeObject jsv
      _stripeCard_id                     <- fromJSValUnchecked =<< unsafeGetProp "id" o
      _stripeCard_account                <- fromJSVal          =<< unsafeGetProp "account" o
      _stripeCard_addressCity            <- fromJSVal          =<< unsafeGetProp "address_city" o
      _stripeCard_addressCountry         <- fromJSVal          =<< unsafeGetProp "address_country" o
      _stripeCard_addressLine1           <- fromJSVal          =<< unsafeGetProp "address_line1" o
      _stripeCard_addressLine1Check      <- fromJSVal          =<< unsafeGetProp "address_line1Check" o
      _stripeCard_addressLine2           <- fromJSVal          =<< unsafeGetProp "address_line2" o
      _stripeCard_addressState           <- fromJSVal          =<< unsafeGetProp "address_state" o
      _stripeCard_addressZip             <- fromJSVal          =<< unsafeGetProp "address_zip" o
      _stripeCard_addressZipCheck        <- fromJSVal          =<< unsafeGetProp "address_zip_check" o
      _stripeCard_availablePayoutMethods <- fromJSValUnchecked =<< unsafeGetProp "available_payout_methods" o
      _stripeCard_brand                  <- fromJSVal          =<< unsafeGetProp "brand" o
      _stripeCard_country                <- fromJSVal          =<< unsafeGetProp "country" o
      _stripeCard_currency               <- fromJSVal          =<< unsafeGetProp "currency" o
      _stripeCard_customer               <- fromJSVal          =<< unsafeGetProp "customer" o
      _stripeCard_cvcCheck               <- fromJSVal          =<< unsafeGetProp "cvc_check" o
      _stripeCard_defaultForCurrency     <- fromJSVal          =<< unsafeGetProp "default_for_currency" o
      _stripeCard_dynamicLast4           <- fromJSVal          =<< unsafeGetProp "dynamic_last4" o
      _stripeCard_expMonth               <- fromJSVal          =<< unsafeGetProp "exp_month" o
      _stripeCard_expYear                <- fromJSVal          =<< unsafeGetProp "exp_year" o
      _stripeCard_fingerprint            <- fromJSVal          =<< unsafeGetProp "fingerprint" o
      _stripeCard_funding                <- fromJSVal          =<< unsafeGetProp "funding" o
      _stripeCard_last4                  <- fromJSVal          =<< unsafeGetProp "last4" o
      _stripeCard_metadata               <- mapFromJSVal       =<< unsafeGetProp "metadata" o
      _stripeCard_name                   <- fromJSVal          =<< unsafeGetProp "name" o
      _stripeCard_recipient              <- fromJSVal          =<< unsafeGetProp "recipient" o
      _stripeCard_tokenizationMethod     <- fromJSVal          =<< unsafeGetProp "tokenization_method" o
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

instance FromJSVal StripeToken where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv -> do
      void $ jsg ("console" :: Text) ^. js1 ("log" :: Text) jsv
      o <- makeObject jsv
      _stripeToken_id       <- fromJSValUnchecked =<< unsafeGetProp "id" o
      _stripeToken_card     <- fromJSValUnchecked =<< unsafeGetProp "card" o
      _stripeToken_clientIp <- fromJSValUnchecked =<< unsafeGetProp "client_ip" o
      _stripeToken_created  <- utcTimeFromJSVal   =<< unsafeGetProp "created" o
      _stripeToken_liveMode <- fromJSValUnchecked =<< unsafeGetProp "live_mode" o
      _stripeToken_type     <- fromJSValUnchecked =<< unsafeGetProp "type" o
      _stripeToken_used     <- fromJSValUnchecked =<< unsafeGetProp "used" o
      pure StripeToken {..}
    where
      utcTimeFromJSVal = fmap (posixSecondsToUTCTime . fromInteger . (truncate :: Double -> Integer)) . fromJSValUnchecked
