module Reflex.Stripe.Object where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (SomeException, handle)
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Data.Default (Default, def)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Language.Javascript.JSaddle
  ( FromJSVal, fromJSVal, fromJSValUnchecked, ToJSVal, toJSVal
  , function, freeFunction
  , JSVal, MonadJSM, liftJSM
  , jsg, jsg1, js1, js2, jsNull, create, makeObject, getProp, unsafeGetProp, setProp, maybeNullOrUndefined'
  )
import Reflex.Dom (Event, ffor, Performable, PerformEvent, performEvent_, TriggerEvent, newTriggerEvent)
import Reflex.Stripe.Elements.Types (IsStripeElement, stripeElement)
import Reflex.Stripe.Types (StripeError, StripeToken)

-- |Shared state after initializing the Stripe.js library.
data Stripe = Stripe
  { _stripe_object :: JSVal
  -- ^The Stripe object - https://stripe.com/docs/stripe.js#the-stripe-object
  , _stripe_nextElementId :: MVar Integer
  -- ^The next unique identifier to use when creating Stripe Elements, since to mount them we need a document unique identifier to put in the @id@ attribute.
  -- This is shared with any 'Reflex.Stripe.Elements.Object.StripeElements' that are created using this.
  }

-- |Initialize the Stripe.js object given the publishable API token.
initStripe :: MonadJSM m => Text -> m Stripe
initStripe apiToken = liftJSM $ do
  _stripe_object <- jsg1 ("Stripe" :: Text) apiToken
  _stripe_nextElementId <- liftIO $ newMVar 0
  pure Stripe {..}

data StripeCreateToken = StripeCreateToken
  { _stripeCreateToken_name :: Maybe Text
  , _stripeCreateToken_addressLine1 :: Maybe Text
  , _stripeCreateToken_addressLine2 :: Maybe Text
  , _stripeCreateToken_addressCity :: Maybe Text
  , _stripeCreateToken_addressState :: Maybe Text
  , _stripeCreateToken_addressZip :: Maybe Text
  , _stripeCreateToken_addressCountry :: Maybe Text
  , _stripeCreateToken_currency :: Maybe Text
  }

instance Default StripeCreateToken where
  def = StripeCreateToken Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSVal StripeCreateToken where
  toJSVal (StripeCreateToken {..}) = do
    o <- create
    for_ _stripeCreateToken_name           $ \ v -> toJSVal v >>= \ jsv -> setProp "name" jsv o
    for_ _stripeCreateToken_addressLine1   $ \ v -> toJSVal v >>= \ jsv -> setProp "address_line1" jsv o
    for_ _stripeCreateToken_addressLine2   $ \ v -> toJSVal v >>= \ jsv -> setProp "address_line2" jsv o
    for_ _stripeCreateToken_addressCity    $ \ v -> toJSVal v >>= \ jsv -> setProp "address_city" jsv o
    for_ _stripeCreateToken_addressState   $ \ v -> toJSVal v >>= \ jsv -> setProp "address_state" jsv o
    for_ _stripeCreateToken_addressZip     $ \ v -> toJSVal v >>= \ jsv -> setProp "address_zip" jsv o
    for_ _stripeCreateToken_addressCountry $ \ v -> toJSVal v >>= \ jsv -> setProp "address_country" jsv o
    for_ _stripeCreateToken_currency       $ \ v -> toJSVal v >>= \ jsv -> setProp "currency" jsv o
    toJSVal o

-- |Result of a 'createStripeToken' call, carrying either the token or an error.
data StripeCreateTokenResponse
  = StripeCreateTokenSuccess StripeToken
  | StripeCreateTokenError StripeError
  deriving (Eq, Show)

instance FromJSVal StripeCreateTokenResponse where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv -> do
      o <- makeObject jsv
      errMay <- fromJSVal =<< getProp "error" o
      case errMay of
        Just err -> pure $ StripeCreateTokenError err
        Nothing -> StripeCreateTokenSuccess <$> (fromJSValUnchecked =<< getProp "token" o)

-- |Create a token from a given Stripe Element whenever the given event fires. Stripe will collect data from the given element along with any other elements
-- created from the same 'StripeElements' that it decides are useful.
--
-- Returns an 'Event' which will fire when Stripe returns a response.
createStripeTokens
  :: (IsStripeElement el, MonadJSM m, PerformEvent t m, MonadJSM (Performable m), TriggerEvent t m)
  => Stripe -> el -> StripeCreateToken -> Event t () -> m (Event t StripeCreateTokenResponse)
createStripeTokens (Stripe { _stripe_object }) el options triggerEvent = do
  (resultEvent, triggerResult) <- newTriggerEvent
  performEvent_ . ffor triggerEvent $ \ _ -> liftJSM $ do
    funRef <- liftIO $ newIORef Nothing
    fun <- function $ \ _ _ args -> do
      result <- fromJSValUnchecked (fromMaybe jsNull $ listToMaybe args)
      liftIO $ triggerResult result
      maybe (pure ()) freeFunction =<< liftIO (readIORef funRef)
      liftIO $ writeIORef funRef Nothing
    liftIO . writeIORef funRef . Just $ fun
    void $ (_stripe_object ^. js2 ("createToken" :: Text) (stripeElement el) options) ^. js1 ("then" :: Text) fun
  pure resultEvent

