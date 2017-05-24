module Reflex.Stripe.Object where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified JavaScript.Object.Internal as Obj
import Language.Javascript.JSaddle
  ( FromJSVal, fromJSVal, fromJSValUnchecked, ToJSVal, toJSVal
  , function, freeFunction
  , JSVal, MonadJSM, liftJSM
  , js, jsg1, js1, js2, jsNull
  )
import Reflex.Dom (Event, TriggerEvent, newTriggerEvent)
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

instance ToJSVal StripeCreateToken where
  toJSVal (StripeCreateToken {..}) = do
    o@(Obj.Object oJsv) <- Obj.create
    for_ _stripeCreateToken_name           $ \ v -> toJSVal v >>= \ jsv -> Obj.setProp "name" jsv o
    for_ _stripeCreateToken_addressLine1   $ \ v -> toJSVal v >>= \ jsv -> Obj.setProp "address_line1" jsv o
    for_ _stripeCreateToken_addressLine2   $ \ v -> toJSVal v >>= \ jsv -> Obj.setProp "address_line2" jsv o
    for_ _stripeCreateToken_addressCity    $ \ v -> toJSVal v >>= \ jsv -> Obj.setProp "address_city" jsv o
    for_ _stripeCreateToken_addressState   $ \ v -> toJSVal v >>= \ jsv -> Obj.setProp "address_state" jsv o
    for_ _stripeCreateToken_addressZip     $ \ v -> toJSVal v >>= \ jsv -> Obj.setProp "address_zip" jsv o
    for_ _stripeCreateToken_addressCountry $ \ v -> toJSVal v >>= \ jsv -> Obj.setProp "address_country" jsv o
    for_ _stripeCreateToken_currency       $ \ v -> toJSVal v >>= \ jsv -> Obj.setProp "currency" jsv o
    pure oJsv

-- |Result of a 'createStripeToken' call, carrying either the token or an error.
data StripeCreateTokenResponse
  = StripeCreateTokenSuccess StripeToken
  | StripeCreateTokenError StripeError

instance FromJSVal StripeCreateTokenResponse where
  fromJSVal jsv = do
    errMay <- fromJSVal =<< jsv ^. js ("error" :: Text)
    case errMay of
      Just err -> pure . Just $ StripeCreateTokenError err
      Nothing -> Just . StripeCreateTokenSuccess <$> (fromJSValUnchecked =<< jsv ^. js ("token" :: Text))

-- |Create a token from a given Stripe Element. Stripe will collect data from the given element along with any other elements created from the same
-- 'StripeElements' that it decides are useful.
--
-- Returns an 'Event' which will fire when Stripe returns a response.
createStripeToken :: (IsStripeElement el, MonadJSM m, TriggerEvent t m) => Stripe -> el -> StripeCreateToken -> m (Event t StripeCreateTokenResponse)
createStripeToken (Stripe { _stripe_object }) el options = do
  (ev, trigger) <- newTriggerEvent
  liftJSM $ do
    funRef <- liftIO $ newIORef Nothing
    fun <- function $ \ _ _ args -> do
      result <- fromJSValUnchecked (fromMaybe jsNull $ listToMaybe args)
      liftIO $ trigger result
      maybe (pure ()) freeFunction =<< liftIO (readIORef funRef)
      liftIO $ writeIORef funRef Nothing
    liftIO . writeIORef funRef . Just $ fun
    void $ (_stripe_object ^. js2 ("createToken" :: Text) (stripeElement el) options) ^. js1 ("then" :: Text) fun
  pure ev

