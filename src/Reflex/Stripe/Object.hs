module Reflex.Stripe.Object where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, putMVar, takeMVar)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad.Trans (liftIO)
import Data.Default (Default, def)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Traversable (for, traverse)
import Language.Javascript.JSaddle
  ( FromJSVal, fromJSVal, fromJSValUnchecked, ToJSVal, toJSVal
  , function, freeFunction
  , JSVal, MonadJSM, liftJSM
  , jsg, jsg1, js1, js2, jsNull, create, makeObject, unsafeGetProp, setProp, maybeNullOrUndefined'
  )
import Reflex.Dom (Event, ffor, Performable, PerformEvent, performEventAsync, TriggerEvent)
import Reflex.Stripe.Elements.Types (IsStripeElement, stripeElement)
import Reflex.Stripe.Types (StripeError(StripeError), StripeToken)

-- |Shared state after initializing the Stripe.js library.
data Stripe = Stripe
  { _stripe_object :: JSVal
  -- ^The Stripe object - https://stripe.com/docs/stripe.js#the-stripe-object
  , _stripe_nextElementId :: MVar Integer
  -- ^The next unique identifier to use when creating Stripe Elements, since to mount them we need a document unique identifier to put in the @id@ attribute.
  -- This is shared with any 'Reflex.Stripe.Elements.Object.StripeElements' that are created using this.
  }

makeLenses ''Stripe

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

makeLenses ''StripeCreateToken

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

makePrisms ''StripeCreateTokenResponse

instance FromJSVal StripeCreateTokenResponse where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv -> do
      o <- makeObject jsv
      errMay <- fromJSVal =<< unsafeGetProp "error" o
      case errMay of
        Just err -> pure $ StripeCreateTokenError err
        Nothing ->
          StripeCreateTokenSuccess <$> (fromJSValUnchecked =<< unsafeGetProp "token" o)

-- |Create a token from a given Stripe Element whenever the given event fires. Stripe will collect data from the given element along with any other elements
-- created from the same 'StripeElements' that it decides are useful.
--
-- The parameters of the create token request are wrapped in a 'Traversable' @f@, which you can use to make multiple requests (e.g. @[]@), side channel values
-- from the request to the response (@(,) a@) or just make a plain request (@Identity@).
--
-- Returns an 'Event' which will fire when Stripe returns response(s). Note that if there is a failure parsing a response from stripe, this will synthesize
-- a 'StripeCreateTokenError' with type of @invalid_response_error@, and log the unparsed response on the console.
createStripeTokens
  :: (IsStripeElement el, PerformEvent t m, TriggerEvent t m, MonadJSM m, MonadJSM (Performable m), Traversable f)
  => Stripe -> Event t (f (el, StripeCreateToken)) -> m (Event t (f StripeCreateTokenResponse))
createStripeTokens (Stripe { _stripe_object }) requestses =
  performEventAsync $ ffor requestses $ \ requests callback -> do
    responseRefs <- for requests $ \ (el, options) -> do
      responseRef <- liftIO newEmptyMVar
      funRef <- liftIO $ newIORef Nothing
      fun <- liftJSM $ function $ \ _ _ args -> do
        resultMay <- fromJSVal (fromMaybe jsNull $ listToMaybe args)
        case resultMay of
          Just result ->
            liftIO $ putMVar responseRef result
          Nothing -> do
            liftJSM . void $ jsg ("console" :: Text) ^. js2 ("log" :: Text) ("Failed to decode result of stripe.createToken:" :: Text) args
            liftIO . putMVar responseRef . StripeCreateTokenError $ StripeError "invalid_response_error" Nothing Nothing Nothing Nothing Nothing
        maybe (pure ()) freeFunction =<< liftIO (readIORef funRef)
        liftIO $ writeIORef funRef Nothing
      liftIO . writeIORef funRef . Just $ fun
      liftJSM . void $ (_stripe_object ^. js2 ("createToken" :: Text) (stripeElement el) options) ^. js1 ("then" :: Text) fun
      pure responseRef
    void . liftIO . forkIO $ callback =<< traverse takeMVar responseRefs

