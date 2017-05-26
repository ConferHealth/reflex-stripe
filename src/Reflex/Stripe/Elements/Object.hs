module Reflex.Stripe.Elements.Object where

import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Data.Default (Default, def)
import Data.Foldable (for_)
import Data.Text (Text)
import Language.Javascript.JSaddle (JSVal, js1, MonadJSM, liftJSM, ToJSVal, toJSVal, create, setProp)
import Reflex.Stripe.Object (Stripe(Stripe), _stripe_object, _stripe_nextElementId)

-- |Structure to configure a custom font face to use with Stripe Elements
data StripeElementFontConfig = StripeElementFontConfig
  { _stripeElementFontConfig_family :: Text
  , _stripeElementFontConfig_src :: Text
  , _stripeElementFontConfig_style :: Maybe Text
  , _stripeElementFontConfig_unicodeRange :: Maybe Text
  , _stripeElementFontConfig_weight :: Maybe Text
  }

instance ToJSVal StripeElementFontConfig where
  toJSVal (StripeElementFontConfig {..}) = do
    o <- create
    toJSVal _stripeElementFontConfig_family >>= \ jsv -> setProp "family" jsv o
    toJSVal _stripeElementFontConfig_src    >>= \ jsv -> setProp "src" jsv o
    for_ _stripeElementFontConfig_style        $ \ v -> toJSVal v >>= \ jsv -> setProp "style" jsv o
    for_ _stripeElementFontConfig_unicodeRange $ \ v -> toJSVal v >>= \ jsv -> setProp "unicodeRange" jsv o
    for_ _stripeElementFontConfig_weight       $ \ v -> toJSVal v >>= \ jsv -> setProp "weight" jsv o
    toJSVal o

-- |Structure holding configuration parameters when initializing Stripe Elements
data StripeElementsConfig = StripeElementsConfig
  { _stripeElementsConfig_fonts :: [StripeElementFontConfig]
  -- ^Any custom font declarations that should be made within the Stripe Element. Stripe Elements runs within an @<iframe>@ so no enclosing CSS font
  -- definitions will be available.
  , _stripeElementsConfig_locale :: Maybe Text
  -- ^What locale to use for placeholders and error messages. @Nothing@ means infer locale automatically from the browser.
  }

instance ToJSVal StripeElementsConfig where
  toJSVal (StripeElementsConfig {..}) = do
    o <- create
    toJSVal _stripeElementsConfig_fonts >>= \ jsv -> setProp "fonts" jsv o
    for_ _stripeElementsConfig_locale $ \ v -> toJSVal v >>= \ jsv -> setProp "locale" jsv o
    toJSVal o

instance Default StripeElementsConfig where
  def = StripeElementsConfig [] Nothing

-- |Shared state after initializing the Stripe.js library.
data StripeElements = StripeElements
  { _stripeElements_object :: JSVal
  -- ^The Stripe Elements object - https://stripe.com/docs/stripe.js#the-elements-object
  , _stripeElements_nextElementId :: MVar Integer
  -- ^The next unique identifier to use when creating Stripe Elements, since to mount them we need a document unique identifier to put in the @id@ attribute.
  -- This is inherited from @_stripe_nextElementId@.
  }

-- |Initialize Stripe Elements using an already initialized Stripe object.
initStripeElements :: MonadJSM m => Stripe -> StripeElementsConfig -> m StripeElements
initStripeElements (Stripe { _stripe_object, _stripe_nextElementId }) config = liftJSM $ do
  _stripeElements_object <- _stripe_object ^. js1 ("elements" :: Text) config
  let _stripeElements_nextElementId = _stripe_nextElementId
  pure StripeElements {..}


