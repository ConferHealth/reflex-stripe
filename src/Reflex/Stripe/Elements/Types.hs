module Reflex.Stripe.Elements.Types where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad ((<=<))
import Control.Monad.Trans (liftIO)
import Data.Default (Default, def)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Language.Javascript.JSaddle
  ( FromJSVal, fromJSVal, fromJSValUnchecked, ToJSVal, toJSVal
  , function
  , JSVal, JSM, MonadJSM, liftJSM
  , js0, js2, jsNull
  , Object, create, makeObject, setProp, unsafeGetProp, maybeNullOrUndefined'
  )
import Prelude
import Reflex.Dom (Event, TriggerEvent, newTriggerEvent)

-- |Style properties that can be applied ad-hoc to a Stripe element
data StripeElementStyle = StripeElementStyle
  { _stripeElementStyle_color               :: Maybe Text
  , _stripeElementStyle_fontFamily          :: Maybe Text
  , _stripeElementStyle_fontSize            :: Maybe Text
  , _stripeElementStyle_fontSmoothing       :: Maybe Text
  , _stripeElementStyle_fontStyle           :: Maybe Text
  , _stripeElementStyle_fontVariant         :: Maybe Text
  , _stripeElementStyle_iconColor           :: Maybe Text
  , _stripeElementStyle_lineHeight          :: Maybe Text
  , _stripeElementStyle_letterSpacing       :: Maybe Text
  , _stripeElementStyle_textDecoration      :: Maybe Text
  , _stripeElementStyle_textShadow          :: Maybe Text
  , _stripeElementStyle_textTransform       :: Maybe Text
  , _stripeElementStyle_hoverStyle          :: Maybe StripeElementStyle
  , _stripeElementStyle_focusStyle          :: Maybe StripeElementStyle
  , _stripeElementStyle_placeholderStyle    :: Maybe StripeElementStyle
  , _stripeElementStyle_selectionStyle      :: Maybe StripeElementStyle
  , _stripeElementStyle_webkitAutofillStyle :: Maybe StripeElementStyle
  }
  deriving (Eq, Show)

instance Default StripeElementStyle where
  def = StripeElementStyle Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                           Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

makeLenses ''StripeElementStyle

instance ToJSVal StripeElementStyle where
  toJSVal (StripeElementStyle {..}) = do
    o <- create
    for_ _stripeElementStyle_color               $ \ v -> toJSVal v >>= \ jsv -> setProp "color"             jsv o
    for_ _stripeElementStyle_fontFamily          $ \ v -> toJSVal v >>= \ jsv -> setProp "fontFamily"        jsv o
    for_ _stripeElementStyle_fontSize            $ \ v -> toJSVal v >>= \ jsv -> setProp "fontSize"          jsv o
    for_ _stripeElementStyle_fontSmoothing       $ \ v -> toJSVal v >>= \ jsv -> setProp "fontSmoothing"     jsv o
    for_ _stripeElementStyle_fontStyle           $ \ v -> toJSVal v >>= \ jsv -> setProp "fontStyle"         jsv o
    for_ _stripeElementStyle_fontVariant         $ \ v -> toJSVal v >>= \ jsv -> setProp "fontVariant"       jsv o
    for_ _stripeElementStyle_iconColor           $ \ v -> toJSVal v >>= \ jsv -> setProp "iconColor"         jsv o
    for_ _stripeElementStyle_lineHeight          $ \ v -> toJSVal v >>= \ jsv -> setProp "lineHeight"        jsv o
    for_ _stripeElementStyle_letterSpacing       $ \ v -> toJSVal v >>= \ jsv -> setProp "letterSpacing"     jsv o
    for_ _stripeElementStyle_textDecoration      $ \ v -> toJSVal v >>= \ jsv -> setProp "textDecoration"    jsv o
    for_ _stripeElementStyle_textShadow          $ \ v -> toJSVal v >>= \ jsv -> setProp "textShadow"        jsv o
    for_ _stripeElementStyle_textTransform       $ \ v -> toJSVal v >>= \ jsv -> setProp "textTransform"     jsv o
    for_ _stripeElementStyle_hoverStyle          $ \ s -> toJSVal s >>= \ jsv -> setProp ":hover"            jsv o
    for_ _stripeElementStyle_focusStyle          $ \ s -> toJSVal s >>= \ jsv -> setProp ":focus"            jsv o
    for_ _stripeElementStyle_placeholderStyle    $ \ s -> toJSVal s >>= \ jsv -> setProp "::placeholder"     jsv o
    for_ _stripeElementStyle_selectionStyle      $ \ s -> toJSVal s >>= \ jsv -> setProp "::selection"       jsv o
    for_ _stripeElementStyle_webkitAutofillStyle $ \ s -> toJSVal s >>= \ jsv -> setProp ":-webkit-autofill" jsv o
    toJSVal o

-- |How icons should be rendered within a Stripe element, either monochromatic solid or shaded (default)
data StripeIconStyle
  = StripeIconStyleSolid
  | StripeIconStyleDefault
  deriving (Eq, Show)

makePrisms ''StripeIconStyle

instance ToJSVal StripeIconStyle where
  toJSVal = \ case
    StripeIconStyleSolid   -> toJSVal ("solid" :: Text)
    StripeIconStyleDefault -> toJSVal ("default" :: Text)

-- |Common configuration for any Stripe element.
data StripeElementConfig = StripeElementConfig
  { _stripeElementConfig_baseClass :: Maybe Text
  -- ^CSS class to apply always.
  , _stripeElementConfig_completeClass :: Maybe Text
  -- ^CSS class to apply when the element has been filled out and is valid.
  , _stripeElementConfig_emptyClass :: Maybe Text
  -- ^CSS class to apply when the element has no value.
  , _stripeElementConfig_focusClass :: Maybe Text
  -- ^CSS class to apply when the element has focus.
  , _stripeElementConfig_invalidClass :: Maybe Text
  -- ^CSS class to apply when a value has been entered but is invalid.
  , _stripeElementConfig_webkitAutofillClass :: Maybe Text
  -- ^CSS class to apply when the value has been autofilled by the browser (on any browser which supports the :-webkit-autofill pseudoclass.
  , _stripeElementConfig_hideIcon :: Bool
  -- ^Whether to hide any icons that would appear within the element.
  , _stripeElementConfig_iconStyle :: Maybe StripeIconStyle
  -- ^Override the icon style for any icons that would appear within the element.
  , _stripeElementConfig_baseStyle :: Maybe StripeElementStyle
  -- ^CSS style properties to apply always.
  , _stripeElementConfig_completeStyle :: Maybe StripeElementStyle
  -- ^CSS style properties to apply when the element has a value and is valid.
  , _stripeElementConfig_emptyStyle :: Maybe StripeElementStyle
  -- ^CSS style properties to apply when the element has no value.
  , _stripeElementConfig_invalidStyle :: Maybe StripeElementStyle
  -- ^CSS style properties to apply when the element has a value but is invalid.
  }
  deriving (Eq, Show)

makeLenses ''StripeElementConfig

-- |Convert a 'StripeElementConfig' to a JS object. Identical to 'toJSVal' but returns the 'Obj.Object' wrapper, rather than a 'JSVal'.
stripeElementConfigToObject :: StripeElementConfig -> JSM Object
stripeElementConfigToObject (StripeElementConfig {..}) = do
  classes <- create
  for_ _stripeElementConfig_baseClass           $ \ v -> toJSVal v >>= \ jsv -> setProp "base"           jsv classes
  for_ _stripeElementConfig_completeClass       $ \ v -> toJSVal v >>= \ jsv -> setProp "complete"       jsv classes
  for_ _stripeElementConfig_emptyClass          $ \ v -> toJSVal v >>= \ jsv -> setProp "empty"          jsv classes
  for_ _stripeElementConfig_focusClass          $ \ v -> toJSVal v >>= \ jsv -> setProp "focus"          jsv classes
  for_ _stripeElementConfig_invalidClass        $ \ v -> toJSVal v >>= \ jsv -> setProp "invalid"        jsv classes
  for_ _stripeElementConfig_webkitAutofillClass $ \ v -> toJSVal v >>= \ jsv -> setProp "webkitAutofill" jsv classes
  classesJsv <- toJSVal classes

  style <- create
  for_ _stripeElementConfig_baseStyle     $ \ v -> toJSVal v >>= \ jsv -> setProp "base"     jsv style
  for_ _stripeElementConfig_completeStyle $ \ v -> toJSVal v >>= \ jsv -> setProp "complete" jsv style
  for_ _stripeElementConfig_emptyStyle    $ \ v -> toJSVal v >>= \ jsv -> setProp "empty"    jsv style
  for_ _stripeElementConfig_invalidStyle  $ \ v -> toJSVal v >>= \ jsv -> setProp "invalid"  jsv style
  styleJsv <- toJSVal style

  o <- create
  toJSVal _stripeElementConfig_hideIcon >>= \ jsv -> setProp "hideIcon" jsv o
  setProp "classes" classesJsv o
  setProp "style" styleJsv o
  for_ _stripeElementConfig_iconStyle $ \ v -> toJSVal v >>= \ jsv -> setProp "iconStyle" jsv o
  pure o

instance ToJSVal StripeElementConfig where
  toJSVal = toJSVal <=< stripeElementConfigToObject

instance Default StripeElementConfig where
  def = StripeElementConfig Nothing Nothing Nothing Nothing Nothing Nothing False Nothing Nothing Nothing Nothing Nothing

-- |Class of the various types of Stripe element, all of which the common functions 'blurStripeElement', 'clearStripeElement', and 'focusStripeElement'
-- work on.
class IsStripeElement el where
  -- |Get the underlying Stripe Element object.
  stripeElement :: el -> JSVal

-- |Remove focus from a given Stripe element
blurStripeElement :: (IsStripeElement el, MonadJSM m) => el -> m ()
blurStripeElement el = void . liftJSM $ stripeElement el ^. js0 ("blur" :: Text)

-- |Clear value from a given Stripe element
clearStripeElement :: (IsStripeElement el, MonadJSM m) => el -> m ()
clearStripeElement el = void . liftJSM $ stripeElement el ^. js0 ("clear" :: Text)

-- |Focus on a given Stripe element
focusStripeElement :: (IsStripeElement el, MonadJSM m) => el -> m ()
focusStripeElement el = void . liftJSM $ stripeElement el ^. js0 ("focus" :: Text)

-- |Structure reporting a validation error in an Stripe Element.
data StripeElementError = StripeElementError
  { _stripeElementError_message :: Text
  , _stripeElementError_code :: Text
  }
  deriving (Eq, Show)

makeLenses ''StripeElementError

instance FromJSVal StripeElementError where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv -> do
      o <- makeObject jsv
      _stripeElementError_message <- fromJSValUnchecked =<< unsafeGetProp "message" o
      _stripeElementError_code    <- fromJSValUnchecked =<< unsafeGetProp "code" o
      pure StripeElementError {..}

-- |Generic way to bind to an event using @Element.on@.
getStripeElementEvent :: (IsStripeElement el, MonadJSM m, TriggerEvent t m) => Text -> (JSVal -> JSM a) -> el -> m (Event t a)
getStripeElementEvent eventType parseEvent el = do
  let element = stripeElement el
  -- FIXME would newEventWithLazyTriggerWithOnComplete be good to use here?
  (ev, trigger) <- newTriggerEvent
  liftJSM $ do
    funJsv <- function $ \ _ _ args -> do
      a <- parseEvent (fromMaybe jsNull $ listToMaybe args)
      liftIO $ trigger a
    -- FIXME Function never gets explicitly freed
    void $ element ^. js2 ("on" :: Text) eventType funJsv
  pure ev

-- |Retrieve an Event which fires whenever the given Stripe Element becomes blurred
getStripeElementOnBlur :: (IsStripeElement el, MonadJSM m, TriggerEvent t m) => el -> m (Event t ())
getStripeElementOnBlur = getStripeElementEvent "blur" (\ _ -> pure ())

-- |Retrieve an Event which fires whenever the given Stripe Element becomes focused
getStripeElementOnFocus :: (IsStripeElement el, MonadJSM m, TriggerEvent t m) => el -> m (Event t ())
getStripeElementOnFocus = getStripeElementEvent "focus" (\ _ -> pure ())

-- |Retrieve an Event which fires whenever the given Stripe Element becomes ready (mounted, fully rendered, and can be focused)
getStripeElementOnReady :: (IsStripeElement el, MonadJSM m, TriggerEvent t m) => el -> m (Event t ())
getStripeElementOnReady = getStripeElementEvent "ready" (\ _ -> pure ())
--
-- |Structure holding details of a change event for a Stripe element.
data StripeElementChange = StripeElementChange
  { _stripeElementChange_empty :: Bool
  -- ^Whether the element is now empty (has no value)
  , _stripeElementChange_complete :: Bool
  -- ^Whether the element is now complete (has a valid value)
  , _stripeElementChange_error :: Maybe StripeElementError
  -- ^The current validation error, if any.
  }
  deriving (Eq, Show)

makeLenses ''StripeElementChange

instance FromJSVal StripeElementChange where
  fromJSVal =
    maybeNullOrUndefined' $ \ jsv -> do
      o <- makeObject jsv
      _stripeElementChange_empty      <- fromJSValUnchecked =<< unsafeGetProp "empty" o
      _stripeElementChange_complete   <- fromJSValUnchecked =<< unsafeGetProp "complete" o
      _stripeElementChange_error      <- maybeNullOrUndefined' fromJSValUnchecked =<< unsafeGetProp "error" o
      pure StripeElementChange {..}

-- |Retrieve an Event which fires whenever the given Stripe Element changes.
-- Note this works for any Stripe element but will not report element type specific details, e.g. postal code.
getStripeElementOnChange :: (IsStripeElement el, MonadJSM m, TriggerEvent t m) => el -> m (Event t StripeElementChange)
getStripeElementOnChange = getStripeElementEvent "change" fromJSValUnchecked

