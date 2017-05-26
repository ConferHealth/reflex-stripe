module Reflex.Stripe.Elements.Card where

import Control.Concurrent.MVar (putMVar, takeMVar)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Default (Default, def)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Language.Javascript.JSaddle
  ( FromJSVal, fromJSVal, fromJSValUnchecked, JSVal, ToJSVal, toJSVal
  , js1, js2, jsFalse, jsTrue
  , Object, create, makeObject, unsafeGetProp, setProp, maybeNullOrUndefined'
  , JSM, MonadJSM, liftJSM )
import Reflex.Dom
  ( (=:), DomBuilder, DomBuilderSpace, Element, Event, EventResult, elAttr', ffilter, ffor, HasMountStatus, getMountStatus, MountState(Mounted)
  , getMountStatus, Performable, PerformEvent, performEvent_, TriggerEvent, uniqDyn, updated
  )
import Reflex.Stripe.Elements.Object (StripeElements(StripeElements), _stripeElements_object, _stripeElements_nextElementId)
import Reflex.Stripe.Elements.Types (IsStripeElement, stripeElement, StripeElementConfig, stripeElementConfigToObject, StripeElementError, getStripeElementEvent)

-- |Data type lifted to the kind level representing whether a postal code field is present inside a Stripe combined card field.
data HasPostalCodeField
  = PostalCodeEnabled
  | PostalCodeDisabled

-- |GADT which is like a type-level @Maybe@ that is always @Just@ if @pcf ~ HasPostalCodeField@ and always @Nothing@ otherwise.
data WithPostalCodeField (postalCode :: HasPostalCodeField) a where
  PostalCode :: a -> WithPostalCodeField 'PostalCodeEnabled a
  NoPostalCode :: WithPostalCodeField 'PostalCodeDisabled a

deriving instance Functor (WithPostalCodeField postalCode)
deriving instance Eq a => Eq (WithPostalCodeField postalCode a)
deriving instance Show a => Show (WithPostalCodeField postalCode a)

-- |Configuration for a Stripe combined-style card element (type @card@)
data StripeCardElementConfig (postalCode :: HasPostalCodeField) = StripeCardElementConfig
  { _stripeCardElementConfig_stripeElementConfig :: StripeElementConfig
  -- ^Configuration information shared between types of Stripe element
  , _stripeCardElementConfig_postalCode :: WithPostalCodeField postalCode (Maybe Text)
  -- ^Controls both whether a postal code field is shown, and if it is shown an optional initial value to prefill it with.
  }

makeLenses ''StripeCardElementConfig

instance ToJSVal (StripeCardElementConfig pcf) where
  toJSVal (StripeCardElementConfig {..}) = do
    o <- stripeElementConfigToObject _stripeCardElementConfig_stripeElementConfig

    case _stripeCardElementConfig_postalCode of
      PostalCode vMay -> do
        for_ vMay $ \ v -> do
          jsv <- toJSVal v
          value <- create
          setProp "postalCode" jsv value
          valueJsv <- toJSVal value
          setProp "value" valueJsv o
        setProp "hidePostalCode" jsFalse o
      _ ->
        setProp "hidePostalCode" jsTrue o

    toJSVal o

instance Default (StripeCardElementConfig 'PostalCodeEnabled) where
  def = StripeCardElementConfig def (PostalCode Nothing)

instance Default (StripeCardElementConfig 'PostalCodeDisabled) where
  def = StripeCardElementConfig def NoPostalCode

-- |Structure containing the underlying reference to and Reflex signals for a Stripe combined card element.
data StripeCardElement (postalCode :: HasPostalCodeField) t m = StripeCardElement
  { _stripeCardElement_domElement :: Element EventResult (DomBuilderSpace m) t
  , _stripeCardElement_stripeElement :: JSVal
  , _stripeCardElement_hasPostalCode :: WithPostalCodeField postalCode ()
  }

makeLenses ''StripeCardElement

instance IsStripeElement (StripeCardElement postalCode t m) where
  stripeElement = _stripeCardElement_stripeElement

-- |A Stripe combined card, expiry, CVV, and optionally postal code field.
stripeCardElement
  :: forall (postalCode :: HasPostalCodeField) t m.
     ( DomBuilder t m
     , MonadIO m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m)
     , HasMountStatus t m
     )
  => StripeElements -> StripeCardElementConfig postalCode -> m (StripeCardElement postalCode t m)
stripeCardElement (StripeElements { _stripeElements_object, _stripeElements_nextElementId }) config = do
  elementId <- liftIO $ takeMVar _stripeElements_nextElementId
  liftIO $ putMVar _stripeElements_nextElementId (succ elementId)
  let elementIdStr = "reflex-stripe-element-" <> (pack . show) elementId
  (_stripeCardElement_domElement, _) <- elAttr' "div" ("id" =: elementIdStr) (pure ())
  _stripeCardElement_stripeElement <- liftJSM $ _stripeElements_object ^. js2 ("create" :: Text) ("card" :: Text) config
  let _stripeCardElement_hasPostalCode = void $ _stripeCardElementConfig_postalCode config
  whenMounted <- ffilter (== Mounted) . updated . uniqDyn <$> getMountStatus
  performEvent_ . ffor whenMounted $ \ _ ->
    void . liftJSM $
      _stripeCardElement_stripeElement ^. js1 ("mount" :: Text) ("#" <> elementIdStr)
  pure $ StripeCardElement {..}

-- |A Stripe combined card, expiry, CVV, and no postal code field.
stripeCardElementWithoutPostalCode
  :: forall t m.
     ( DomBuilder t m
     , MonadIO m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m)
     , HasMountStatus t m
     )
  => StripeElements -> StripeCardElementConfig 'PostalCodeDisabled -> m (StripeCardElement 'PostalCodeDisabled t m)
stripeCardElementWithoutPostalCode = stripeCardElement

-- |A Stripe combined card, expiry, CVV, and a postal code field.
stripeCardElementWithPostalCode
  :: forall t m.
     ( DomBuilder t m
     , MonadIO m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m)
     , HasMountStatus t m
     )
  => StripeElements -> StripeCardElementConfig 'PostalCodeEnabled -> m (StripeCardElement 'PostalCodeEnabled t m)
stripeCardElementWithPostalCode = stripeCardElement

-- |Structure holding details of a change event for a card element.
data StripeCardElementChange (postalCode :: HasPostalCodeField) = StripeCardElementChange
  { _stripeCardElementChange_empty :: Bool
  -- ^Whether the element is now empty (has no value)
  , _stripeCardElementChange_complete :: Bool
  -- ^Whether the element is now complete (has a valid value)
  , _stripeCardElementChange_brand :: Maybe Text
  -- ^The brand of credit card. The Stripe documentation is not particularly clear on whether this field is always given, and what values it might take,
  -- so the type is left fairly unconstrained.
  , _stripeCardElementChange_error :: Maybe StripeElementError
  -- ^The current validation error, if any.
  , _stripeCardElementChange_postalCode :: WithPostalCodeField postalCode Text
  -- ^The current value of the postal code subfield, if the combined card field has a postal code field.
  }
  deriving (Eq, Show)

-- ^Parse some 'JSVal' into a 'StripeCardElementChange', delegating the postal code specific portion to a given function. Used to implement 'FromJSVal' for
-- both 'StripeCardElementChange' variants.
parseStripeCardElementChange :: (Object -> JSM (WithPostalCodeField postalCode Text)) -> JSVal -> JSM (Maybe (StripeCardElementChange postalCode))
parseStripeCardElementChange parsePostalCode =
  maybeNullOrUndefined' $ \ jsv -> do
    o <- makeObject jsv
    _stripeCardElementChange_empty      <- fromJSValUnchecked =<< unsafeGetProp "empty" o
    _stripeCardElementChange_complete   <- fromJSValUnchecked =<< unsafeGetProp "complete" o
    _stripeCardElementChange_brand      <- fromJSVal          =<< unsafeGetProp "brand" o
    _stripeCardElementChange_error      <- fromJSVal          =<< unsafeGetProp "error" o
    _stripeCardElementChange_postalCode <- parsePostalCode o
    pure StripeCardElementChange {..}

instance FromJSVal (StripeCardElementChange 'PostalCodeEnabled) where
  fromJSVal = parseStripeCardElementChange $ \ o ->
    fmap PostalCode $ fromJSValUnchecked =<< unsafeGetProp "postalCode" =<< makeObject =<< unsafeGetProp "value" o

-- |Get an Event which fires each time the state of a Stripe combined card element changes.
getStripeCardElementOnChange
  :: (FromJSVal (StripeCardElementChange postalCode), MonadJSM m, TriggerEvent t m)
  => StripeCardElement postalCode t m -> m (Event t (StripeCardElementChange postalCode))
getStripeCardElementOnChange = getStripeElementEvent "change" fromJSValUnchecked

