module Main where

import Control.Monad ((<=<))
import Control.Monad.Trans (liftIO)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Functor ((<$), void)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Text (Text, pack)
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.NonElementParentNode (getElementById)
import Reflex.Dom
  ( (=:), Dynamic, Event, MonadWidget, attachWidget, button, current, dyn, el, elAttr, ffor, foldDyn, hold, holdDyn, never, performEvent, switch, tag
  , text, _textInput_value, textInput, withJSContextSingleton )
import Reflex.Stripe

main :: IO ()
main =
  withJSContextSingleton $ \ jsSing -> do
    doc <- currentDocumentUnchecked
    appMountpoint <- maybe (fail "couldn't find #app mountpoint") pure =<< getElementById doc ("app" :: Text)
    attachWidget appMountpoint jsSing app

app :: MonadWidget t m => m ()
app = do
  rec
    stripeObjectMay <- holdDyn Nothing (Just <$> newStripe)
    newStripe <- fmap switch . hold never <=< dyn . ffor stripeObjectMay $ \ case
      Just stripeObject -> do
        never <$ withStripe stripeObject
      Nothing ->
        withoutStripe
  pure ()

withoutStripe :: MonadWidget t m => m (Event t Stripe)
withoutStripe = do
  elAttr "form" ("action" =: "javascript:") $ do
    apiToken <- el "label" $ do
      text "Stripe publishable token: "
      _textInput_value <$> textInput def
    b <- button "Initialize"

    performEvent $ initStripe <$> tag (current apiToken) b

withStripe :: forall t m. MonadWidget t m => Stripe -> m ()
withStripe stripe = do
  elAttr "div" ("style" =: "border-bottom: solid 2px #000000") $ do
    el "h2" $ text "Combined with Postal Code"
    combinedWithPostalCode stripe
  elAttr "div" ("style" =: "border-bottom: solid 2px #000000") $ do
    el "h2" $ text "Combined without Postal Code"
    combinedWithoutPostalCode stripe
  el "div" $ do
    el "h2" $ text "Separate elements"
    separated stripe

timeAndDisplayEvents :: forall t m. MonadWidget t m => Event t [Text] -> m ()
timeAndDisplayEvents newEvents = do
  timedEvents :: Event t (UTCTime, [Text]) <- performEvent $ ffor newEvents $ \ a -> (,a) <$> liftIO getCurrentTime

  events :: Dynamic t (Seq (UTCTime, [Text])) <- foldDyn (flip (|>)) Seq.empty timedEvents

  void . dyn $ ffor events $ \ evs ->
    for_ evs $ \ (t, es) ->
      for_ es $ \ e -> 
        el "div" $ do
          el "b" . text $ (pack . show $ t) <> ": "
          text e

combinedWithPostalCode :: forall t m. MonadWidget t m => Stripe -> m ()
combinedWithPostalCode stripe = do
  newEvents <- elAttr "div" ("style" =: "max-width: 400px") $ do
    stripeElements <- initStripeElements stripe def
    sce       <- stripeCardElementWithPostalCode stripeElements def
    clicks    <- button "Tokenize"
    responses <- createStripeTokens stripe $ Identity (sce, def) <$ clicks
    blurs     <- getStripeElementOnBlur sce
    focuses   <- getStripeElementOnFocus sce
    readies   <- getStripeElementOnReady sce
    changes   <- getStripeCardElementOnChange sce

    pure $ mconcat
      [ ["clicked"] <$ clicks
      , ["blurred"] <$ blurs
      , ["focused "] <$ focuses
      , ((:[]) . ("change: " <>) . pack . show) <$> changes
      , ["ready"] <$ readies
      , ((:[]) . ("response: " <>) . pack . show . runIdentity) <$> responses
      ]

  timeAndDisplayEvents newEvents

combinedWithoutPostalCode :: forall t m. MonadWidget t m => Stripe -> m ()
combinedWithoutPostalCode stripe = do
  newEvents <- elAttr "div" ("style" =: "max-width: 400px") $ do
    stripeElements <- initStripeElements stripe def
    sce       <- stripeCardElementWithoutPostalCode stripeElements def
    clicks    <- button "Tokenize"
    responses <- createStripeTokens stripe $ Identity (sce, def) <$ clicks
    blurs     <- getStripeElementOnBlur sce
    focuses   <- getStripeElementOnFocus sce
    readies   <- getStripeElementOnReady sce
    changes   <- getStripeCardElementOnChange sce

    pure $ mconcat
      [ ["clicked"] <$ clicks
      , ["blurred"] <$ blurs
      , ["focused "] <$ focuses
      , ((:[]) . ("change: " <>) . pack . show) <$> changes
      , ["ready"] <$ readies
      , ((:[]) . ("response: " <>) . pack . show . runIdentity) <$> responses
      ]

  timeAndDisplayEvents newEvents

separated :: forall t m. MonadWidget t m => Stripe -> m ()
separated stripe = do
  stripeElements <- initStripeElements stripe def

  (numberElement, newNumberEvents) <- elAttr "div" ("style" =: "max-width: 400px") $ do
    scne    <- stripeCardNumberElement stripeElements def
    blurs   <- getStripeElementOnBlur scne
    focuses <- getStripeElementOnFocus scne
    readies <- getStripeElementOnReady scne
    changes <- getStripeElementOnChange scne

    pure . (scne,) $ mconcat
      [ ["number blurred"] <$ blurs
      , ["number focused"] <$ focuses
      , ((:[]) . ("number changed: " <>) . pack . show) <$> changes
      , ["number ready"] <$ readies
      ]

  newExpiryEvents <- elAttr "div" ("style" =: "max-width: 400px") $ do
    scee    <- stripeCardExpiryElement stripeElements def
    blurs   <- getStripeElementOnBlur scee
    focuses <- getStripeElementOnFocus scee
    readies <- getStripeElementOnReady scee
    changes <- getStripeElementOnChange scee

    pure $ mconcat
      [ ["expiry blurred"] <$ blurs
      , ["expiry focused"] <$ focuses
      , ((:[]) . ("expiry changed: " <>) . pack . show) <$> changes
      , ["expiry ready"] <$ readies
      ]

  newCvcEvents <- elAttr "div" ("style" =: "max-width: 400px") $ do
    scce    <- stripeCardCvcElement stripeElements def
    blurs   <- getStripeElementOnBlur scce
    focuses <- getStripeElementOnFocus scce
    readies <- getStripeElementOnReady scce
    changes <- getStripeElementOnChange scce

    pure $ mconcat
      [ ["cvc blurred"] <$ blurs
      , ["cvc focused"] <$ focuses
      , ((:[]) . ("cvc changed: " <>) . pack . show) <$> changes
      , ["cvc ready"] <$ readies
      ]

  newPostalCodeEvents <- elAttr "div" ("style" =: "max-width: 400px") $ do
    spce    <- stripePostalCodeElement stripeElements def
    blurs   <- getStripeElementOnBlur spce
    focuses <- getStripeElementOnFocus spce
    readies <- getStripeElementOnReady spce
    changes <- getStripePostalCodeElementOnChange spce

    pure $ mconcat
      [ ["postal code blurred"] <$ blurs
      , ["postal code focused"] <$ focuses
      , ((:[]) . ("postal code changed: " <>) . pack . show) <$> changes
      , ["postal code ready"] <$ readies
      ]

  clicks <- button "Tokenize"
  responses <- createStripeTokens stripe $ Identity (numberElement, def) <$ clicks
  timeAndDisplayEvents $ mconcat
    [ newNumberEvents
    , newExpiryEvents
    , newCvcEvents
    , newPostalCodeEvents
    , ["clicked"] <$ clicks
    , ((:[]) . ("response: " <>) . pack . show . runIdentity) <$> responses
    ]
