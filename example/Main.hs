module Main where

import Control.Monad ((<=<))
import Control.Monad.Trans (liftIO)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Functor ((<$), void)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Text (Text, pack)
import Reflex.Dom
  ( (=:), Dynamic, Event, MonadWidget, mainWidget, button, current, dyn, el, elAttr, ffor, foldDyn, hold, holdDyn, never, performEvent, switch, tag
  , text, _textInput_value, textInput )
import Reflex.Stripe

main :: IO ()
main = mainWidget app

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
  stripeElements <- initStripeElements stripe def
  (clicks, blurs, focuses, changes, readies, responses) <- elAttr "div" ("style" =: "max-width: 400px") $ do
    sce <- stripeCardElementWithPostalCode stripeElements def
    cls <- button "Tokenize"
    resps <- createStripeTokens stripe sce def cls
    bs <- getStripeElementOnBlur sce
    fs <- getStripeElementOnFocus sce
    rs <- getStripeElementOnReady sce
    chs <- getStripeCardElementOnChange sce
    pure (cls, bs, fs, chs, rs, resps)

  let newEvents :: Event t [Text]
      newEvents = mconcat
        [ ["clicked"] <$ clicks
        , ["blurred"] <$ blurs
        , ["focused "] <$ focuses
        , ((:[]) . ("change: " <>) . pack . show) <$> changes
        , ["ready"] <$ readies
        , ((:[]) . ("response: " <>) . pack . show) <$> responses
        ]

  timedEvents :: Event t (UTCTime, [Text]) <- performEvent $ ffor newEvents $ \ a -> (,a) <$> liftIO getCurrentTime

  events :: Dynamic t (Seq (UTCTime, [Text])) <- foldDyn (flip (|>)) Seq.empty timedEvents

  void . dyn $ ffor events $ \ evs ->
    for_ evs $ \ (t, es) ->
      for_ es $ \ e -> 
        el "div" $ do
          el "b" . text $ (pack . show $ t) <> ": "
          text e
