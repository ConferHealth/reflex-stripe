module Reflex.Stripe.Utils where

import Control.Lens ((^.))
import Data.Functor (void)
import Data.Text (Text)
import Language.Javascript.JSaddle (ToJSVal, JSM, jsg, js1)

consoleLog :: ToJSVal arg => arg -> JSM ()
consoleLog arg = void $ jsg ("console" :: Text) ^. js1 ("log" :: Text) arg
