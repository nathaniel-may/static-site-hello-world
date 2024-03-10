module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Foldable (traverse_)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Element (replaceChildren)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import TW.TW (twMerge)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (HTMLElement, toElement)

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  head <- selectElement_ "head"
  body <- selectElement_ "body"
  liftEffect $ replaceChildren (toElement head) []
  liftEffect $ replaceChildren (toElement body) []
  traverse_ (\elem -> runUI (headComponent elem) unit head) headContents
  runUI bodyComponent unit body

-- nodeMain :: Effect Unit
-- nodeMain = ???

bodyComponent ∷ ∀ q i o m. MonadAff m => H.Component q i o m
bodyComponent =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = pure }
    }
  where
  render :: ∀ state action slots. state -> H.ComponentHTML action slots m
  render _ =
    HH.div
      [ css "h-full leading-relaxed font-light text-lg text-warm-gray" ]
      [ HH.div
        [ css "flex justify-center items-center" ]
        [ HH.h1
          [ css "font-40 font-bold" ]
          [ HH.text "Hello World" ]
        ]
      ]

headComponent :: ∀ a q i o m. HH.HTML (H.ComponentSlot a m Unit) Unit -> H.Component q i o m
headComponent elem =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = pure }
    }
  where
  initialState _ = elem

  render state = state
  
headContents = [ HH.title_ [ HH.text "Hello World" ] ]

css :: ∀ r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName

cssMerge :: ∀ r i. Array String -> HH.IProp (class :: String | r) i
cssMerge = css <<< twMerge

-- | Finds the element and throws if it cannot be found. Call after the document has loaded.
selectElement_ :: String -> Aff HTMLElement
selectElement_ name = do
    elem <- HA.selectElement (QuerySelector name)
    maybe (throwError <<< error $ "No elements matched the query selector `" <> name <> "`") pure elem
