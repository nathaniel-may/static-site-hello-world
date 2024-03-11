module GenerateInitialHTML where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Element (outerHTML)
import Main as Main
import Web.HTML.HTMLElement (toElement)

-- | prints a string representation of the initial html so it can be written to a file to be served.
main :: Effect Unit
main = launchAff_ $ log =<< initialHTML

initialHTML :: Aff String
initialHTML = do
  _ <- liftEffect Main.main
  html <- Main.selectElement_ "html"
  liftEffect <<< outerHTML $ toElement html
