{-# LANGUAGE DeriveAnyClass #-}

module Site.Amb (Route) where

import Ema
import Generics.SOP qualified as SOP
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Route_Index
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo, HasModel, IsRoute)

instance CanRender Route where
  routeAsset enc m r =
    Ema.AssetGenerated Ema.Html . RU.renderHtml $ do
      H.docType
      H.html ! A.lang "en" $ do
        H.head $ do
          H.meta ! A.charset "UTF-8"
          -- This makes the site mobile friendly by default.
          H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
          H.link
            ! A.href "https://unpkg.com/tailwindcss@2/dist/tailwind.min.css"
            ! A.rel "stylesheet"
            ! A.type_ "text/css"
          H.title "Amb"
          H.base ! A.href "/"
        H.body $ do
          H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
            H.h1 ! A.class_ "text-3xl font-bold" $ "Basic site"
            case r of
              Route_Index -> do
                "You are on the index page. "
    where
      routeElem r' = do
        H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r'
      routeHref r' =
        A.href (fromString . toString $ Ema.routeUrl enc m r')
