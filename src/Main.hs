{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Some
import Ema
import Ema.CLI qualified
import Generics.SOP qualified as SOP
import Main.Utf8 (withUtf8)
import Options.Applicative
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route = Route_Index
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo, HasModel, IsRoute)

instance CanRender Route where
  routeAsset enc m Route_Index =
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
            H.h1 ! A.class_ "text-3xl font-bold" $ "timedot-invoice"
    where
      _routeElem r' = do
        H.a ! A.class_ "text-red-500 hover:underline" ! _routeHref r'
      _routeHref r' =
        A.href (fromString . toString $ Ema.routeUrl enc m r')

main :: IO ()
main = do
  withUtf8 $ do
    timedotFile <- execParser parseCli
    void $ runSiteWithCli @Route emaCli ()
  where
    -- TODO: Allow setting port
    emaCli = Ema.CLI.Cli (Some $ Ema.CLI.Run ("127.0.0.1", 9092)) False

cliParser :: Parser FilePath
cliParser = do
  argument str (metavar "TIMEDOT_FILE" <> value "./hours.timedot")

parseCli :: ParserInfo FilePath
parseCli =
  info
    (versionOption <*> cliParser <**> helper)
    ( fullDesc
        <> progDesc "timedot-invoice: TODO"
        <> header "timedot-invoice"
    )
  where
    versionOption =
      infoOption
        "0.1"
        (long "version" <> help "Show version")
