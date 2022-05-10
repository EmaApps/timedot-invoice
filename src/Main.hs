{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Map.Syntax
import Data.Some (Some (Some))
import Ema
import Ema.CLI qualified
import Generics.SOP qualified as SOP
import Heist qualified as H
import Heist.Interpreted qualified as HI
import Main.Utf8 (withUtf8)
import Options.Applicative
import TI.HLedger
import TI.Heist qualified as H

data Route = Route_Index
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (IsRoute)
    via (SingleModelRoute Model Route)

data Model = Model
  { modelTimedotFile :: FilePath
  , modelTemplateState :: H.TemplateState
  , modelHours :: TimedotEntries
  , modelErrors :: [Text]
  }

renderTpl :: Model -> H.Splices (HI.Splice Identity) -> LByteString
renderTpl (Model _ tmplSt _ _) args =
  -- TODO: don't hardcode template name
  either errorHtml id $ H.renderHeistTemplate "./example/hours.timedot" args tmplSt
  where
    errorHtml err = "<span style=\"text-color: red\">" <> encodeUtf8 err <> "</span>"

instance HasModel Route where
  type ModelInput Route = FilePath
  modelDynamic _ _ fp = do
    let tmplFile = fp <> ".tpl"
    tmplContents <- readFileBS tmplFile
    let tmplSt = H.addTemplateFile tmplFile tmplFile tmplContents def
    (errs, hrs) <- liftIO $ parseTimedot fp
    -- TODO: Dynamic
    pure $ pure $ Model fp tmplSt hrs errs

instance CanRender Route where
  routeAsset _ m Route_Index =
    Ema.AssetGenerated Ema.Html $
      renderTpl m $ do
        "invoice:errors" ## H.listSplice (modelErrors m) "error" $ \err -> do
          "error:err" ## HI.textSplice err
        "invoice:hours" ## H.listSplice (modelHours m) "hour" $ \(day, clients) -> do
          "hour:day" ## HI.textSplice (show day)
          "hour:clients" ## H.listSplice (Map.toList clients) "client" $ \(client, hours) -> do
            "client:name" ## HI.textSplice (show client)
            "client:hours" ## HI.textSplice (show hours)

main :: IO ()
main = do
  withUtf8 $ do
    timedotFile <- execParser parseCli
    print timedotFile
    void $ runSiteWithCli @Route emaCli timedotFile
  where
    -- TODO: Allow setting port
    emaCli = Ema.CLI.Cli (Some $ Ema.CLI.Run ("127.0.0.1", 9092)) False

-- TODO: Add more CLI params
-- - Duration (default: 2 weeks)
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
