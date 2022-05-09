{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Default (def)
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
  }

renderTpl :: Model -> H.Splices (HI.Splice Identity) -> LByteString
renderTpl (Model _ tmplSt _) args =
  -- TODO: don't hardcode template name
  either error id $ H.renderHeistTemplate "./example/hours.timedot" args tmplSt

instance HasModel Route where
  type ModelInput Route = FilePath
  modelDynamic _ _ fp = do
    let tmplFile = fp <> ".tpl"
    tmplContents <- readFileBS tmplFile
    let tmplSt = H.addTemplateFile tmplFile tmplFile tmplContents def
    hrs <- liftIO $ parseTimedot fp
    -- TODO: Dynamic
    pure $ pure $ Model fp tmplSt hrs

instance CanRender Route where
  routeAsset _ m Route_Index =
    Ema.AssetGenerated Ema.Html $
      renderTpl m $ do
        "invoice:hours" ## HI.textSplice (show $ modelHours m)

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
