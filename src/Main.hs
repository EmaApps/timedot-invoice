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
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.UnionMount qualified as UM
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
  { modelTemplateState :: H.TemplateState
  , modelHours :: TimedotEntries
  , modelErrors :: [Text]
  }

emptyModel :: IO Model
emptyModel = do
  ts <- H.emptyTemplateState
  pure $ Model ts mempty mempty

data FileType
  = FileType_Timedot
  | FileType_Tpl
  | FileType_Yaml
  deriving stock (Eq, Show, Ord)

instance HasModel Route where
  type ModelInput Route = FilePath
  modelDynamic _ _ timedotFile = do
    model0 <- liftIO emptyModel
    baseDir <- liftIO $ makeAbsolute $ takeDirectory timedotFile
    let tplFile = timedotFile <> ".tpl"
        pats = [(FileType_Timedot, takeFileName timedotFile), (FileType_Tpl, takeFileName tplFile)]
    fmap Dynamic $
      UM.mount baseDir pats [".*"] model0 $
        \tag fp act -> case tag of
          FileType_Timedot -> case act of
            UM.Refresh _ () -> do
              (errs, hrs) <- liftIO $ parseTimedot $ baseDir </> fp
              pure $ \m -> m {modelHours = hrs, modelErrors = errs}
            UM.Delete -> do
              putStrLn $ "WARNING: file gone: " <> fp
              pure $ \m -> m {modelHours = mempty, modelErrors = ["No timedot file available"]}
          FileType_Tpl -> case act of
            UM.Refresh _ () -> do
              s <- readFileBS $ baseDir </> fp
              let tmplSt = H.addTemplateFile fp fp s def
              pure $ \m -> m {modelTemplateState = tmplSt}
            UM.Delete -> do
              putStrLn $ "WARNING: file gone: " <> fp
              pure $ \m -> m {modelTemplateState = H.removeTemplateFile fp (modelTemplateState m)}
          FileType_Yaml -> do
            liftIO $ putStrLn "Not implemented"
            pure id

instance CanRender Route where
  routeAsset _ m Route_Index =
    Ema.AssetGenerated Ema.Html . renderTpl m $ do
      "invoice:errors" ## H.listSplice (modelErrors m) "error" $ \err -> do
        "error:err" ## HI.textSplice err
      "invoice:hours" ## H.listSplice (modelHours m) "hour" $ \(day, clients) -> do
        "hour:day" ## HI.textSplice (show day)
        "hour:clients" ## H.listSplice (Map.toList clients) "client" $ \(client, hours) -> do
          "client:name" ## HI.textSplice (show client)
          "client:hours" ## HI.textSplice (show hours)
    where
      renderTpl :: Model -> H.Splices (HI.Splice Identity) -> LByteString
      renderTpl (Model tmplSt _ _) args =
        -- TODO: don't hardcode template name
        either errorHtml id $ H.renderHeistTemplate "hours.timedot" args tmplSt
        where
          errorHtml err = "<span style=\"text-color: red\">" <> encodeUtf8 err <> "</span>"

main :: IO ()
main = do
  withUtf8 $ do
    timedotFile <- execParser parseCli
    putStrLn $ "Running on timedot file: " <> timedotFile
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
