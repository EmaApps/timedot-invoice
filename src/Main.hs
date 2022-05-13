{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Map.Syntax ((##))
import Data.Some (Some (Some))
import Ema
import Ema.CLI qualified
import Generics.SOP qualified as SOP
import Heist qualified as H
import Heist.Interpreted qualified as HI
import Main.Utf8 (withUtf8)
import Options.Applicative
import System.FilePath (splitFileName, (</>))
import System.FilePattern (FilePattern)
import System.UnionMount qualified as UM
import TI.HLedger qualified as HLedger
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
  , modelHours :: HLedger.TimedotEntries
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

filePatterns :: CLI -> [(FileType, FilePattern)]
filePatterns CLI {cliTimedotFile} =
  [ (FileType_Timedot, cliTimedotFile)
  , (FileType_Tpl, cliTimedotFile <> ".tpl")
  , (FileType_Yaml, cliTimedotFile <> ".tpl.yaml")
  ]

instance EmaSite Route where
  type SiteArg Route = CLI
  siteInput _ _ input@CLI {..} = do
    model0 <- liftIO emptyModel
    fmap Dynamic $
      UM.mount cliBaseDir (filePatterns input) [".*"] model0 $
        \tag fp act -> case tag of
          FileType_Timedot -> case act of
            UM.Refresh _ () -> do
              (errs, hrs) <- liftIO $ HLedger.parseTimedot $ cliBaseDir </> fp
              pure $ \m -> m {modelHours = hrs, modelErrors = errs}
            UM.Delete -> do
              putStrLn $ "WARNING: file gone: " <> fp
              pure $ \m -> m {modelHours = mempty, modelErrors = ["No timedot file available"]}
          FileType_Tpl -> case act of
            UM.Refresh _ () -> do
              s <- readFileBS $ cliBaseDir </> fp
              let tmplSt = H.addTemplateFile fp fp s def
              pure $ \m -> m {modelTemplateState = tmplSt}
            UM.Delete -> do
              putStrLn $ "WARNING: file gone: " <> fp
              pure $ \m -> m {modelTemplateState = H.removeTemplateFile fp (modelTemplateState m)}
          FileType_Yaml -> do
            liftIO $ putStrLn $ "TODO: YAML handling " <> fp
            pure id
  siteOutput _ m Route_Index =
    Ema.AssetGenerated Ema.Html . renderTpl (modelTemplateState m) $ do
      "invoice:errors" ## H.listSplice (modelErrors m) "error" $ \err -> "error:err" ## HI.textSplice err
      "invoice:hours" ## H.listSplice (modelHours m) "hour" $ \(day, clients) -> do
        "hour:day" ## HI.textSplice (show day)
        "hour:clients" ## H.listSplice (Map.toList clients) "client" $ \(client, hours) -> do
          "client:name" ## HI.textSplice (show client)
          "client:hours" ## HI.textSplice (show hours)
    where
      renderTpl :: H.TemplateState -> H.Splices (HI.Splice Identity) -> LByteString
      renderTpl tmplSt args =
        -- TODO: don't hardcode template name
        either errorHtml id $ H.renderHeistTemplate "hours.timedot" args tmplSt
        where
          errorHtml err = "<span style=\"text-color: red\">" <> encodeUtf8 err <> "</span>"

main :: IO ()
main = withUtf8 $ do
  cli <- parseCli
  putStrLn $ "Running with args: " <> show cli
  runEmaLiveServer (Proxy @Route) (cliHost cli) (cliPort cli) cli
  where
    -- Like `runSiteWithCli` but only runs the live server, using given host
    -- and port.
    runEmaLiveServer (Proxy :: Proxy r) host port cli =
      let emaCli = Ema.CLI.Cli (Some $ Ema.CLI.Run (host, port)) False
       in void $ runSiteWithCli @r emaCli cli

data CLI = CLI
  { -- | The base directory containing the timedot file. Ema will scan this.
    cliBaseDir :: FilePath
  , -- | Filename of the timedot file under the base directory
    cliTimedotFile :: FilePath
  , -- | Live server host and port
    cliHost :: Ema.CLI.Host
  , cliPort :: Ema.CLI.Port
  }
  deriving stock (Eq, Show)

parseCli :: IO CLI
parseCli =
  execParser cliParserInfo
  where
    -- TODO: Add more CLI params
    -- - Duration (default: 2 weeks)
    cliParser :: Parser CLI
    cliParser = do
      cliHost <- Ema.CLI.hostParser
      cliPort <- Ema.CLI.portParser
      (cliBaseDir, cliTimedotFile) <-
        (fst &&& snd) . splitFileName
          <$> argument str (metavar "TIMEDOT_FILE" <> value "./hours.timedot")
      pure $ CLI {..}

    cliParserInfo :: ParserInfo CLI
    cliParserInfo =
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
