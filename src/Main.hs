{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Aeson.Types qualified as Aeson
import Data.Default (def)
import Data.Map.Syntax ((##))
import Data.Yaml qualified as Yaml
import Ema (
  Asset (AssetGenerated),
  Dynamic (Dynamic),
  EmaSite (..),
  Format (Html),
  IsRoute,
  SingleModelRoute (SingleModelRoute),
 )
import Ema qualified
import Ema.CLI qualified
import Generics.SOP qualified as SOP
import Heist qualified as H
import Heist.Interpreted qualified as HI
import Heist.Splices.Apply qualified as HA
import Heist.Splices.Bind qualified as HB
import Heist.Splices.Json qualified as HJ
import Main.Utf8 (withUtf8)
import Options.Applicative
import System.FilePath (splitFileName, (</>))
import System.FilePattern (FilePattern)
import System.UnionMount qualified as UM
import TI.HLedger qualified as HLedger
import TI.Heist qualified as H
import TI.Matrix qualified as M

{- | The "index.html" route.

 Route types are the lynchpin of Ema apps, thus we need this sophisticated type
 even if our site will generate a single HTML file.
-}
data Route = Route_Index
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (IsRoute)
    via (SingleModelRoute Model Route)

-- | All the data required to generate our "index.html" file
data Model = Model
  { modelTemplateState :: H.TemplateState
  , modelVars :: Aeson.Value
  , modelHours :: HLedger.TimedotEntries
  , modelErrors :: [Text]
  }

-- | The empty model corresponding to the "index.html" that has no data (yet).
emptyModel :: IO Model
emptyModel = do
  ts <- H.emptyTemplateState
  pure $ Model ts Aeson.Null mempty mempty

-- | The type of files we want to watch (for changes) on disk.
data FileType
  = FileType_Timedot
  | FileType_Tpl
  | FileType_Yaml
  deriving stock (Eq, Show, Ord)

{- | Given a "foo.timedot" what kind of files do we want to observe
for changes on?
-}
filePatternsToWatch :: FilePath -> [(FileType, FilePattern)]
filePatternsToWatch timedotFile =
  [ (FileType_Timedot, timedotFile)
  , (FileType_Tpl, timedotFile <> ".tpl")
  , (FileType_Yaml, timedotFile <> ".tpl.yaml")
  ]

{- Define the Ema site

  - `siteInput` defines the "input" data required to generate the site. This data
  is a Ema `Dynamic`, as in - it is time-varying.
  - `siteOutput` defines the "output" data, which in our case is a HTML string
  to be written to "index.html" route.
  - `SiteArg` (which is `Arg`) is what the `siteInput` is parametrized over. Its
  value is passed over to the `runSite` function which is called from `main`.

-}
instance EmaSite Route where
  type SiteArg Route = Arg
  siteInput _ _ Arg {..} = do
    model0 <- liftIO emptyModel
    -- We use the unionmount library to map the filesystem to in-memory `Model`,
    -- and update it over file as the files change. This abstraction is wrapped
    -- in a `Dynamic`.
    fmap Dynamic $
      UM.mount argBaseDir (filePatternsToWatch argTimedotFile) [".*"] model0 $
        \tag fp act -> case tag of
          FileType_Timedot -> case act of
            UM.Refresh _ () -> do
              (errs, hrs) <- liftIO $ HLedger.parseTimedot $ argBaseDir </> fp
              pure $ \m -> m {modelHours = hrs, modelErrors = errs}
            UM.Delete -> do
              putStrLn $ "WARNING: file gone: " <> fp
              pure $ \m -> m {modelHours = mempty, modelErrors = ["No timedot file available"]}
          FileType_Tpl -> case act of
            UM.Refresh _ () -> do
              s <- readFileBS $ argBaseDir </> fp
              let tmplSt = H.addTemplateFile fp fp s def
              pure $ \m -> m {modelTemplateState = tmplSt}
            UM.Delete -> do
              putStrLn $ "WARNING: file gone: " <> fp
              pure $ \m -> m {modelTemplateState = H.removeTemplateFile fp (modelTemplateState m)}
          FileType_Yaml -> case act of
            UM.Refresh _ () -> do
              liftIO (Yaml.decodeFileEither $ argBaseDir </> fp) >>= \case
                Left err -> pure $ \m -> m {modelErrors = ["YAML error: " <> show err]}
                Right v -> do
                  print v
                  pure $ \m -> m {modelVars = v}
            UM.Delete -> do
              putStrLn $ "WARNING: YAML file gone: " <> fp
              pure $ \m -> m {modelVars = Aeson.Null}
  siteOutput _ m Route_Index =
    -- We use Heist templates for HTML. Here we define the variables required to
    -- render that template. cf. https://srid.ca/heist-start
    Ema.AssetGenerated Ema.Html . renderTpl (modelTemplateState m) $ do
      -- Heist helpers
      "bind" ## HB.bindImpl
      "apply" ## HA.applyImpl
      -- App specific vars
      "invoice:metadata" ## HJ.bindJson (modelVars m)
      "invoice:errors" ## H.listSplice (modelErrors m) "error" $ \err -> "error:err" ## HI.textSplice err
      let matrix = M.matrixFromMap $ modelHours m
      "invoice:clients" ## H.listSplice (M.matrixCols matrix) "invoice:each-client" $ \client ->
        "invoice:client" ## HI.textSplice (toText . toString $ client)
      "invoice:matrix" ## M.matrixSplice matrix
    where
      renderTpl :: H.TemplateState -> H.Splices (HI.Splice Identity) -> LByteString
      renderTpl tmplSt args =
        -- TODO: don't hardcode template name
        either errorHtml id $ H.renderHeistTemplate "hours.timedot" args tmplSt
        where
          errorHtml err = "<span style=\"text-color: red\">" <> encodeUtf8 err <> "</span>"

main :: IO ()
main = withUtf8 $ do
  (siteArg, port) <- parseCli
  putStrLn $ "Running with args: " <> show siteArg
  Ema.runSiteLiveServerOnly @Route "127.0.0.1" port siteArg

-- | Argument to function that produces the Ema site input.
data Arg = Arg
  { -- | The base directory containing the timedot file. It will be scanned.
    argBaseDir :: FilePath
  , -- | Filename of the timedot file under the base directory
    argTimedotFile :: FilePath
  }
  deriving stock (Eq, Show)

parseCli :: IO (Arg, Ema.CLI.Port)
parseCli =
  execParser $ parserInfo cliParser
  where
    -- TODO: Add more Arg params
    -- - Duration (default: 2 weeks)
    cliParser :: Parser (Arg, Ema.CLI.Port)
    cliParser = do
      cliPort <- Ema.CLI.portParser
      (argBaseDir, argTimedotFile) <-
        (fst &&& snd) . splitFileName
          <$> argument str (metavar "TIMEDOT_FILE" <> value "./hours.timedot")
      pure (Arg {..}, cliPort)

    parserInfo :: Parser a -> ParserInfo a
    parserInfo p =
      info
        (versionOption <*> p <**> helper)
        ( fullDesc
            <> progDesc "timedot-invoice: TODO"
            <> header "timedot-invoice"
        )
      where
        versionOption =
          infoOption
            "0.1"
            (long "version" <> help "Show version")
