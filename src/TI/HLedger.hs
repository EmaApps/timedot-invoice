{-# HLINT ignore "Use ?~" #-}
{-# LANGUAGE RecordWildCards #-}

module TI.HLedger (TimedotEntries, parseTimedot) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar
import Hledger qualified
import Hledger.Data.Types qualified as HL
import Hledger.Read.TimedotReader qualified as TimedotReader
import Relude.Extra.Lens ((.~))

type TimedotEntries = [(Day, Map Client Hours)]

parseTimedot :: FilePath -> IO TimedotEntries
parseTimedot fp = do
  s <- readFileText fp
  jrnl <- readTimedot s
  pure $ parseTransaction <$> HL.jtxns jrnl

-- | Like `readJournal'` but for timedot files
readTimedot :: Text -> IO HL.Journal
readTimedot t = Hledger.readJournal opts Nothing t >>= either Hledger.error' return
  where
    sFormat = Hledger.rFormat (TimedotReader.reader @IO)
    opts =
      Hledger.definputopts
        & Hledger.mformat .~ Just sFormat

newtype Client = Client {unClient :: Text}
  deriving newtype (Show, Eq, Ord, IsString)

newtype Hours = Hours {unHours :: Integer}
  deriving newtype (Show, Eq, Ord, Num)

-- Available in latest version of `time` which we don't have
type WeekOfYear = Int

parseTransaction :: HasCallStack => HL.Transaction -> (Day, Map Client Hours)
parseTransaction (HL.Transaction {..}) =
  ( tdate
  , Map.fromListWith (\_ _ -> error "dups") $
      tpostings <&> \(HL.Posting {..}) ->
        (fromString . toString $ paccount, parseHours pamount)
  )
  where
    parseHours :: HL.MixedAmount -> Hours
    parseHours amt = fromMaybe (error "bad hours") $ do
      HL.Mixed (toList -> [HL.Amount {..}]) <- pure amt
      let r :: Rational = toRational aquantity
      guard $ denominator r == 1
      guard $ numerator r > 0
      pure $ Hours $ numerator r
