{-# HLINT ignore "Use ?~" #-}
module TI.HLedger where

import Hledger qualified
import Hledger.Data.Types qualified as HL
import Hledger.Read.TimedotReader qualified as TimedotReader
import Relude.Extra.Lens ((.~))

-- | Like `readJournal'` but for timedot files
readTimedot :: Text -> IO HL.Journal
readTimedot t = Hledger.readJournal opts Nothing t >>= either Hledger.error' return
  where
    sFormat = Hledger.rFormat (TimedotReader.reader @IO)
    opts =
      Hledger.definputopts
        & Hledger.mformat .~ Just sFormat

newtype Client = Client {unClient :: Text}
  deriving newtype (Show, Eq, Ord)

newtype Hours = Hours {unHours :: Word}
  deriving newtype (Show, Eq, Ord, Num)

-- Available in latest version of `time` which we don't have
type WeekOfYear = Int

extractHours :: HL.Journal -> Map WeekOfYear (Map Client Hours)
extractHours = mempty
