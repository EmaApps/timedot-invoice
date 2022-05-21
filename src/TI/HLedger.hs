{-# LANGUAGE RecordWildCards #-}

module TI.HLedger (TimedotEntries, parseTimedot) where

import Control.Lens.Operators ((?~))
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Traversable (for)
import Hledger qualified
import Hledger.Data.Types qualified as HL
import Hledger.Read.TimedotReader qualified as TimedotReader
import Relude.Extra (groupBy)
import Text.Show qualified as Show

type TimedotEntries = Map (NonEmpty Day) (Map Client Hours)

type Error = Text

parseTimedot :: FilePath -> IO ([Error], TimedotEntries)
parseTimedot fp = do
  s <- readFileText fp
  jrnl <- readTimedot s
  let res = parseTransaction <$> HL.jtxns jrnl
  pure (lefts res, groupByWeek $ Map.fromListWith (<>) $ rights res)

-- | Like `readJournal'` but for timedot files
readTimedot :: Text -> IO HL.Journal
readTimedot t = Hledger.readJournal opts Nothing t >>= either Hledger.error' return
  where
    sFormat = Hledger.rFormat (TimedotReader.reader @IO)
    opts =
      Hledger.definputopts
        & Hledger.mformat ?~ sFormat

newtype Client = Client {unClient :: Text}
  deriving newtype (Show, Eq, Ord, IsString, ToString)

newtype Hours = Hours {unHours :: Integer}
  deriving newtype (Show, Eq, Ord, Num)

-- TODO: Sane error handling (display in HTML)
parseTransaction :: HasCallStack => HL.Transaction -> Either Text (Day, Map Client Hours)
parseTransaction (HL.Transaction {..}) = do
  -- TODO: replace error with throwError
  hours <- fmap (Map.fromListWith (\_ _ -> error "dups")) $
    for tpostings $ \(HL.Posting {..}) -> do
      hs <- parseHours pamount
      pure (fromString . toString $ paccount, hs)
  pure (tdate, hours)
  where
    parseHours :: HL.MixedAmount -> Either Error Hours
    parseHours amt = do
      HL.Mixed (toList -> [HL.Amount {..}]) <- pure amt
      let r :: Rational = toRational aquantity
      if denominator r == 1
        then
          if numerator r > 0
            then pure $ Hours $ numerator r
            else Left "Negative hours"
        else Left "Non-integer hours"

newtype Week = Week (Integer, Int) -- Year, WeekOfYear
  deriving newtype (Eq, Ord)

instance Show Week where
  show (Week (y, w)) = show y ++ ", week: " ++ show w

groupByWeek ::
  forall k a.
  (Ord k, Num a) =>
  Map Day (Map k a) ->
  Map (NonEmpty Day) (Map k a)
groupByWeek es =
  groupBy (getWeek . fst) (Map.toAscList es)
    & Map.toAscList
    & fmap
      ( \(_wk, es') ->
          ( NEL.sort $ fmap fst es'
          , Map.unionsWith (+) $ fmap snd es'
          )
      )
    & Map.fromListWith (Map.unionWith (+))
  where
    getWeek day =
      let (year, week, _) = toWeekDate day
       in Week (year, week)
