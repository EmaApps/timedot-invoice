module TI.Matrix where

import Data.Map.Strict qualified as Map
import Data.Map.Syntax ((##))
import Heist.Interpreted qualified as HI
import TI.Heist qualified as H

-- | A (partial) matrix with not all cells necessarily filled.
data Matrix r c a = Matrix
  { matrixRows :: [r]
  , matrixCols :: [c]
  , matrixCells :: [(r, [(c, Maybe a)])]
  }
  deriving stock (Eq, Show)

matrixFromMap :: (Ord r, Ord c) => Map r (Map c a) -> Matrix r c a
matrixFromMap m =
  let cols = ordNub $ sort $ foldMap Map.keys $ Map.elems m
      rows = sort $ Map.keys m
      cells =
        Map.toAscList m <&> \(r, colMap) ->
          (r,) $ cols <&> \c -> (c, Map.lookup c colMap)
   in Matrix rows cols cells

matrixSplice :: (Show r, Show c, Show a) => Matrix r c a -> HI.Splice Identity
matrixSplice m =
  H.listSplice (matrixCells m) "matrix:each-row" $ \(row, cols) -> do
    "matrix:row" ## HI.textSplice (show row)
    "matrix:cols" ## H.listSplice cols "matrix:each-column" $ \(col, mval) -> do
      "matrix:col" ## HI.textSplice (show col)
      -- TODO: handle maybe via template
      "matrix:cell" ## HI.textSplice (show mval)
