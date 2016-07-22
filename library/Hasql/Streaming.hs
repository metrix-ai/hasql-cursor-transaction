module Hasql.Streaming
(
  StreamingQuery(..),
  F.BatchSize(..),
  run,
)
where

import Hasql.Streaming.Prelude
import qualified Hasql.Encoders as A
import qualified Hasql.Decoders as B
import qualified Hasql.Transaction as E
import qualified Hasql.Streaming.Queries as C
import qualified Hasql.Streaming.Model as F
import qualified Control.Foldl as D


-- |
-- A specification of a streaming query.
-- 
-- Essentially it is a parametric query extended with a reduction strategy and a batch size.
-- Where reduction strategy determines how to fold the results and when to terminate,
-- and batch size determines how many rows to fetch during each roundtrip to the database.
data StreamingQuery input output =
  forall row. 
  StreamingQuery {
    template :: !ByteString,
    paramsEncoder :: !(A.Params input),
    rowDecoder :: !(B.Row row),
    rowsFold :: !(D.Fold row output),
    batchSize :: !F.BatchSize
  }

run :: StreamingQuery input output -> input -> E.Transaction output
run StreamingQuery{..} input =
  declareCursor *> fetchFromCursor <* closeCursor
  where
    cursorName =
      "Hasql.Streaming"
    declareCursor =
      E.query input query
      where
        query =
          C.declareCursor cursorName template paramsEncoder
    closeCursor =
      E.query cursorName C.closeCursor
    fetchFromCursor =
      E.query (batchSize, cursorName) (C.fetchFromCursor_fold rowsFold rowDecoder)
