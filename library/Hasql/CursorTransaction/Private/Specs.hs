module Hasql.CursorTransaction.Private.Specs
where

import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.Encoders as A
import qualified Hasql.Decoders as B
import qualified Control.Foldl as D


-- |
-- A specification of a streaming query.
-- 
-- Provides an abstraction over Postgres Cursor,
-- which allows to process result sets of any size in constant memory.
-- 
-- Essentially it is a parametric query specification extended with a reduction strategy and a batch size,
-- where reduction strategy determines how to fold the rows into the final result,
-- and batch size determines how many rows to fetch during each roundtrip to the database.
data CursorQuery params result =
  CursorQuery !ByteString !(A.Params params) !(ReducingDecoder result) !BatchSize

instance Profunctor CursorQuery where
  dimap fn1 fn2 (CursorQuery template encoder decoder batchSize) =
    CursorQuery template (contramap fn1 encoder) (fmap fn2 decoder) batchSize

instance Functor (CursorQuery params) where
  fmap =
    rmap


-- |
-- A specification of how to decode and reduce multiple rows.
data ReducingDecoder reduction =
  forall row. ReducingDecoder !(B.Row row) !(D.Fold row reduction)

instance Functor ReducingDecoder where
  fmap fn (ReducingDecoder rowDecoder rowsFold) =
    ReducingDecoder rowDecoder (fmap fn rowsFold)

instance Applicative ReducingDecoder where
  pure reduction =
    ReducingDecoder (pure ()) (pure reduction)
  (<*>) (ReducingDecoder rowDecoder1 rowsFold1) (ReducingDecoder rowDecoder2 rowsFold2) =
    ReducingDecoder rowDecoder3 rowsFold3
    where
      rowDecoder3 =
        strictPair <$> rowDecoder1 <*> rowDecoder2
        where
          strictPair !a !b =
            (a, b)
      rowsFold3 =
        lmap fst rowsFold1 <*> lmap snd rowsFold2


-- |
-- Spefifies how many rows to fetch in a single DB rountrip.
data BatchSize =
  BatchSize_10 | BatchSize_100 | BatchSize_1000 | BatchSize_10000
  deriving (Enum, Bounded)


-- |
-- A parameters encoder immediately supplied with parameters.
newtype EncodedParams =
  EncodedParams (Supplied A.Params)
  deriving (Monoid)

-- |
-- Pack the params encoder and params into EncodedParams.
encodedParams :: A.Params params -> params -> EncodedParams
encodedParams encoder params =
  EncodedParams (Supplied encoder params)
