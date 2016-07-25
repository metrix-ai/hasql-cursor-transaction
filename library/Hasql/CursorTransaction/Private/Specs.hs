module Hasql.CursorTransaction.Private.Specs
where

import Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.Encoders as A
import qualified Hasql.Decoders as B
import qualified Control.Foldl as D


-- |
-- Spefifies how many rows to fetch in a single DB rountrip.
newtype BatchSize =
  BatchSize Int64

batchSize_10 :: BatchSize
batchSize_10 =
  BatchSize 10

batchSize_100 :: BatchSize
batchSize_100 =
  BatchSize 100

batchSize_1000 :: BatchSize
batchSize_1000 =
  BatchSize 1000

batchSize_10000 :: BatchSize
batchSize_10000 =
  BatchSize 10000


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
