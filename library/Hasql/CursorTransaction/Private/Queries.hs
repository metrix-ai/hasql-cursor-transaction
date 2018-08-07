module Hasql.CursorTransaction.Private.Queries where

import           Hasql.CursorTransaction.Private.Prelude
import qualified Hasql.Statement               as A
import qualified Hasql.Encoders                as B
import qualified Hasql.Decoders                as C
import qualified Hasql.CursorTransaction.Private.Specs
                                               as F
import qualified ByteString.TreeBuilder        as D


declareCursor :: ByteString -> ByteString -> B.Params a -> A.Statement a ()
declareCursor name sql encoder = A.Statement sql' encoder C.unit False
 where
  sql' =
    D.toByteString
      $  "DECLARE "
      <> D.byteString name
      <> " NO SCROLL CURSOR FOR "
      <> D.byteString sql

closeCursor :: ByteString -> A.Statement () ()
closeCursor name = A.Statement sql B.unit C.unit True
  where sql = "CLOSE " <> name

fetchFromCursor
  :: ByteString -> F.BatchSize -> C.Result result -> A.Statement () result
fetchFromCursor name (F.BatchSize batchSize) decoder = A.Statement sql
                                                                   encoder
                                                                   decoder
                                                                   True
 where
  sql =
    D.toByteString
      $  "FETCH FORWARD "
      <> D.asciiIntegral batchSize
      <> " FROM "
      <> D.byteString name
  encoder = B.unit
