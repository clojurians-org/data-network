{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE RankNTypes, RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module DataNetwork.Core.Conduit.PostgreSQL where

import DataNetwork.Core.Types
import DataNetwork.Core.Utils

import Prelude
import Data.Maybe (catMaybes)
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Aeson as J

import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M
import Data.Maybe (isJust, fromMaybe)
import Data.Functor ((<&>))
import Data.String.Conversions (cs)
import Text.Heredoc (str)
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time

import Control.Applicative (liftA2)

import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Resource (MonadResource, allocate, release, runResourceT)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import Control.Exception (finally, catch, SomeException(..))
import Control.Monad (when, void)

import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChanIO, closeTBMChan, writeTBMChan, readTBMChan)
import Data.Conduit.TMChan (sourceTBMChan)

import qualified UnliftIO as U
import Data.Either (isLeft, isRight, fromRight)
import qualified Hasql.Connection as H 
import qualified Hasql.Session as H
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD

import Control.Monad.Except (ExceptT(..), runExceptT)
import Labels ((:=)(..))

import Control.Monad.Trans.Control (MonadTransControl(..), MonadBaseControl(..))
import qualified Control.Exception.Lifted as L
import qualified Control.Concurrent.Lifted as L

withPGChan :: forall m a. (MonadResource m, MonadBaseControl IO m)
  => Credential -> T.Text -> T.Text -> (Int -> m a) -> m (TBMChan [a])
withPGChan (Credential hostName hostPort username password) database sql decode = do
  let setting = H.settings (cs hostName) (fromIntegral hostPort) (cs username) (cs password) (cs database)
      defCursorSize = 2000
      defBatchSize = 20000
      defCurName = "larluo"
  (reg, chan) <- allocate (newTBMChanIO defBatchSize) (U.atomically . closeTBMChan)
  _ <- L.fork $ void . runExceptT $
      L.bracket (ExceptT $ liftIO $ H.acquire setting) (liftIO . H.release) $ \conn -> do
        liftIO $ flip H.run conn $ do
          H.sql "BEGIN"
          H.sql $ "DECLARE " <> defCurName <> " NO SCROLL CURSOR FOR " <> cs sql
          sinkRows undefined conn chan defCurName defCursorSize
          H.sql $ "CLOSE " <> cs defCurName
          H.sql "COMMIT"
          release reg
  return chan
  where
    fetchFromCursor :: B.ByteString -> Int -> HD.Row row -> H.Session [row]
    fetchFromCursor name batchSize decode =
      H.statement () $ HS.Statement sql HE.unit (HD.rowList decode) True
      where sql = "FETCH FORWARD " <> (cs (show batchSize)) <> " FROM " <> name
    
    sinkRows decode conn chan cursorName cursorSize = do
      rows <- fetchFromCursor (cs cursorName) cursorSize decode
      when ((not . null) rows) $ do
        U.atomically $ writeTBMChan chan rows
        sinkRows decode conn chan cursorName cursorSize

{--
instance Oracle.FromDataFields J.Value where
  fromDataFields' = fmap (J.Object . M.fromList . catMaybes) . mapM Oracle.fromDataField

  
pgRawChan :: forall m a. (MonadResource m, U.MonadUnliftIO m)
  => Credential -> T.Text -> T.Text -> ([Oracle.DataField] -> m a) -> m (TBMChan a)
pgRawChan (Credential hostName hostPort username password) database sql decode = do
  
oracleChan :: forall m a. (MonadResource m, U.MonadUnliftIO m)
  => Credential -> T.Text -> T.Text  -> m (TBMChan [Oracle.DataField])
oracleChan credential database sql = oracleRawChan credential database sql return

oracleJSONChan :: forall m a. (J.FromJSON a, MonadResource m, U.MonadUnliftIO m)
  => Credential -> T.Text -> T.Text  -> m (TBMChan (Maybe a))
oracleJSONChan credential database sql = oracleRawChan credential database sql decode
  where decode v = do
          jv <- liftIO $ Oracle.fromDataFields' v
          case J.fromJSON jv of
            J.Error s -> return Nothing
            J.Success v -> return $ Just v
          
oracleShowTables ::  forall m. (MonadIO m, U.MonadUnliftIO m)
  => Credential -> T.Text -> m [("schema" := T.Text, "table" := T.Text)]
oracleShowTables credential database =
  runConduitRes $ (lift chan >>= sourceTBMChan) .| C.concatMap id .| C.sinkList
  where
    chan = oracleJSONChan credential database sql
    sql = [str| select owner as schema, table_name as table
              | from all_tables
              | where tablespace_name not in ('SYSTEM', 'SYSAUX')
              |]

oracleDescribeTable :: forall m. (MonadIO m, U.MonadUnliftIO m)
  => Credential -> T.Text -> (T.Text, T.Text) -> m [("name" := T.Text, "type" := T.Text, "desc" := T.Text)]
oracleDescribeTable credential database (schema, table) = do
  liftIO $ print sql
  runConduitRes $ (lift chan >>= sourceTBMChan) .| C.concatMap id .| C.sinkList
  where
    chan = oracleJSONChan credential database sql
    sql =    "select t1.column_name as name, t1.data_type as type, t2.comments as desc \n"
          <> "  from all_tab_columns t1 inner join all_col_comments t2 \n"
          <> "    on t1.owner = t2.owner and t1.table_name = t2.table_name and t1.column_name = t2.column_name\n"
          <> " where t1.owner = '" <> schema <> "' AND t1.table_name = '" <> table <> "'\n"
--}
--oracleSelectSQL 
