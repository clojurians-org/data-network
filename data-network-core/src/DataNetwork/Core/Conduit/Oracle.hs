{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, LambdaCase, ExtendedDefaultRules #-}
{-# lANGUAGE TupleSections, DataKinds, TypeOperators #-}
{-# LANGUAGE RankNTypes, FlexibleInstances, RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module DataNetwork.Core.Conduit.Oracle where

import DataNetwork.Core.Types
import DataNetwork.Core.Utils

import Prelude
--import qualified Text.Show as T
import TextShow
import Data.Maybe (catMaybes)
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Aeson as J
import qualified Data.Aeson.Lens as J

import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M
import Data.Maybe (isJust, fromMaybe)
import Data.Functor ((<&>))
import Data.String.Conversions (cs)
import Text.Heredoc (str)
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time

import Control.Applicative (liftA2)
import qualified Database.Dpi as Oracle
import qualified Database.Dpi.Field as Oracle

import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Resource (MonadResource, allocate, release, runResourceT)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import Control.Exception (finally, catch, SomeException(..))
import Control.Monad (when, void, forM_)

import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChanIO, closeTBMChan, writeTBMChan, readTBMChan)
import Data.Conduit.TMChan (sourceTBMChan)

import qualified UnliftIO as U

import Labels ((:=)(..))
import Control.Lens
import Data.Maybe (fromJust)

import Text.InterpolatedString.Perl6 (q, qc, qq)

instance Oracle.FromDataField (T.Text, J.Value) where
  fromDataField fd@Oracle.DataField{..} = runMaybeT $ do
    jv <- MaybeT $ go fd value
    name <- lift $ Oracle.getName fd
    return (cs name, jv)
    where
      utcTimeToScientific = realToFrac . Time.utcTimeToPOSIXSeconds
      zonedTimeToScientific = realToFrac . Time.utcTimeToPOSIXSeconds . Time.zonedTimeToUTC
      go v (Oracle.DataNull          _) = return . Just $ J.Null
      go v (Oracle.DataBoolean       _) = (fmap . fmap) J.Bool (Oracle.fromDataField v :: IO (Maybe Bool))
      go v (Oracle.DataInt           _) = (fmap . fmap) (J.Number . fromInteger) (Oracle.fromDataField v :: IO (Maybe Integer))
      go v (Oracle.DataNumInt        _) = (fmap . fmap) (J.Number . fromInteger) (Oracle.fromDataField v :: IO (Maybe Integer))
      go v (Oracle.DataNumUint       _) = (fmap . fmap) (J.Number . fromInteger) (Oracle.fromDataField v :: IO (Maybe Integer))
      go v (Oracle.DataUint          _) = (fmap . fmap) (J.Number . fromInteger) (Oracle.fromDataField v :: IO (Maybe Integer))
      go v (Oracle.DataDouble        _) = (fmap . fmap) (J.Number . realToFrac) (Oracle.fromDataField v :: IO (Maybe Double))
      go v (Oracle.DataNumDouble     _) = (fmap . fmap) (J.Number . realToFrac) (Oracle.fromDataField v :: IO (Maybe Double))
      go v (Oracle.DataNumBytes      _) = (fmap . fmap) (J.Number . realToFrac) (Oracle.fromDataField v :: IO (Maybe Double))
      go v (Oracle.DataFloat         _) = (fmap . fmap) (J.Number . realToFrac) (Oracle.fromDataField v :: IO (Maybe Float))
      go v (Oracle.DataChar          _) = (fmap . fmap) (J.String . cs @B.ByteString) (Oracle.fromDataField v :: IO (Maybe B.ByteString))
      go v (Oracle.DataLongRaw       _) = (fmap . fmap) (J.String . cs @B.ByteString) (Oracle.fromDataField v :: IO (Maybe B.ByteString))
      go v (Oracle.DataLongVarchar   _) = (fmap . fmap) (J.String . cs @B.ByteString) (Oracle.fromDataField v :: IO (Maybe B.ByteString))
      go v (Oracle.DataNChar         _) = (fmap . fmap) (J.String . cs @B.ByteString) (Oracle.fromDataField v :: IO (Maybe B.ByteString))
      go v (Oracle.DataNVarchar      _) = (fmap . fmap) (J.String . cs @B.ByteString) (Oracle.fromDataField v :: IO (Maybe B.ByteString))
      go v (Oracle.DataRaw           _) = (fmap . fmap) (J.String . cs @B.ByteString) (Oracle.fromDataField v :: IO (Maybe B.ByteString))
      go v (Oracle.DataVarchar       _) = (fmap . fmap) (J.String . cs @B.ByteString) (Oracle.fromDataField v :: IO (Maybe B.ByteString))
      go v (Oracle.DataIntervalDs    _) = (fmap . fmap) (J.Number . realToFrac) (Oracle.fromDataField v :: IO (Maybe Time.DiffTime)   )
      go v (Oracle.DataIntervalYm    _) = (fmap . fmap) (J.Number . realToFrac) (Oracle.fromDataField v :: IO (Maybe Time.DiffTime)   )
      go v (Oracle.DataTimestampD    _) = (fmap . fmap) (J.Number . utcTimeToScientific) (Oracle.fromDataField v :: IO (Maybe Time.UTCTime)    )
      go v (Oracle.DataTimestampLtzD _) = (fmap . fmap) (J.Number . zonedTimeToScientific) (Oracle.fromDataField v :: IO (Maybe Time.ZonedTime)  )
      go v (Oracle.DataTimestampTzD  _) = (fmap . fmap) (J.Number . zonedTimeToScientific) (Oracle.fromDataField v :: IO (Maybe Time.ZonedTime)  )
      go v (Oracle.DataDate          _) = (fmap . fmap) (J.Number . utcTimeToScientific) (Oracle.fromDataField v :: IO (Maybe Time.UTCTime)    )
      go v (Oracle.DataTimestamp     _) = (fmap . fmap) (J.Number . utcTimeToScientific) (Oracle.fromDataField v :: IO (Maybe Time.UTCTime)    )
      go v (Oracle.DataTimestampLtz  _) = (fmap . fmap) (J.Number . zonedTimeToScientific) (Oracle.fromDataField v :: IO (Maybe Time.ZonedTime)  )
      go v (Oracle.DataTimestampTz   _) = (fmap . fmap) (J.Number . zonedTimeToScientific) (Oracle.fromDataField v :: IO (Maybe Time.ZonedTime)  )
      go _ _                     = U.throwString "unhandle data type!"

instance Oracle.FromDataFields J.Value where
  fromDataFields' = fmap (J.Object . M.fromList . catMaybes) . mapM Oracle.fromDataField
  
oracleRawChan :: forall m a. (MonadResource m, U.MonadUnliftIO m)
  => Credential -> T.Text -> T.Text -> ([Oracle.DataField] -> m a) -> m (TBMChan a)
oracleRawChan (Credential hostName hostPort username password) database sql decode = do
  let config = Oracle.defaultOracle (cs username) (cs password)
                 (cs (hostName <> ":" <> tshow hostPort <> "/" <> database))
      defCursorSize = 2000
      defBatchSize = 20000
  (reg, chan) <- allocate (newTBMChanIO defBatchSize) (U.atomically . closeTBMChan)
  U.async $ bracketR Oracle.createContext Oracle.destroyContext $ \ctx ->
    bracketR (Oracle.createConnection ctx config return)
             (\c -> Oracle.closeConnection Oracle.ModeConnCloseDefault c
                     `finally` Oracle.releaseConnection c) $ \conn ->
      bracketR (Oracle.prepareStatement conn False (cs sql)) Oracle.releaseStatement $ \stmt -> do
          liftIO $ Oracle.setFetchArraySize stmt defCursorSize
          cn <- liftIO $ Oracle.executeStatement stmt Oracle.ModeExecDefault
                `catch` \(SomeException e) -> print e >> fail (show e)
          sinkRows decode chan stmt cn defCursorSize
          liftIO $ release reg
  return chan
  where
    -- https://github.com/oracle/odpi/issues/70
    sinkRows decode chan stmt cn cursorSize = do
      offset <- liftIO $ Oracle.fetch stmt
      when (isJust offset)  $ do
        row <- liftIO $ mapM (\i -> Oracle.DataField <$> Oracle.getQueryInfo stmt i
                                            <*> Oracle.getQueryValue stmt i)
                  [1..cn]
        v <- decode row
        liftIO $ U.atomically $ writeTBMChan chan v
        sinkRows decode chan stmt cn cursorSize
        
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
  C.runConduitRes $ (lift chan >>= sourceTBMChan) .| C.concatMap id .| C.sinkList
  where
    chan = oracleJSONChan credential database sql
    sql = [str| select owner as "schema", table_name as "table"
              | from all_tables
              | where tablespace_name not in ('SYSTEM', 'SYSAUX')
              |]

oracleDescribeTable :: forall m. (MonadIO m, U.MonadUnliftIO m)
  => Credential -> T.Text -> (T.Text, T.Text) -> m [("name" := T.Text, "type" := T.Text, "desc" := T.Text)]
oracleDescribeTable credential database (schema, table) = do
  C.runConduitRes $ (lift chan >>= sourceTBMChan) .| C.concatMap id .| C.sinkList
  where
    chan = oracleJSONChan credential database sql
    sql =    " select t1.column_name as \"name\", t1.data_type as \"type\", t2.comments as \"desc\" \n"
          <> " from all_tab_columns t1 inner join all_col_comments t2 \n"
          <> "    on t1.owner = t2.owner and t1.table_name = t2.table_name and t1.column_name = t2.column_name\n"
          <> " where t1.owner = '" <> schema <> "' AND t1.table_name = '" <> table <> "'\n"

maxOracleOffset :: forall m. (MonadIO m, U.MonadUnliftIO m)
  => Credential -> T.Text -> T.Text -> m J.Value
maxOracleOffset credential database sql = do

  C.runConduitRes $ (lift chan >>= sourceTBMChan) .| C.concatMap id .| C.sinkList
    <&> fromJust . (^? _head . (J.key "MAX_OFFSET" :: Traversal' J.Value J.Value))
  where
    chan = oracleJSONChan credential database sql'
    sql' = [qc| SELECT MAX(offset) AS MAX_OFFSET FROM ( { sql } ) t |]

aboveOracleOffset :: forall m. (MonadIO m, U.MonadUnliftIO m)
  => Maybe J.Value -> Credential -> T.Text -> T.Text -> m ([J.Value], J.Value)
aboveOracleOffset offsetMv credential database sql = do
  offset <- maybe (maxOracleOffset credential database sql) return offsetMv
--  liftIO $ printT (mkSQL offset)
  C.runConduitRes
     $ (lift (chan offset) >>= sourceTBMChan) .| C.concatMap id
    .| C.getZipConduit ((,) <$> C.ZipConduit C.sinkList
                            <*> C.ZipConduit ( C.concatMap (^? J.key "OFFSET")
                                            .| C.foldl maxOffset offset))

  --undefined             
  where
    chan = oracleJSONChan credential database . mkSQL
    mkSQL = \case
      J.Null -> [qc| { sql } |]
      J.String s -> [qc| SELECT * FROM ({ sql }) t WHERE t.OFFSET > '{s}' |]
      J.Number n -> [qc| SELECT * FROM ({ sql }) t WHERE t.OFFSET > {n} |]
      x -> error ("unhandle json type: " <> show x)
    maxOffset :: J.Value -> J.Value -> J.Value
    maxOffset (J.String a) (J.String b) = J.String (max a b)
    maxOffset (J.Number a) (J.Number b) = J.Number (max a b)
    maxOffset J.Null b = b
    maxOffset a J.Null = a
    maxOffset a b = error ("unhandle: " <> show a <> show b)

oracleRepl :: IO ()
oracleRepl = do
--  a <- oracleShowTables (Credential "10.129.35.227" 1521 "SCHNEW" "SCHNEW") "EDWDB"
--  print a
  b <- oracleDescribeTable (Credential "10.129.35.227" 1521 "SCHNEW" "SCHNEW") "EDWDB" ("SCHNEW", "SCH_CURR_JOB")
  print b
  {--
  (rs, mv') <- aboveOracleOffset (Just J.Null) (Credential "10.129.35.227" 1521 "SCHNEW" "SCHNEW")  "EDWDB" $ sql
  printT . show $ mv'
  --forM_ rs (printT . show)
  printT "finished"
  --}
  where
    sql = [str| SELECT t2.RUN_END_DATE as OFFSET
              |      , t1.JOB_NAME
              |      , t2.EXECUTE_STATE
              |   FROM SCH_CURR_JOB t1
              |  INNER JOIN SCH_CURR_JOBSTATE t2
              |     ON t1.JOB_SEQ = T2.JOB_SEQ
              |]
