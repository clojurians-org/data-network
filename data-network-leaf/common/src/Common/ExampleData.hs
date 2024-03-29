{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels, DataKinds, TypeOperators #-}

module Common.ExampleData where

import qualified DataNetwork.Core.Types as DN
import Common.Types
import Common.Class
import Prelude

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Heredoc (str)
import qualified Data.Tree as TR
import Data.Default (def)
import Data.String.Conversions (cs)

import Data.Function ((&))
import Control.Applicative (liftA2)
import Labels ((:=)(..))
import qualified Labels as L
import Data.Maybe (fromJust)
import qualified Data.HashMap.Lazy as M

import qualified Data.Aeson as J

exampleFaasCenter :: FaaSCenter
exampleFaasCenter =
  ( #dataNetwork := ( #eventPulses :=
                        (M.fromList $ fmap (liftA2 (,) epName id) exampleEventPulses)
                    , #dataCircuits :=
                        (M.fromList $ fmap (liftA2 (,) (fromJust . dciXid) id) exampleDataCircuits)
                    , #dataConduits := M.empty
--                        fmap (liftA2 (,) (fromJust . dcoXid) id) exampleDataConduits
                    , #logicFragments := M.empty
                    , #primLogicFragments := M.empty )
  , #dataSandbox := ( #stateContainers := M.empty
                    , #dataSources :=
                        (M.fromList $ fmap (liftA2 (,) (fromJust . getDataSourceId) id) exampleDataSources)
                    , #dataServices :=
                        (M.fromList $ fmap (liftA2 (,) (fromJust . getDataServiceId) id) exampleDataServices) )
  , #eventLake := ( #cronTimers := M.empty
                  , #fileWatchers := M.empty
                  , #sqlScanners :=
                       (M.fromList $ fmap (liftA2 (,) (fromJust . L.get #xid . DN.label) id) exampleSQLScanners)
                    )
  , #faas := M.empty
    )

exampleSQLScanners :: [DN.SQLScanner]
exampleSQLScanners =
  [ DN.SQLScanner
      ( #name := "dw_schedule"
      , #desc := "数仓调度系统扫描"
      , #cron := DN.CronExpr "0/1 * * * *"
      , #sql_connect := DN.SQLConnect
          ( #type := DN.Oracle
          , #credential := DN.Credential "10.132.37.241" 1521 "KB" "KB123456"
          , #database_name := DN.DatabaseName "EDMP")
      , #sql := [str| SELECT t2.RUN_END_DATE as OFFSET
                    |      , t1.JOB_NAME
                    |      , t2.EXECUTE_STATE
                    |   FROM SCH_CURR_JOB t1
                    |  INNER JOIN SCH_CURR_JOBSTATE t2
                    |     ON t1.JOB_SEQ = T2.JOB_SEQ
                    |]
      , #increment_field := ""
      , #xid := Just 1 )
    ]

exampleDataSources :: [DataSource]
exampleDataSources =
  [ DSO_SQLCursor (def { dsoSQLCursorName = "sqlCursor_pg_tb_interface"
                       , dsoSQLCursorDesc = "sqlCursor_pg_tb_interface"
                       , dsoSQLCursorType = "PostgreSQL"
                       , dsoSQLCursorHost = "10.132.37.200:5432"
                       , dsoSQLCursorDatabase = "monitor"
                       , dsoSQLCursorUsername = "monitor"
                       , dsoSQLCursorPassword = "monitor"
                       , dsoSQLCursorTable = "tb_interface"
                       , dsoSQLCursorFields = []
                       , dsoSQLCursorXid = Just 1})
  , DSO_SQLCursor (def { dsoSQLCursorName = "sqlCursor_ora_tb_interface"
                       , dsoSQLCursorDesc = "sqlCursor_ora_tb_interface"
                       , dsoSQLCursorType = "Oracle"
                       , dsoSQLCursorHost = "10.132.37.241:1521"
                       , dsoSQLCursorDatabase = "EDMP"
                       , dsoSQLCursorUsername = "KB"
                       , dsoSQLCursorPassword = "KB123456"
                       , dsoSQLCursorTable = "KB.TB_INTERFACE_LOG"
                       , dsoSQLCursorFields = []
                       , dsoSQLCursorXid = Just 2})
  , DSO_SQLCursor (def { dsoSQLCursorName = "sqlCursor_ora_schedule"
                       , dsoSQLCursorDesc = "sqlCursor_ora_schedule"
                       , dsoSQLCursorType = "Oracle"
                       , dsoSQLCursorHost = "10.129.35.227:1521"
                       , dsoSQLCursorDatabase = "EDWDB"
                       , dsoSQLCursorUsername = "SCHNEW"
                       , dsoSQLCursorPassword = "SCHNEW"
                       , dsoSQLCursorTable = "KB.TB_INTERFACE_LOG"
                       , dsoSQLCursorFields = []
                       , dsoSQLCursorXid = Just 3})         
    
    ]


exampleDataServices :: [DataService]
exampleDataServices = 
  [ DSE_FileService_SFTP (def { dsefsSFtpName = "sftp_my_201"
                              , dsefsSFtpDesc = "sftp_my_201"
                              , dsefsSFtpHost = "10.132.37.201:22"
                              , dsefsSFtpUsername = "op"
                              , dsefsSFtpPassword = "op"
                              , dsefsSFtpFilePath = "./"
                              , dsefsSFtpFilePattern = "larluo.txt"
                              , dsefsSFtpXid = (Just 1)
                              })
  , DSE_FileService_SFTP (def { dsefsSFtpName = "sftp_my_202"
                              , dsefsSFtpDesc = "sftp_my_202"
                              , dsefsSFtpHost = "10.132.37.202:22"
                              , dsefsSFtpUsername = "op"
                              , dsefsSFtpPassword = "op"
                              , dsefsSFtpFilePath = "./"
                              , dsefsSFtpFilePattern = "larluo.txt"
                              , dsefsSFtpXid = (Just 2)
                              })
    
    ]

exampleEventPulses :: [EventPulse]
exampleEventPulses = do
  [ def { epName = "EP-dw_schedule"
        , epDesc = "数仓作业通知"
        , epDataCircuitValues = exampleDataCircuitValues }
    ]
exampleDataCircuitValues :: [DataCircuitValue]
exampleDataCircuitValues = do
  [ def { dcivGuard = M.singleton "JOB_NAME" (J.String "PR_TM_NCMS_LOAN_CONT_INFO")
        , dcivName = "DCV_HRFilePush"
        , dcivDesc = "华瑞银行数据下传平台"
        , dcivLinkedDataCircuit = (3, "文件下传平台")
        , dcivLinkedDataSandbox = def
            { ldsaStateContainers = []
            , ldsaDataSources = [(2, "sqlCursor_ora_tb_interface")]
            , ldsaDataServices = [(1, "sftp_my_201")]
            }
        }
    ]

exampleDataCircuits :: [DataCircuit]
exampleDataCircuits =
  [ def { dciName = "DataCircuitREPL"
        , dciDesc = "数据电路REPL"
        , dciXid = Just 0}
  , def { dciName = "FileLoadPlatform"
        , dciDesc = "文件加载平台"
        , dciXid = Just 1 }
  , def { dciName = "DWSchedulePlatform"
        , dciDesc = "数仓调度平台"
        , dciXid = Just 2 }
  , def { dciName = "FilePushPlatform"
        , dciDesc = "文件下传平台"
        , dciDataSandboxHolder = def
            { dsahDataSources = [ DSOH_SQLCursor ]
            , dsahDataServices = [ DSEH_FileService_MinIO ] }
        , dciPartCombinator = examplePartCombinator
        , dciXid = Just 3 }
  , def { dciName = "dataQueryPlatform"
        , dciDesc = "数据查询平台"
        , dciDataSandboxHolder = def
            { dsahDataSources = [ DSOH_SQLCursor ] }
        , dciXid = Just 4}
  , def { dciName = "ExternalDataPlatform"
        , dciDesc = "外部数据平台"
        , dciDataSandboxHolder = def
            { dsahStateContainers = [ SCH_PostgreSQL ]
            , dsahDataSources = [ DSOH_SQLCursor ] }
        , dciXid = Just 5 }
  , def { dciName = "LogPullPlatform"
        , dciDesc = "日志抽取平台"
        , dciXid = Just 6 }
  , def { dciName = "StreamingPlatform"
        , dciDesc = "流式计算平台"
        , dciXid = Just 7 }
  , def { dciName = "RealtimeAlertPlatform"
        , dciDesc = "实时预警平台"
        , dciXid = Just 8 }
  , def { dciName = "MachineLearningPlatform"
        , dciDesc = "机器学习平台"
        , dciXid = Just 9 }
    ]
  
examplePartCombinator :: TR.Tree DataCircuitPart
examplePartCombinator = do
  TR.Node def
    [ flip TR.Node [] $ DCIP_EmbededDataConduit $ def
        { dcoName = "aaa"
        , dcoPartCombinator =
            TR.Node def
              [ flip TR.Node [] $ DCOP_EmbededPrimLogicFragment $ def
                  { plfName = "conduit"
                  , plfDesc = ""
                  , plfEffectEngineCode = (Nothing, exampleConduitCode)
                    }
              ]
          }
    ]

exampleConduitCode :: T.Text
exampleConduitCode = (cs . unlines)
  [   "  void . runConduitRes . flip runReaderT dataSandbox $ do"
    , "    dataSandbox <- ask"
    , "    lift $ (L.get #dataSource dataSandbox )"
    , "      .| C.concat"
    , "      .| C.take 3"
    , "      .| C.map ((<> \"\\n\") .cs . J.encode)"
    , "      .| (L.get #dataService dataSandbox)"
    , "      .| C.sinkList"
    ]
  
tshow :: (Show a) => a -> T.Text
tshow = cs . show

repl :: IO ()
repl = do
  exampleEventPulses
    & head
    & toHaskellCodeBuilder exampleFaasCenter
    & toHaskellCode & T.putStrLn

