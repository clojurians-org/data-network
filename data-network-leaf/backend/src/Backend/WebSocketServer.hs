{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}


--module Backend.WebSocketServer (serveWebSocket) where
module Backend.WebSocketServer where

import qualified DataNetwork.Core.Types as DN
import DataNetwork.Core.Conduit

import Common.Class
import Common.Types
import Common.WebSocketMessage
import Common.ExampleData

import Prelude
import Text.Heredoc (str)
import qualified TextShow as T

import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Combinators (mapLeft, maybeToLeft, maybeToRight)
import System.Random (randomRIO)

import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Functor ((<&>))

import Control.Exception (bracket, finally)
import GHC.Int (Int64)
import Data.String (IsString(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.String.Conversions (cs)
import qualified Data.Aeson as J
import qualified Network.WebSockets as WS
--import Data.Conduit (runConduit, yield, (.|))
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Conduit.TMChan ((>=<))
import qualified Data.Conduit.TMChan as CC
import qualified Control.Concurrent.STM.TBMChan as STM
import qualified Language.Haskell.Interpreter as I

import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import Snap.Core (MonadSnap)
import Network.WebSockets.Snap (runWebSocketsSnap)

import Control.Lens
import Control.Applicative ((<|>))
import qualified Labels as L

import Control.Monad.Except (runExceptT, liftEither)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExceptT, ExceptT(..), except)

import Data.Bits (Bits(..))
import Foreign.C.Types (CULong)

import Network.Socket (withSocketsDo)
import Network.SSH.Client.LibSSH2
  ( Sftp, Session, sessionInit, sessionClose
  , checkHost
  , withSFTPUser, withOpenSftpFile, sftpListDir)
import Network.SSH.Client.LibSSH2.Foreign
  ( SftpFileTransferFlags(..), KnownHostResult(..)
  , SftpAttributes(..)
  , usernamePasswordAuth, sftpInit, sftpShutdown
  , sftpOpenFile, sftpCloseHandle, sftpWriteFileFromBS)

import qualified UnliftIO as U
import qualified Control.Monad.Trans.Resource as R

import qualified Data.HashMap.Lazy as M
import qualified Data.HashMap.Strict as MS
import Data.Aeson.QQ (aesonQQ)
import qualified Control.Concurrent.Async.Lifted as L
import Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.Vinyl as V
import Data.Vinyl ((:::), (=:), Rec((:&)))

import Data.Time.Clock.POSIX (getPOSIXTime)

import Debug.Trace (trace, traceShow)

serveWebSocket :: MonadSnap m => MVar AppST ->  m ()
serveWebSocket appST = runWebSocketsSnap (wsConduitApp appST)

subJSON :: J.Value -> J.Value -> Bool
subJSON (J.Object m1) _ | null m1 = False
subJSON (J.Object m1) (J.Object m2) = do
  and . flip fmap (MS.toList m1) $ \(k1, v1) -> do
    case MS.lookup k1 m2 of
      Just v2 -> subJSON v1 v2
      Nothing -> False
subJSON (J.String s) (J.Number x) = s == (cs $ show x)
subJSON v1 v2 = (v1 == v2)

subJSONs :: J.Value -> [J.Value] -> Bool
subJSONs x [] = False
subJSONs x xs = all (subJSON x) xs

sinkEvent :: (MonadIO m, J.ToJSON a) => STM.TBMChan B.ByteString -> a -> m ()
sinkEvent brokerChan = U.atomically . STM.writeTBMChan brokerChan . J.encode

wsRepl2 :: IO ()
wsRepl2 = do
  let code = toHaskellCode $ toHaskellCodeBuilder exampleFaasCenter $ head exampleDataCircuitValues
  putStrLn (cs code)
  (liftIO $ I.runInterpreter $ dynHaskell code) >>= print
  

runDataCircuit :: (MonadIO m)
  => STM.TBMChan B.ByteString -> MVar AppST -> (T.Text, DataCircuitValue) -> m ()
runDataCircuit brokerChan appStmv (eventPulseName, dciv) = do
  ts <- liftIO $ getPOSIXTime

  sinkEvent brokerChan . AsyncDataCircuitBegin $
       #event_pulse =: eventPulseName
    :& #name =: dcivName dciv
    :& #ts =: ts
    :& V.RNil
  
  faas <- U.readMVar appStmv
  
  let haskellCode = toHaskellCode $ toHaskellCodeBuilder faas dciv
  
  -- liftIO $ T.printT haskellCode
  liftIO $ I.runInterpreter . dynHaskell $ haskellCode

  sinkEvent brokerChan . AsyncDataCircuitEnd $
       #event_pulse =: eventPulseName
    :& #name =: dcivName dciv
    :& #ts =: ts
    :& #result =: "SUCCESS"
    :& V.RNil

  return ()
  
activeEP :: (R.MonadUnliftIO m, MonadBaseControl IO m)
  => STM.TBMChan B.ByteString -> MVar AppST -> T.Text -> [J.Value] -> m ()
activeEP brokerChan appStmv name payload = do
  faas <- U.readMVar appStmv
  let getter = L.lens #dataNetwork . L.lens #eventPulses . at name
      eventPulseMaybe = view getter faas
  liftIO $ putStrLn $ "eventName:" <>  (cs name)      
  when (isJust eventPulseMaybe) $ do
    let eventPulse = fromJust eventPulseMaybe

    ts <- liftIO $ getPOSIXTime
    sinkEvent brokerChan . AsyncEventPulseActive $
      #name =: name :& #ts =: ts :& V.RNil
    
    C.runConduit $ C.yieldMany (epDataCircuitValues eventPulse)
                .| C.concatMap (guardPayload payload)
                .| C.mapM (L.async . runDataCircuit brokerChan appStmv . (,) name)
                .| C.sinkNull
  where
    guardPayload :: [J.Value] -> DataCircuitValue -> Maybe DataCircuitValue
    guardPayload payload dciv = do
      case subJSONs (J.toJSON . dcivGuard $ dciv) payload of
        True -> Just dciv
        False -> Nothing


rpcHandle :: (R.MonadResource m, R.MonadUnliftIO m, MonadBaseControl IO m) 
  => STM.TBMChan B.ByteString -> MVar AppST -> DN.RPCResponse -> m ()
rpcHandle brokerChan appStM (DN.FaasNotifyPush (DN.FaasKey "SQLScanner" name, "ScannerItemsEvent", payloadJV)) = do
  
  let scannerItemsEvent = J.fromJSON payloadJV :: J.Result DN.SQLScannerNotifyEvent
  case J.fromJSON payloadJV of
    J.Success (DN.ScannerItemsEvent items) -> do
      activeEP brokerChan appStM ("EP-" <> name) (items ^.. each . V.rlensf #row)
    _ -> fail "rpcHandle: SQLScannerNotifyEvent-PARSE ERROR!"
--  void $ activeEP appStM name
  return ()
rpcHandle _ _ _ = return ()

wsConduitApp :: MVar AppST -> WS.ServerApp
wsConduitApp appST pending= do
  putStrLn "ui-ws connection accepting ..."
  uiConn <-  WS.acceptRequest pending

  withSocketsDo $ WS.runClient "127.0.0.1" 1111 "/ws/" (\rpcConn -> R.runResourceT $ do
    (outReg, outChan) <- mkChan 1000
    liftIO $ putStrLn "node-ws connected  ..." 
    let uiSource = forever $ liftIO (WS.receiveData uiConn) >>= C.yield
        chanSink = CC.sinkTBMChan outChan
        rpcSink = C.awaitForever $ liftIO . WS.sendTextData rpcConn

        chanSource = CC.sourceTBMChan outChan
        rpcSource = forever $ liftIO (WS.receiveData rpcConn) >>= C.yield
        uiSink = C.awaitForever $ liftIO . WS.sendTextData uiConn

    joinSource <- (chanSource .|  C.iterM (liftIO . putStrLn . ("leaf-receive:" <>) . cs))
              >=< ( rpcSource
                 .| C.iterM (liftIO . putStrLn . ("rpc-receive:" <>) . cs)
                 .| C.concatMap J.decode
                 .| C.iterM (rpcHandle outChan appST)
                 .| C.map (J.encode . NodeRPCRes)
                 )

    L.concurrently_
      (C.runConduit
        $ uiSource
       .| C.iterM (liftIO . putStrLn . ("receive mesage:" <>) . cs)
       .| C.concatMap J.decode
       .| (C.getZipConduit
             $ C.ZipConduit ( C.concatMap fromLeafRPCReq
--                           .| C.iterM (liftIO . putStrLn . ("leaf-req:" <>) . cs . show)
                           .| C.mapM (wsHandle outChan appST)
                           .| C.map (J.encode @WSResponseMessage)
--                           .| C.iterM (liftIO . putStrLn . ("leaf-res:" <>) . cs)
                           .| chanSink
                            )
            *> C.ZipConduit ( C.concatMap fromNodeRPCReq
                           .| C.iterM (liftIO . putStrLn . ("rpc-proxy:" <>) . cs . show)
                           .| C.map (J.encode @DN.RPCRequest)
                           .| rpcSink
                            )
             )
        )


      (C.runConduit $ joinSource .| uiSink))
    `U.catch` \(U.SomeException e) -> putStrLn (show e)

  putStrLn "ui-ws connection finished ..."
  where
    mkChan :: (R.MonadResource m) =>Int -> m (R.ReleaseKey, CC.TBMChan a)
    mkChan n = R.allocate (STM.newTBMChanIO n) (U.atomically . STM.closeTBMChan)

wsHandle :: (MonadIO m, R.MonadUnliftIO m, MonadBaseControl IO m)
  => STM.TBMChan B.ByteString -> MVar AppST -> WSRequestMessage -> m WSResponseMessage

wsHandle _ appST AppInitREQ = do
  -- U.readMVar appST >>= liftIO . putStrLn . ("INIT REQ" ++ ) . show
  return . AppInitRES =<< U.readMVar appST
wsHandle _ appST (HaskellCodeRunRequest r) =
  return . HaskellCodeRunResponse . mapLeft show =<< 
    (liftIO . I.runInterpreter . dynHaskell) r
wsHandle brokerChan appST (EventPulseAREQ name) = do
--  faas <- U.readMVar appST  
  activeEP brokerChan appST name [J.Null]
  return . EventPulseARES $ Right ()
wsHandle _ appST (DSOSQLCursorDatabaseRREQ cr "Oracle" database) = do
  liftIO $ putStrLn (show cr <> "-" <> cs database)
  r <- DSOSQLCursorDatabaseRRES . Right . take 100 <$> oracleShowTables cr database
  liftIO $ putStrLn (show r)
  return r

wsHandle _ appST (DSOSQLCursorTableRREQ cr "Oracle" database (schema, table) ) = do
  DSOSQLCursorTableRRES . Right <$> oracleDescribeTable cr database (schema, table)


wsHandle _ appST (DSEFSSFtpDirectoryRREQ
                  (DN.Credential hostName hostPort username password)
                  path) = liftIO $ do
  bracket (sessionInit (cs hostName) hostPort) sessionClose $ \s -> do
    liftIO $ usernamePasswordAuth s (cs username) (cs password)
    bracket (sftpInit s) sftpShutdown $ \sftp -> do
      sftpList <- liftIO $ sftpListDir sftp (cs $ fromMaybe "." path)
      return . DSEFSSFtpDirectoryRRES . Right $ sftpList <&> \(name, attrs) -> do
        let size = (fromIntegral . saFileSize) attrs
            ctime = (realToFrac . saMtime) attrs
            xtype = (parseSFtpEntryType . saPermissions) attrs
        SFtpEntry (cs name) xtype size ctime
  where
    parseSFtpEntryType ::  CULong -> SFtpEntryType
    parseSFtpEntryType = \case
      a | a .&. 0o0100000 /= 0 -> SFtpFille
        | a .&. 0o0040000 /= 0 -> SFtpDirectory
      _ -> SFtpUnknown
  {--      
wsHandle appST unknown = do
  putStrLn $ "CronTimerDeleteResponse: " ++ (show unknown)
  return . WSResponseUnknown $ unknown

--}

dynHaskell :: T.Text -> I.Interpreter ()
dynHaskell stmt = do
  I.loadModules ["backend/src/Fn.hs" ]
  I.setTopLevelModules ["Fn"]
  I.set [I.languageExtensions I.:= [ I.OverloadedStrings
                                   , I.OverloadedLabels
                                   , I.TemplateHaskell
                                   , I.QuasiQuotes]]
  I.runStmt (cs stmt)

wsRepl :: IO ()
wsRepl = do
  {--
  let code = toHaskellCode . toHaskellCodeBuilder exampleFaasCenter $ (head exampleEventPulses)
  T.putStrLn code
  (mapLeft show <$> (I.runInterpreter . dynHaskell) code) >>= print
  wsHandle undefined
    (DSEFSSFtpDirectoryRREQ (Credential "10.132.37.201" 22 "op" "op") (Just ".")) >>= print
  --}
  b <- oracleDescribeTable (DN.Credential "10.129.35.227" 1521 "SCHNEW" "SCHNEW") "EDWDB"
         ("SCHNEW", "SCH_CURR_JOB")
  -- SCH_CURR_JOBSTATE
  -- SCH_CURR_JOB
  mapM_ (T.printT . show)  b
