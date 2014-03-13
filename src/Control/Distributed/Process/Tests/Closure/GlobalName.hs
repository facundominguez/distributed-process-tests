{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StaticValues #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Distributed.Process.Tests.Closure.GlobalName (tests) where

import Network.Transport.Test (TestTransport(..))

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  ( newEmptyMVar
  , readMVar
  , takeMVar
  , putMVar
  )
import qualified Control.Distributed.Process.Internal.Spawn as G
import Control.Distributed.Static.GlobalName hiding (Static, Closure, staticClosure)
import Control.Distributed.Static.Generic
  ( Resolve(..)
  , lmapStatic
  , Inject(..)
  , staticClosure
  )
import qualified Control.Distributed.Static.Generic as G (Static, Closure)
import Control.Distributed.Static (StringLabel)
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Control.Distributed.Process (RemoteTable, Process, liftIO)
import Control.Distributed.Process.Internal.Types
  ( LocalNode(remoteTable)
  , ResolveDict(..)
  )
import Control.Distributed.Process.Internal.Closure.BuiltIn (returnCP, bindCP)
import Control.Monad.Reader(withReaderT, ReaderT)
import Control.Monad.Trans(lift)

import Test.HUnit (Assertion)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)

sdictInt :: SerializableDict Int
sdictInt = SerializableDict

factorial :: Int -> Process Int
factorial 0 = return 1
factorial n = (n *) <$> factorial (n - 1)

type Static a = G.Static (Either StringLabel GlobalName) a
type Closure a = G.Closure (Either StringLabel GlobalName) a

instance Resolve (ReaderT LocalNode IO) (Either StringLabel GlobalName) where
  resolve = either (withReaderT remoteTable . resolve) (lift . resolve)

gnDict :: ResolveDict (ReaderT LocalNode IO) (Either StringLabel GlobalName)
gnDict = ResolveDict

stSDictInt :: Static (SerializableDict Int)
stSDictInt = lmapStatic inject $ staticLabel $ static sdictInt

factorialOf :: Closure (Int -> Process Int)
factorialOf = staticClosure $ lmapStatic inject $ staticLabel $ static factorial

factorial' :: Int -> Closure (Process Int)
factorial' n = returnCP stSDictInt n `bindCP` factorialOf

testCallGlobalName :: TestTransport -> RemoteTable -> Assertion
testCallGlobalName TestTransport{..} rtable = do
  serverNodeAddr <- newEmptyMVar
  clientDone <- newEmptyMVar

  forkIO $ do
    node <- newLocalNodeAnyLabel testTransport gnDict rtable
    putMVar serverNodeAddr (localNodeId node)

  forkIO $ do
    node <- newLocalNodeAnyLabel testTransport gnDict rtable
    nid <- readMVar serverNodeAddr
    runProcess node $ do
      120 <- G.call stSDictInt nid ($(mkClosureNI 1 [| factorial |]) (5 :: Int))
      liftIO $ putMVar clientDone ()

  takeMVar clientDone

testCallBindGlobalName :: TestTransport -> RemoteTable -> Assertion
testCallBindGlobalName TestTransport{..} rtable = do
  serverNodeAddr <- newEmptyMVar
  clientDone <- newEmptyMVar

  forkIO $ do
    node <- newLocalNodeAnyLabel testTransport gnDict rtable
    putMVar serverNodeAddr (localNodeId node)

  forkIO $ do
    node <- newLocalNodeAnyLabel testTransport gnDict rtable
    nid <- readMVar serverNodeAddr
    runProcess node $ do
      (120 :: Int) <- G.call stSDictInt nid (factorial' 5)
      liftIO $ putMVar clientDone ()

  takeMVar clientDone

tests :: TestTransport -> IO [Test]
tests testtrans = do
    let rtable = initRemoteTable
    return
        [ testCase "CallGlobalName"  (testCallGlobalName  testtrans rtable)
        , testCase "CallBindGlobalName"  (testCallBindGlobalName  testtrans rtable)
        ]
