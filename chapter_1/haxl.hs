{-# LANGUAGE ExistentialQuantification, StandaloneDeriving, RankNTypes #-}

module Main where

import Control.Monad
import Control.Applicative
import Control.Exception (SomeException, toException, fromException, Exception)

import Data.IORef
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Monoid
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as M

import Unsafe.Coerce

data Request a = Request String

data FetchStatus a
  = NotFetched
  | FetchSuccess a
  | FetchFailure SomeException

data BlockedRequest =
  forall a. BlockedRequest (Request a) (IORef (FetchStatus a))

newtype DataCache =
  DataCache (forall a. HashMap (Request a) (IORef (FetchStatus a)))

deriving instance Eq (Request a)
instance Hashable (Request a) where
  hashWithSalt s (Request str) = hashWithSalt s str

lookupCache :: Request a -> DataCache -> Maybe (IORef (FetchStatus a))
lookupCache key (DataCache m) = M.lookup key m

insertCache :: Request a -> IORef (FetchStatus a) -> DataCache -> DataCache
insertCache key val (DataCache m) =
  DataCache $ unsafeCoerce (M.insert key val m)

data Result a
  = Done a
  | Blocked (Seq BlockedRequest) (Fetch a)
  | Throw SomeException

newtype Fetch a = Fetch { unFetch :: IORef DataCache -> IO (Result a) }

throw :: Exception e => e -> Fetch a
throw e = Fetch $ const (return $ Throw (toException e))

instance Functor Fetch where
  fmap f (Fetch v) = Fetch $ \ref -> do
    r <- v ref
    case r of
      Done a       -> return $ Done (f a)
      Blocked br c -> return $ Blocked br (fmap f c)
      Throw e      -> return $ Throw e

instance Monad Fetch where
  return a = Fetch $ \ref -> return (Done a)
  Fetch m >>= k = Fetch $ \ref -> do
    r <- m ref
    case r of
      Done a       -> unFetch (k a) ref
      Blocked br c -> return $ Blocked br (c >>= k)
      Throw e      -> return $ Throw e

instance Applicative Fetch where
  pure = return
  Fetch f <*> Fetch x = Fetch $ \ref -> do
    f' <- f ref
    x' <- x ref
    case (f', x') of
      (Done g,        Done y      ) -> return $ Done (g y)
      (Done g,        Blocked br c) -> return $ Blocked br (g <$> c)
      (Done _,        Throw e     ) -> return $ Throw e
      (Blocked br c,  Done y      ) -> return $ Blocked br (c <*> return y)
      (Blocked b1 c,  Blocked b2 d) -> return $ Blocked (b1 <> b2) (c <*> d)
      (Blocked br c,  Throw e     ) -> return $ Blocked br (c <*> throw e)
      (Throw e,       _           ) -> return $ Throw e

catch :: Exception e => Fetch a -> (e -> Fetch a) -> Fetch a
catch (Fetch h) handler = Fetch $ \ref -> do
  r <- h ref
  case r of
    Done a -> return $ Done a
    Blocked br c ->
      return $ Blocked br (catch c handler)
    Throw e -> case fromException e of
      Just e' -> unFetch (handler e') ref
      Nothing -> return $ Throw e

dataFetch :: Request a -> Fetch a
dataFetch req = Fetch $ \ref -> do
  cache <- readIORef ref
  case lookupCache req cache of
    Nothing -> do
      box <- newIORef NotFetched
      writeIORef ref (insertCache req box cache)
      let br = BlockedRequest req box
      return $ Blocked (S.singleton br) (cont box)
    Just box -> do
      r <- readIORef box
      case r of
        FetchSuccess result ->
          return $ Done result
        NotFetched ->
          return $ Blocked S.empty (cont box)
        FetchFailure e ->
          return $ Throw e
  where
    cont box = Fetch $ \ref -> do
      FetchSuccess a <- readIORef box
      return (Done a)

fetch :: Seq BlockedRequest -> IO ()
fetch = F.mapM_ runReq
  where
    runReq (BlockedRequest (Request str) status) = do
      putStrLn $ "Req: " ++ str
      writeIORef status $ FetchSuccess (unsafeCoerce str)

runFetch :: Fetch a -> IO a
runFetch (Fetch h) = do
  ref <- newIORef $ DataCache M.empty
  r <- h ref
  case r of
    Done a -> return a
    Blocked br cont -> do
      fetch br
      runFetch cont
