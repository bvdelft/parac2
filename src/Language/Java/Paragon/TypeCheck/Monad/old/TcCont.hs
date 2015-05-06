module Language.Java.Paragon.TypeCheck.Monad.TcCont where

-- import Language.Java.Paragon.Syntax

import Language.Java.Paragon.TypeCheck.Monad.TcBase (TcBase, runTcBase)
import qualified Language.Java.Paragon.TypeCheck.Monad.TcBase 
    as Base (getUniqRef, getTypeMap, getThisT, withTypeMap, liftIO, 
             withErrCtxt, getErrCtxt, withErrCtxt')

import Language.Java.Paragon.TypeCheck.Uniq (Uniq)
import Language.Java.Paragon.TypeCheck.TcEnv (TypeMap)
import Language.Java.Paragon.TypeCheck.Types (TcType)

import Control.Monad (liftM)  -- liftM only to instantiate fmap


-----------------------------------------------
--            The TcCont monad               --
-----------------------------------------------

newtype TcCont r a = TcCont ((a -> TcBase r) -> TcBase r)

runTcCont :: TcType -> TcCont a a -> IO (Either String a)
runTcCont typ tcca = runTcBase typ $ runCont2Base tcca return
    where runCont2Base :: TcCont r a -> (a -> TcBase r) -> TcBase r
          runCont2Base (TcCont f) k = f k



instance Monad (TcCont r) where
  return x = TcCont $ \k -> k x
  TcCont f >>= h = TcCont $ \k ->
                    f (\a -> let TcCont g = h a in g k)
  fail err = liftBase (fail err)

instance Functor (TcCont r) where
  fmap = liftM

-----------------------------------------------
-- Here's the whole reason why we go through 
-- this lifting charade

callCC :: ((a -> TcCont r b) -> TcCont r a) -> TcCont r a
callCC cont = TcCont $ \k -> 
                let TcCont g = cont (\a -> TcCont $ \_ -> k a) in g k


-----------------------------------------------
-- Lifting the TcBase combinators to TcCont

liftBase :: TcBase a -> TcCont r a
liftBase tcba = TcCont $ \k -> tcba >>= k

getUniqRef :: TcCont r Uniq
getUniqRef = liftBase Base.getUniqRef

getTypeMap :: TcCont r TypeMap
getTypeMap = liftBase Base.getTypeMap

getThisT :: TcCont r TcType
getThisT = liftBase Base.getThisT

withTypeMap :: (TypeMap -> TypeMap) -> TcCont r a -> TcCont r a
withTypeMap tmf (TcCont f) = 
    TcCont $ \k -> do
      tm <- Base.getTypeMap
      Base.withTypeMap tmf $ f (\a -> Base.withTypeMap (const tm) $ k a)
                               

--    = TcCont $ \k -> Base.withTypeMap tmf (f return) >>= k
--    = TcCont $ \k -> Base.withTypeMap tmf $ f k

liftIO :: IO a -> TcCont r a
liftIO = liftBase . Base.liftIO

withErrCtxt :: String -> TcCont r a -> TcCont r a
withErrCtxt str (TcCont f) =
    TcCont $ \k -> do 
      ec <- Base.getErrCtxt
      Base.withErrCtxt str $ f (\a -> Base.withErrCtxt' (const ec) $ k a)
