module Language.Java.Paragon.TypeCheck.Monad.TcBase where

-- import Language.Java.Paragon.Syntax

import Language.Java.Paragon.TypeCheck.TcEnv
import Language.Java.Paragon.TypeCheck.Uniq
import Language.Java.Paragon.TypeCheck.Types (TcType)

import Control.Monad (liftM)         -- liftM only to instantiate fmap


------------------------------------------------
--             The TcBase monad               --
------------------------------------------------
--
-- Monad for typechecking on 
-- the CompilationUnit level:
-- Has a name environment, fresh name generation,
-- error capture, and IO.

type ErrCtxt = String -> String

newtype TcBase a =
    TcBase (TypeMap -> TcType -> Uniq -> ErrCtxt -> IO (Either String a))

runTcBase :: TcType -> TcBase a -> IO (Either String a)
runTcBase typ (TcBase f) = do
  initU <- initUniq
  f emptyTM typ initU id
  

instance Monad TcBase where
  return x = TcBase $ \_ _ _ _ -> return $ Right x
  TcBase f >>= k = TcBase $ \tm ty u ec -> do
                        esa <- f tm ty u ec
                        case esa of
                          Left err -> return $ Left err
                          Right a -> do
                              let TcBase g = k a
                              g tm ty u ec
  fail err = TcBase $ \_ _ _ ec -> return (Left $ ec err)

instance Functor TcBase where
  fmap = liftM

-----------------------------------------
-- Core combinators of the TcBase monad

getUniqRef :: TcBase Uniq
getUniqRef = TcBase $ \_ _ u _ -> return $ Right u

getTypeMap :: TcBase TypeMap
getTypeMap = TcBase $ \tm _ _ _ -> return $ Right tm

getThisT :: TcBase TcType
getThisT = TcBase $ \_ ty _ _ -> return $ Right ty

withTypeMap :: (TypeMap -> TypeMap) -> TcBase a -> TcBase a
withTypeMap k (TcBase f) = TcBase $ f . k

liftIO :: IO a -> TcBase a
liftIO ioa = TcBase $ \_ _ _ _ -> do liftM Right ioa

withErrCtxt :: String -> TcBase a -> TcBase a
--withErrCtxt str (TcBase f) = 
--    TcBase $ \tm ty u ec -> f tm ty u (ec . (str ++))
withErrCtxt str = withErrCtxt' (. (str ++))

getErrCtxt :: TcBase (String -> String)
getErrCtxt = TcBase $ \_ _ _ ec -> return $ Right ec

withErrCtxt' :: ((String -> String) -> (String -> String)) -> TcBase a -> TcBase a
withErrCtxt' strff (TcBase f) = TcBase $ \tm ty u ec -> f tm ty u (strff ec)