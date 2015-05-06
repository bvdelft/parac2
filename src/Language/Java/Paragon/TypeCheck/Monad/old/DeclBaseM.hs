module Language.Java.Paragon.TypeCheck.Monad.DeclBaseM where

import Language.Java.Paragon.Monad.PiReader

import Language.Java.Paragon.TypeCheck.TypeMap
import Language.Java.Paragon.TypeCheck.Types


import Control.Monad
import Control.Applicative

newtype DeclBaseM a = DeclBaseM (TypeMap -> TcType -> PiReader a)

instance Monad DeclBaseM where
  return = liftPR . return

  DeclBaseM f >>= k = DeclBaseM $ \tm ty -> do
                       a <- f tm ty
                       let DeclBaseM g = k a
                       g tm ty

  fail = liftPR . fail

instance Functor DeclBaseM where
  fmap = liftM

liftPR :: PiReader a -> DeclBaseM a
liftPR pra = DeclBaseM $ \_ _ -> pra

liftPRWith :: (PiReader a -> PiReader a) -> DeclBaseM a -> DeclBaseM a
liftPRWith praf (DeclBaseM f) = DeclBaseM $ \tm ty -> praf $ f tm ty

getTypeMap :: DeclBaseM TypeMap
getTypeMap = DeclBaseM $ \tm _ -> return tm

getThisType :: DeclBaseM TcType
getThisType = DeclBaseM $ \_ ty -> return ty
