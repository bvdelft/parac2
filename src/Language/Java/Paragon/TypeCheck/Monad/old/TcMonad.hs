module Language.Java.Paragon.TypeCheck.Monad.TcMonad where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Interaction

--import Language.Java.Paragon.TypeCheck.Monad.TcBase
import Language.Java.Paragon.TypeCheck.Monad.TcCont
import Language.Java.Paragon.TypeCheck.TcEnv
import Language.Java.Paragon.TypeCheck.TcState
import Language.Java.Paragon.TypeCheck.Constraints hiding (Map)
import Language.Java.Paragon.TypeCheck.Actors
import Language.Java.Paragon.TypeCheck.Locks (noMods)

import Control.Monad (liftM)         -- liftM only to instantiate fmap

import qualified Data.Map as Map

tcMonadModule :: String
tcMonadModule = typeCheckerBase ++ ".Monad.TcMonad"

------------------------------------------------
--               The Tc monad                 --
------------------------------------------------
--
-- A monad on top of TcCont for typechecking
-- on the level of method declarations.
-- Has a method environment, a mergeable state, 
-- and a writer for constraints

newtype Tc r a =
    Tc (TcEnv -> TcState -> TcCont r (a, TcState, [ConstraintWMsg]))

runTc :: TcEnv -> TcState -> Tc r a -> TcCont r (a, [ConstraintWMsg])
runTc env state (Tc f) = do
  (a,_,cs) <- f env state
  return (a, cs)

-------------------------------------
setupStartState :: TcCont r TcState
setupStartState = do
  tm <- getTypeMap
  let aMap = gatherActorInfo tm
  return $ TcState aMap noMods Map.empty


gatherActorInfo :: TypeMap -> Map (Name ()) ActorInfo
gatherActorInfo = gatherActorInfo' Nothing

    where gatherActorInfo' mPre tm =
              let acts = Map.assocs $ actors tm -- :: [(Ident, ActorId)]
                  aMap = Map.fromList $ map (mkInfo mPre $ fields tm) acts
                  tMap = gatherActorInfoAux TName mPre 
                           (Map.assocs $ Map.map (tMembers . snd) $ types tm)
                  pMap = gatherActorInfoAux PName mPre (Map.assocs $ packages tm)
              in foldl1 Map.union [aMap, tMap, pMap]

          mkStab :: VarFieldSig -> Stability
          mkStab (VSig _ _ _ _ final) = 
              if final then Stable else FieldV Nothing

          mkInfo :: Maybe (Name ())
                 -> Map (Ident ()) VarFieldSig 
                 -> (Ident (), ActorId) 
                 -> (Name (), ActorInfo)
          mkInfo mPre fs (i,aid) = 
              case Map.lookup i fs of
                Just vti -> (Name () EName mPre i, AI aid (mkStab vti))
                _ -> panic (tcMonadModule ++ ".gatherActorInfo") $ 
                     "No field for corresponding actor " ++ show i

          gatherActorInfoAux :: NameType 
                             -> Maybe (Name ()) 
                             -> [(Ident (), TypeMap)] 
                             -> Map (Name ()) ActorInfo
          gatherActorInfoAux nt mPre = foldl Map.union Map.empty . map aux
              where aux :: (Ident (), TypeMap) -> Map (Name ()) ActorInfo
                    aux (i,tm) = 
                        let pre = Name () nt mPre i
                        in gatherActorInfo' (Just pre) tm

{-
gatherActorInfo = Map.mapKeysMonotonic (Name ()) . gatherActorInfoAux
  where
    gatherActorInfoAux :: TypeMap -> Map [Ident ()] ActorInfo
    gatherActorInfoAux tm = 
        --trace ("TRACE: " ++ show tm) $
        let acts = Map.assocs $ actors tm -- :: [(Ident, ActorId)]
            aMap = Map.fromList $ map (mkInfo $ fields tm) acts
            tMap = gatherActorInfoT (Map.assocs $ pkgsAndTypes tm)
        in Map.union aMap tMap

            where mkStab :: VarFieldSig -> Stability
                  mkStab (VSig _ _ _ final) = 
                       if final then Stable else FieldV Nothing
                
                  mkInfo :: Map (Ident ()) VarFieldSig -> (Ident (), ActorId) -> ([Ident ()], ActorInfo)
                  mkInfo fs (i,aid) = 
                    case Map.lookup i fs of
                      Just vti -> ([i], AI aid (mkStab vti))
                      _ -> error $ "Internal error: no field for corresponding actor " ++ show i

    gatherActorInfoT :: [(Ident (), TypeMap)] -> Map [Ident ()] ActorInfo
    gatherActorInfoT = foldl Map.union Map.empty . map aux
        where aux :: (Ident (), TypeMap) -> Map [Ident ()] ActorInfo
              aux (i,tm) = let aMap = gatherActorInfoAux tm
                           in Map.mapKeysMonotonic (i:) $ Map.map (extendT i) aMap
              extendT :: Ident () -> ActorInfo -> ActorInfo
              extendT i (AI aid (FieldV (Name x is))) = AI aid (FieldV (Name x $ i:is))
              extendT _ a = a
-}      
-------------------------------------


instance Monad (Tc r) where
  return x = Tc $ \_ s -> return (x, s, [])
  Tc f >>= k = Tc $ \e s0 -> do
                     (a, s1, cs1) <- f e s0
                     let Tc g = k a
                     (b, s2, cs2) <- g e s1
                     return (b, s2, cs1 ++ cs2)
  fail err = Tc $ \_ _ -> fail err


instance Functor (Tc r) where
  fmap = liftM

liftCont :: TcCont r a -> Tc r a
liftCont tcba = Tc $ \_ s -> do
                  a <- tcba
                  return (a, s, [])

withErrCtxtTc :: String -> Tc r a -> Tc r a
withErrCtxtTc str (Tc f) = Tc $ \e s -> withErrCtxt str (f e s)
                             


-- Running in parallel
infix 1 |||
(|||) :: Tc r a -> Tc r b -> Tc r (a,b)
(Tc f1) ||| (Tc f2) = 
    Tc $ \te ts -> do
      (a, ts1, cs1) <- f1 te ts
      (b, ts2, cs2) <- f2 te ts
      ts' <- mergeStatesCont ts1 ts2
      return $ ((a,b), ts', cs1 ++ cs2)

mergeStatesCont :: TcState -> TcState -> TcCont r TcState
mergeStatesCont s1 s2 = do
  u <- getUniqRef
  liftIO $ mergeStates u s1 s2

-- The environment

getEnv :: Tc r TcEnv
getEnv = Tc (\e s -> return (e,s,[]))

withEnv :: (TcEnv -> TcEnv) -> Tc r a -> Tc r a
withEnv k (Tc f) = Tc (f . k)

-- The state

getState :: Tc r TcState
getState = Tc (\_ s -> return (s,s,[]))

setState :: TcState -> Tc r ()
setState s = Tc (\_ _ -> return ((),s,[]))

updateState :: (TcState -> TcState) -> Tc r ()
updateState f = getState >>= return . f >>= setState

mergeWithState :: TcState -> Tc r ()
mergeWithState s = do
  sOld <- getState
  sNew <- liftCont $ mergeStatesCont sOld s
  setState sNew

-- Constraints

addConstraint :: Constraint -> String -> Tc r ()
addConstraint c str = Tc (\_ s -> return ((), s, [(c,str)]))

