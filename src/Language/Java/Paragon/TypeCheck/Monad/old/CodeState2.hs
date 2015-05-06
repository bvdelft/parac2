module Language.Java.Paragon.TypeCheck.Monad.CodeState
    (
     module Language.Java.Paragon.TypeCheck.Monad.CodeState,
     PolicyBounds(..), ActorPolicy, ActorPolicyBounds
    ) where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Pretty
import Language.Java.Paragon.Interaction

import Language.Java.Paragon.TypeCheck.Policy
import Language.Java.Paragon.TypeCheck.Actors
import Language.Java.Paragon.TypeCheck.Locks
import Language.Java.Paragon.TypeCheck.Types

import Language.Java.Paragon.Monad.Uniq

import qualified Data.Map as Map
import Data.List (intersect, union)
import Data.Maybe (fromJust)
import Control.Monad (zipWithM, when)

codeStateModule :: String
codeStateModule = typeCheckerBase ++ ".Monad.CodeState"

data VarMap = VarMap {
      actorSt    :: ActorMap,
      policySt   :: PolicyMap,
      instanceSt :: InstanceMap,
      typesSt    :: VarMap
    }

data CodeState = CodeState {
      varMap     :: VarMap,
      lockMods   :: LockMods,
      exnS       :: ExnsMap
    }
  deriving (Eq, Show)

type InstanceMap = Map.Map (Ident ()) InstanceSig

data InstanceSig = ISig {
      iType :: TcType,
      iImplActorArgs :: [ActorId],
      iMembers :: VarMap
    }
  deriving (Show, Eq)

type PolicyMap = Map.Map (Ident ()) ActorPolicyBounds

type ActorMap = Map.Map (Ident ()) ActorInfo

data ActorInfo = AI { aID :: ActorId, stability :: Stability }
  deriving (Eq, Show)

data Stability = Stable | FullV | FieldV (Maybe (Name ()))
  deriving (Eq, Show)

------------------------------------------
-- Merging of states in parallel
------------------------------------------

mergeVarMaps :: VarMap -> VarMap -> BaseM VarMap
mergeVarMaps (VarMap as1 ps1 is1 ts1) (VarMap as2 ps2 is2 ts2) = do
  newActors <- mergeActors    as1 as2
  newPols   <- mergePolicies  ps1 ps2
  newInsts  <- mergeInstances is1 is2
  newTypes  <- mergeVarMaps   ts1 ts2
  return $ VarMap newActors newPols newInsts newTypes

--mergeStates :: Uniq -> CodeState -> CodeState -> IO CodeState
mergeStates :: CodeState -> CodeState -> BaseM CodeState
mergeStates (CodeState vm1 ls1 es1) (CodeState vm2 ls2 es2) = do
  newVm    <- mergeVarMaps vm1 vm2
  newExns  <- mergeExns    es1 es2
  return $ CodeState {
               varMap   = newVm,
               lockMods = ls1 <++> ls s2,
               exnS     = newExns
             }

------------------------------------------
-- Instance tracking
------------------------------------------

mergeInstances :: InstanceMap -> InstanceMap -> BaseM InstanceMap
mergeInstances im1 im2 = do
    let newKeys = Map.keys im1 `intersect` Map.keys im2
        oldVals = map (\k -> (fromJust (Map.lookup k im1), fromJust (Map.lookup k im2))) newKeys
    newVals <- mapM mergeISigs oldVals
    return $ Map.fromList $ zip newKeys newVals
        where mergeISigs :: (InstanceSig, InstanceSig) -> IO InstanceSig
              mergeISigs (is1, is2) = do
                             when (iType is1 /= iType is2) $
                                  panic (codeStateModule ++ ".mergeInstances:mergeISigs")
                                      $ show (is1, is2)
                             as <- mergeIas (iImplActorArgs is1) (iImplActorArgs is2)
                             newMems <- mergeVarMaps (iMembers is1) (iMembers is2)
                             return $ ISig (iType is1) as newMems

              mergeIas :: [ActorId] -> [ActorId] -> IO [ActorId]
              mergeIas ias1 ias2 = zipWithM mergeIa ias1 ias2
              
              mergeIa :: ActorId -> ActorId -> IO ActorId
              mergeIa ai1 ai2 
                  | ai1 == ai2 = return ai1
              mergeIa (Instance n _) _ = newInstance n
              mergeIa ai _ = panic (codeStateModule ++ ".mergeIas")
                             $ "Instance has non-instance implicit argument: " ++ show ai

{-
mergeInstances :: Uniq -> InstanceMap -> InstanceMap -> IO InstanceMap
mergeInstances u im1 im2 = do
    let newKeys = Map.keys im1 `intersect` Map.keys im2
        oldVals = map (\k -> (fromJust (Map.lookup k im1), fromJust (Map.lookup k im2))) newKeys
    newVals <- mapM (uncurry mergeIas) oldVals
    return $ Map.fromList $ zip newKeys newVals
        where mergeIas :: [ActorId] -> [ActorId] -> IO [ActorId]
              mergeIas ias1 ias2 = zipWithM mergeIa ias1 ias2
              
              mergeIa :: ActorId -> ActorId -> IO ActorId
              mergeIa ai1 ai2 
                  | ai1 == ai2 = return ai1
              mergeIa (Instance n _) _ = newInstance u n
              mergeIa ai _ = panic (codeStateModule ++ ".mergeIas")
                             $ "Instance has non-instance implicit argument: " ++ show ai
-}

------------------------------------------
-- Policy tracking
------------------------------------------

type PolicyMap = Map.Map (Name ()) ActorPolicyBounds

mergePolicies :: PolicyMap -> PolicyMap -> IO PolicyMap
mergePolicies pm1 pm2 = do
    let newKeys = Map.keys pm1 `intersect` Map.keys pm2
        oldVals = map (\k -> (fromJust (Map.lookup k pm1), fromJust (Map.lookup k pm2))) newKeys
        newVals = map mergePs oldVals
    return $ Map.fromList $ zip newKeys newVals

mergePs :: (ActorPolicyBounds, ActorPolicyBounds) -> ActorPolicyBounds
mergePs pis =
    case pis of
      (KnownPolicy p, KnownPolicy q)
          | p == q    -> KnownPolicy p
          | otherwise ->  mkBounds p q p q
      (PolicyBounds p1 p2, PolicyBounds q1 q2) -> mkBounds p1 q1 p2 q2
      (KnownPolicy  p,     PolicyBounds q1 q2) -> mkBounds p  q1 p  q2
      (PolicyBounds p1 p2, KnownPolicy  q    ) -> mkBounds p1 q  p2 q

  where mkBounds a b c d = PolicyBounds (a `meet` b) (c `join` d)

------------------------------------------
-- Actor analysis
------------------------------------------

type ActorMap = Map.Map (Name ()) ActorInfo

data ActorInfo = AI { aID :: ActorId, stability :: Stability }
  deriving (Eq, Show)

data Stability = Stable | FullV | FieldV (Maybe (Name ()))
  deriving (Eq, Show)

scrambles :: Stability -> Stability -> Bool
scrambles FullV (FieldV _) = True
scrambles x y = x == y

scramble :: Uniq -> Stability -> CodeState -> IO CodeState
scramble u stab state = do
    let acts = Map.toAscList $ actorSt state
    newActs <- mapM (\(k,v) -> scramble' u stab v >>= \v' -> return (k, v')) acts
    return state { actorSt = Map.fromAscList newActs }

scramble' :: Uniq -> Stability -> ActorInfo -> IO ActorInfo
scramble' u stab a@(AI _ stab') =
    if scrambles stab stab' 
     then do aid' <- newUnknown u
             return $ AI aid' stab'
     else return a
             

mergeActors :: Uniq -> ActorMap -> ActorMap -> IO ActorMap
mergeActors u a1 a2 = do
    let newKeys = Map.keys a1 `intersect` Map.keys a2
        oldVals = map (\k -> (fromJust (Map.lookup k a1), fromJust (Map.lookup k a2))) newKeys
    newVals <- mapM (mergeInfo u) oldVals
    return $ Map.fromList $ zip newKeys newVals

mergeInfo :: Uniq -> (ActorInfo, ActorInfo) -> IO ActorInfo
mergeInfo _ (ai1,ai2) | ai1 == ai2 = return ai1
mergeInfo u ((AI _ st),_) = do
  aid <- newUnknown u
  return $ AI aid st

------------------------------------------
-- Exception states
------------------------------------------

type ExnsMap  = Map.Map ExnType ExnPoint
data ExnType  = ExnType TcType | ExnContinue | ExnBreak | ExnReturn
  deriving (Eq, Ord, Show)
data ExnPoint = ExnPoint { epState :: CodeState, epWrite :: (TcPolicy TcActor) }
  deriving (Eq, Show)



mergeExns :: Uniq -> ExnsMap -> ExnsMap -> IO ExnsMap
mergeExns u em1 em2 = do
    let newKeys = Map.keys em1 `union` Map.keys em2
        oldVals = map (\k -> (Map.lookup k em1, Map.lookup k em2)) newKeys
    newVals <-mapM (uncurry (mergePoints u)) oldVals
    return $ Map.fromList $ zip newKeys newVals

-- Invariant: At most one of the two arguments can be Nothing
mergePoints :: Uniq -> Maybe ExnPoint -> Maybe ExnPoint -> IO ExnPoint
mergePoints _ Nothing (Just e) = return e
mergePoints _ (Just e) Nothing = return e
mergePoints u (Just (ExnPoint st1 w1)) (Just (ExnPoint st2 w2)) = do
  st <- mergeStates u st1 st2
  let w = w1 `join` w2
  return (ExnPoint st w)
mergePoints _ _ _ = panic (codeStateModule ++ ".mergePoints")
                    "Both ExnPoint arguments cannot be missing!"

-- This should probably be pre-computed each time the map is updated instead
exnPC :: CodeState -> [((TcPolicy TcActor), String)]
exnPC s = map (\(tyX,ptX) -> (epWrite ptX, errorSrc tyX)) $ Map.assocs $ exnS s

errorSrc :: ExnType -> String
errorSrc et = "area of influence of " ++
    case et of
      ExnBreak -> "a break statement"
      ExnContinue -> "a continue statement"
      ExnReturn -> "a return statement"
      ExnType tX -> "exception " ++ prettyPrint tX
