{-# LANGUAGE RecordWildCards #-}

module Slugs.FSM (
    fromSlugs,
    FSM(..), StateVars,
    Node(..),
    VarInfo(..),
    VType(..),
    Value(..)
  ) where

import           Scope.Name (Name)
import           Slugs.Env
import           TypeCheck.AST (Controller(..),EnumDef(..),StateVar(..),Type(..))

import           Control.Monad (guard)
import           Data.Either (partitionEithers)
import           Data.List (mapAccumL,find,break,group)
import qualified Data.Map.Strict as Map
import qualified Language.Slugs as Slugs


panic :: String -> a
panic msg = error ("PANIC: " ++ msg)

type StateVars = Map.Map Name VarInfo

data VarInfo = VarInfo { viType :: !VType
                       , viBits :: !Int
                       } deriving (Show)

data FSM = FSM { fsmName    :: Name
               , fsmInputs  :: StateVars
               , fsmOutputs :: StateVars
               , fsmEnums   :: [EnumDef]
               , fsmNodes   :: Map.Map Int Node
               } deriving (Show)

data Node = Node { nodeInputs  :: Map.Map Name Value
                 , nodeOutputs :: Map.Map Name Value
                 , nodeTrans   :: [Int]
                 } deriving (Show)

data VType = VTBool
           | VTInt Int Int
           | VTEnum EnumDef
             deriving (Show)

data Value = VBool Bool
           | VCon Name
           | VNum Integer
             deriving (Show)

-- | Translate from the slugs state machine to one that is easier to generate
-- code for.
fromSlugs :: Env -> Controller -> Slugs.FSM -> FSM
fromSlugs env cont Slugs.FSM { .. } =
  FSM { fsmName    = cName cont
      , fsmInputs  = Map.fromList inpVars
      , fsmOutputs = Map.fromList outVars
      , fsmEnums   = cEnums cont
      , fsmNodes   = nodes
      }

  where

  -- NOTE: nodes is defined recursively, as the transitions are pruned when they
  -- are guarded by invalid bit patterns. Passing nodes in, and defining the
  -- transition lazily means that we can filter the transitions based on the
  -- nodes that ended up in the final FSM.
  nodes = Map.mapMaybe (mkNode nodes env inpVars outVars) fsmNodes

  inpVars = [ (svName sv, mkVarInfo env sv bs) | (sv,bs) <- inps ]
  outVars = [ (svName sv, mkVarInfo env sv bs) | (sv,bs) <- outs ]

  -- determine how many bits are used for each named variable
  varGroups =
    [ (head g, length g) | g <- group (map sanitizeVarStr fsmStateDescr) ]

  -- translate the state variables from slugs back to names from the original
  -- specification, and partition into inputs/outputs
  (inps,outs) =
    partitionEithers [ getStateVar env cont str a | (str,a) <- varGroups ]


mkVarInfo :: Env -> StateVar -> Int -> VarInfo
mkVarInfo env StateVar { .. } viBits =
  VarInfo { viType = case (svType, svBounds) of
                       (TBool,_)         -> VTBool
                       (TEnum n,_)       -> VTEnum (lookupEnum n env)
                       (TInt,Just (l,h)) -> VTInt l h
                       _                 -> panic ("mkVarInfo: Invalid type: " ++ show svType)
          , .. }


-- | Lookup the state var from the input controller. The result is Left when the
-- state var was an input, and Right when it was an output.
getStateVar :: Env -> Controller -> String -> a -> Either (StateVar, a) (StateVar, a)
getStateVar env cont str a =
  case find svDef (cInputs cont) of
    Just sv -> Left (sv,a)
    Nothing ->
      case find svDef (cOutputs cont) of
        Just sv -> Right (sv,a)
        Nothing -> panic ("getStateVar: var from slugs missing from spec: " ++ str)

  where

  n = lookupVarName str env

  svDef sv = svName sv == n


-- | Strip out information about bit-vector indexing.
sanitizeVarStr :: String -> String
sanitizeVarStr str = fst (break (== '@') str)


-- | Decompose the state into the input requirements, and output changes.
mkNode :: Map.Map Int a -> Env -> [(Name,VarInfo)] -> [(Name,VarInfo)]
       -> Slugs.Node -> Maybe Node
mkNode keys env inps outs = \ Slugs.Node { .. } ->
  do let (bits',inAssigns) = mapAccumL (decodeValue env) nState inps
         (_,outAssigns)    = mapAccumL (decodeValue env) bits'  outs

     inps' <- sequence inAssigns
     outs' <- sequence outAssigns

     return Node { nodeInputs  = Map.fromList (zip inVars  inps')
                 , nodeOutputs = Map.fromList (zip outVars outs')
                 , nodeTrans   = filter (`Map.member` keys) nTrans }

  where
  inVars  = map fst inps
  outVars = map fst outs


-- | Decode a number that's been encoded as a list of bits.
decodeValue :: Env -> [Int] -> (Name,VarInfo) -> ([Int],Maybe Value)
decodeValue env bits (n,VarInfo { .. }) = (rest,e)
  where
  (used,rest) = splitAt viBits bits

  e = case (viType,used) of
        (VTBool, [b]) ->
             return (VBool (b == 1))

        (VTEnum EnumDef { .. }, bs) ->
          do let ix = decodeNum bs
             guard (ix < length eCons)
             return (VCon (eCons !! ix))

        (VTInt _ _, bs) ->
             return (VNum (toInteger (decodeNum bs) + lowerBound n env))

        _ -> panic "Invalid state!"


-- | Given a little-endian list of ints in '[0,1]', decode a number
decodeNum :: [Int] -> Int
decodeNum  = foldr (\b acc -> acc * 2 + b) 0
