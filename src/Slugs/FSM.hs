{-# LANGUAGE RecordWildCards #-}

module Slugs.FSM (
    fromSlugs,
    FSM(..),
    Node(..),
  ) where

import           Scope.Name (Name)
import           Slugs.Env
import           TypeCheck.AST (Controller(..),EnumDef(..),StateVar(..),Type(..))

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

data FSM = FSM { fsmInputs  :: StateVars
               , fsmOutputs :: StateVars
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
  FSM { fsmInputs  = Map.fromList inpVars
      , fsmOutputs = Map.fromList outVars
      , fsmNodes   = Map.map (mkNode env inpVars outVars) fsmNodes
      }

  where

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
mkNode :: Env -> [(Name,VarInfo)] -> [(Name,VarInfo)] -> Slugs.Node -> Node
mkNode env inps outs = \ Slugs.Node { .. } ->
  let (bits',inAssigns) = mapAccumL (decodeValue env) nState inps
      (_,outAssigns)    = mapAccumL (decodeValue env) bits'  outs

   in Node { nodeInputs  = Map.fromList (zip inVars inAssigns)
           , nodeOutputs = Map.fromList (zip outVars outAssigns)
           , nodeTrans   = nTrans }

  where
  inVars  = map fst inps
  outVars = map fst outs


-- | Decode a number that's been encoded as a list of bits.
decodeValue :: Env -> [Int] -> (Name,VarInfo) -> ([Int],Value)
decodeValue env bits (n,VarInfo { .. }) = (rest,e)
  where
  (bs,rest) = splitAt viBits bits

  e = case (viType,bs) of
        (VTBool, [b])               -> VBool (b == 1)
        (VTEnum EnumDef { .. }, bs) -> VCon (eCons !! decodeNum bs)
        (VTInt _ _, bs)             -> VNum (toInteger (decodeNum bs) + lowerBound n env)


-- | Given a little-endian list of ints in '[0,1]', decode a number
decodeNum :: [Int] -> Int
decodeNum  = foldr (\b acc -> acc * 2 + b) 0
