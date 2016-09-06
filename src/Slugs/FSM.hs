{-# LANGUAGE RecordWildCards #-}

module Slugs.FSM (
    fromSlugs, fromSlugs',
    FSM(..), StateVars,
    Node(..),
    VarInfo(..),
    VType(..),
    Value(..)
  ) where

import           Panic (panic,HasCallStack)
import           Scope.Name (Name,mkName,emptySupply,Origin(..))
import           Slugs.Env
import           TypeCheck.AST (Controller(..),EnumDef(..),StateVar(..),Type(..))

import           Control.Monad (guard)
import           Data.Either (partitionEithers)
import           Data.List (mapAccumL,find,break,group)
import qualified Data.Text.Lazy as L
import qualified Data.Map.Strict as Map
import qualified Language.Slugs as Slugs
import           Text.Location (Range(..))
import           Text.ParserCombinators.ReadP
import           System.FilePath (dropExtension)


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


-- FSM from Specification ------------------------------------------------------

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
  nodes = Map.mapMaybe (mkNode True nodes env inpVars outVars) fsmNodes

  inpVars = [ (svName sv, mkVarInfo env sv bs) | (sv,bs) <- inps ]
  outVars = [ (svName sv, mkVarInfo env sv bs) | (sv,bs) <- outs ]

  -- determine how many bits are used for each named variable
  varGroups =
    [ (head g, length g) | g <- group (map sanitizeVarStr fsmStateDescr) ]

  -- translate the state variables from slugs back to names from the original
  -- specification, and partition into inputs/outputs
  (inps,outs) =
    partitionEithers [ getStateVar env cont str a | (str,a) <- varGroups ]


mkVarInfo :: HasCallStack => Env -> StateVar -> Int -> VarInfo
mkVarInfo env StateVar { .. } viBits =
  VarInfo { viType = case (svType, svBounds) of
                       (TBool,_)         -> VTBool
                       (TEnum n,_)       -> VTEnum (lookupEnum n env)
                       (TInt,Just (l,h)) -> VTInt l h
                       _                 -> panic ("mkVarInfo: Invalid type: " ++ show svType)
          , .. }


-- | Lookup the state var from the input controller. The result is Left when the
-- state var was an input, and Right when it was an output.
getStateVar :: HasCallStack => Env -> Controller -> String -> a -> Either (StateVar, a) (StateVar, a)
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
mkNode :: Bool -> Map.Map Int a -> Env -> [(Name,VarInfo)] -> [(Name,VarInfo)]
       -> Slugs.Node -> Maybe Node
mkNode adjustBounds keys env inps outs = \ Slugs.Node { .. } ->
  do let (bits',inAssigns) = mapAccumL (decodeValue adjustBounds env) nState inps
         (_,outAssigns)    = mapAccumL (decodeValue adjustBounds env) bits'  outs

     inps' <- sequence inAssigns
     outs' <- sequence outAssigns

     return Node { nodeInputs  = Map.fromList (zip inVars  inps')
                 , nodeOutputs = Map.fromList (zip outVars outs')
                 , nodeTrans   = filter (`Map.member` keys) nTrans }

  where
  inVars  = map fst inps
  outVars = map fst outs


-- | Decode a number that's been encoded as a list of bits.
decodeValue :: HasCallStack => Bool -> Env -> [Int] -> (Name,VarInfo) -> ([Int],Maybe Value)
decodeValue adjustBounds env bits (n,VarInfo { .. }) = (rest,e)
  where
  (used,rest) = splitAt viBits bits

  e = case (viType,used) of
        (VTBool, [b]) ->
             return (VBool (b == 1))

        (VTEnum EnumDef { .. }, bs) ->
          do let ix = decodeNum bs
             guard (ix < length eCons)
             return (VCon (eCons !! ix))

        (VTInt l _, bs)
          | adjustBounds ->
            do let lower = lowerBound' (toInteger l) n env
               return (VNum (toInteger (decodeNum bs) + lower))

          | otherwise ->
               return (VNum (toInteger (decodeNum bs)))

        _ -> panic "Invalid state!"


-- | Given a little-endian list of ints in '[0,1]', decode a number
decodeNum :: [Int] -> Int
decodeNum  = foldr (\b acc -> acc * 2 + b) 0


-- FSM from Controller Only ----------------------------------------------------

-- | Generate an FSM from slugs output, when no controller was used for input.
fromSlugs' :: FilePath -> Int -> Slugs.FSM -> FSM
fromSlugs' file numInputs Slugs.FSM { .. } =
  FSM { fsmName    = cName
      , fsmInputs  = Map.fromList inps
      , fsmOutputs = Map.fromList outs
      , fsmEnums   = []
      , fsmNodes   = nodes
      }

  where

  nodes     = Map.mapMaybe (mkNode True nodes emptyEnv inps outs) fsmNodes

  origin    = FromController mempty { rangeSource = Just file }

  (cName,s1) = mkName origin (L.pack (dropExtension file)) emptySupply

  (inps,outs) = splitAt numInputs (concat vars)


  (_, vars) = mapAccumL parseVar (s1,Nothing) fsmStateDescr

  parseVar (s,Nothing) var

    -- when the bit-spec is empty, we treat the variable as a boolean
    | null bitSpec = ((s',Nothing), [mkVar name Nothing])

    -- when the bit-spec is present, we figure out how many variables will be
    -- involved, and initialize the parsing state
    | otherwise = state `seq` ((s',Just state), [])

    where

    (namePart,bitSpec) = break (== '@') var

    (name,s') = mkName origin (L.pack namePart) s

    state = (name,lo,hi)

    (lo,hi) =
      case readP_to_S parseState (drop 1 bitSpec) of
        [(x,"")] -> x
        _        -> panic ("Failed to parse bounds for: " ++ namePart)

  parseVar (s,st@(Just (name,lo,hi))) var

    | null bitSpec || newVar =
      let (st',vs) = parseVar (s,Nothing) var
       in (st',mkVar name (Just (lo,hi)) : vs)

    | otherwise    = ((s,st),[])

    where
    (_,bitSpec) = break (== '@') var

    -- if '.' is present in the bitSpec, that means that it's the definition of
    -- a new variable.
    newVar = '.' `elem` bitSpec

  mkVar n Nothing        = (n, VarInfo { viType = VTBool,      viBits = 1 })
  mkVar n (Just (lo,hi)) = (n, VarInfo { viType = VTInt lo hi, viBits = Slugs.numBits hi })


parseState :: ReadP (Int,Int)
parseState  =
  do lo <- readS_to_P read
     _  <- string "..."
     hi <- readS_to_P read
     return (lo,hi)
