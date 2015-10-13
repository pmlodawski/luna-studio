{-# LANGUAGE OverloadedStrings #-}

module Utils.MockHelper where

import           Utils.PreludePlus
import           Control.Monad

import           Data.Char         (isDigit)
import           Data.List         (all)
import           Data.Maybe        (fromJust, fromMaybe)
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import           Object.Object (PortId(..))
import           Object.Port


data PortConstraint = PortConstraint { _portNums  :: [PortId]
                                     , _portsType :: ValueType
                                     } deriving (Eq, Show)
makeLenses ''PortConstraint


data NodeType = NodeType { _portCount       :: Int
                         , _portConstraints :: [PortConstraint]
                         } deriving (Eq, Show)
makeLenses ''NodeType


portsMaxin  = 9
portsMaxOut = 9


getInputPortsNr  expr = (ord (head expr) - ord '1' + 1) `mod` (portsMaxin + 1)
getOutputPortsNr expr = 1 + (ord (fromMaybe '1' $ listToMaybe (tail expr)) - ord '1') `mod` portsMaxOut


tryVal :: Text -> Maybe NodeType
tryVal expr = Just . constant . readType $ expr


tryDef :: Text -> Maybe NodeType
tryDef expr = case Text.unpack $ Text.take 4 expr of
    "def " -> Just $ NodeType 0 []
    _      -> Nothing


tryFormat :: Text -> Maybe NodeType
tryFormat expr = case head suf of
    '#' -> Just $ NodeType inputPortsNum []
    _   -> Nothing
    where
    suf            = Text.unpack $ Text.takeEnd 3 expr
    lastTwo        = Text.unpack $ Text.takeEnd 2 expr
    inputPortsNum  = getInputPortsNr lastTwo
    outputPortsNum = getOutputPortsNr lastTwo


tryKnown :: Text -> Maybe NodeType
tryKnown expr = lookup expr knownFunctions


knownFunctions :: [(Text, NodeType)]
knownFunctions  = [ ("+",        twoOpNum  )
                  , ("-",        twoOpNum  )
                  , ("*",        twoOpNum  )
                  , ("/",        twoOpNum  )
                  , (">",        twoOpLogic)
                  , ("==",       twoOpLogic)
                  , ("<",        twoOpLogic)
                  , ("<=",       twoOpLogic)
                  , (">=",       twoOpLogic)
                  , ("toString", NodeType 1 [PortConstraint [AllPorts] VTString, PortConstraint [PortNum 1] VTAny])
                  , ("truncate", oneOpNum)
                  , ("round",    oneOpNum)
                  , ("floor",    oneOpNum)
                  , ("ceiling",  oneOpNum)
                  , ("sampleData",NodeType 1 [PortConstraint [AllPorts]  (VTVector VTNumeric), PortConstraint [PortNum 0] VTObject])
                  , ("sampleInt", NodeType 1 [PortConstraint [AllPorts]  (VTVector VTNumeric), PortConstraint [PortNum 0] VTObject])
                  , ("sort",      NodeType 1 [PortConstraint [PortNum 0, AllPorts] (VTVector VTNumeric)])
                  , ("length",    NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts]               VTInt,                PortConstraint [PortNum 1] (VTVector VTAny)]                                )
                  , ("sum",       NodeType 1 [PortConstraint [AllPorts]  VTNumeric,PortConstraint [PortNum 0] (VTVector VTNumeric)])
                  , ("null",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts]               VTBool,               PortConstraint [PortNum 1] (VTVector VTAny)]                                )
                  , ("!",         NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts]               VTNumeric,            PortConstraint [PortNum 1] (VTVector VTNumeric), PortConstraint [PortNum 2] (VTInt)])
                  , ("head",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts]               VTNumeric,            PortConstraint [PortNum 1] (VTVector VTNumeric)                            ])
                  , ("last",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts]               VTNumeric,            PortConstraint [PortNum 1] (VTVector VTNumeric)                            ])
                  , ("init",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1]    (VTVector VTNumeric)                                                                     ]) 
                  , ("tail",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1]    (VTVector VTNumeric)                                                                     ]) 
                  , ("take",      NodeType 2 [PortConstraint [AllPorts, PortNum 0] (VTVector VTNumeric), PortConstraint [PortNum 1] VTInt])
                  , ("drop",      NodeType 2 [PortConstraint [AllPorts, PortNum 0] (VTVector VTNumeric), PortConstraint [PortNum 1] VTInt])
                  , ("empty",     constant   (VTVector VTNumeric))
                  , ("singleton", NodeType 2 [PortConstraint [AllPorts]  (VTVector VTNumeric), PortConstraint [PortNum 1] VTNumeric, PortConstraint [PortNum 0] VTObject])
                  , ("replicate", NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts]       (VTVector VTNumeric), PortConstraint [PortNum 1] VTInt, PortConstraint [PortNum 2] VTNumeric             ])
                  , ("cons",      NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1]    (VTVector VTNumeric), PortConstraint [PortNum 2] VTNumeric                                       ])
                  , ("snoc",      NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1]    (VTVector VTNumeric), PortConstraint [PortNum 2] VTNumeric                                       ])
                  , ("++",        NodeType 2 [PortConstraint [AllPorts, PortNum 0, PortNum 1] (VTVector VTNumeric)])
                  , ("reverse",   NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1]    (VTVector VTNumeric)                                                                     ])
                  , ("map",       NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1]    (VTVector VTNumeric)                                                                     ])
                  , ("filter",    NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1]    (VTVector VTNumeric)                                                                     ])
                  , ("maximum",   NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts] VTNumeric, PortConstraint [PortNum 1] (VTVector VTNumeric)])
                  , ("minimum",   NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts] VTNumeric, PortConstraint [PortNum 1] (VTVector VTNumeric)])
                  , ("cos",       NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("acos",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("cosh",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("asin",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("sinh",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("tan",       NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("atan",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("tanh",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("abs",       NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("signum",    NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("min",       NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1, PortNum 2] VTNumeric])
                  , ("max",       NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1, PortNum 2] VTNumeric])
                  , ("gcd",       NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1, PortNum 2] VTNumeric])
                  , ("lcm",       NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1, PortNum 2] VTNumeric])
                  , ("div",       NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1, PortNum 2] VTNumeric])
                  , ("mod",       NodeType 3 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1, PortNum 2] VTNumeric])
                  , ("pi",        NodeType 1 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts           ] VTNumeric])
                  , ("log,",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  , ("sqrt",      NodeType 2 [PortConstraint [PortNum 0] VTObject, PortConstraint [AllPorts, PortNum 1] VTNumeric])
                  ]



-- yes, we do the same computation twice -- that's a workaround right here, don't mind :)
getNodeType :: Text -> NodeType
getNodeType expr = case msum $ ($ expr) <$> [tryKnown, tryDef, tryFormat, tryVal] of
    Just portType -> portType
    Nothing       -> NodeType 0 []


getInputTypes :: NodeType -> [ValueType]
getInputTypes (NodeType pc constrs) = map inputType [0..(pc-1)]
    where containsPortId id pconstr = elem (PortNum id) (pconstr ^. portNums)
          inputTypeM id             = (view portsType) <$> find (containsPortId id) constrs
          inputType  id             = fromMaybe VTAny $ inputTypeM id


getOutputType :: NodeType -> [ValueType]
getOutputType (NodeType _ constrs) = [fromMaybe VTAny outputTypeM]
    where containsOut pconstr = elem AllPorts $ pconstr ^. portNums
          outputTypeM         = (view portsType) <$> find containsOut constrs


----------------------------------------------------------------------------------------
-- Type utils
----------------------------------------------------------------------------------------
typesEq :: ValueType -> ValueType -> Bool
typesEq VTObject       VTObject       = True
typesEq VTBool         VTBool         = True
typesEq VTChar         VTChar         = True
typesEq VTInt          VTInt          = True
typesEq VTFloat        VTFloat        = True
typesEq VTString       VTString       = True
typesEq (VTVector tp1) (VTVector tp2) = typesEq tp1 tp2
typesEq (VTMaybe  tp1) (VTMaybe  tp2) = typesEq tp1 tp2
typesEq VTAny          _              = True
typesEq _              VTAny          = True
typesEq VTNumeric      VTFloat        = True
typesEq VTNumeric      VTInt          = True
typesEq VTFloat        VTNumeric      = True
typesEq VTInt          VTNumeric      = True
typesEq _              _              = False


--             tp. of arg | promoted constraint
typePromote :: ValueType -> ValueType -> Maybe ValueType
typePromote VTInt          VTNumeric      = Just VTInt
typePromote VTFloat        VTNumeric      = Just VTFloat
typePromote tp             VTAny          = Just tp
typePromote (VTVector tp1) (VTVector tp2) = VTVector <$> typePromote tp1 tp2
typePromote (VTMaybe  tp1) (VTMaybe  tp2) = VTMaybe  <$> typePromote tp1 tp2
typePromote tp1             tp2           = if tp1 == tp2 then Just tp1 else Nothing


-- common port types:
constant :: ValueType -> NodeType
constant vt = NodeType 0 [PortConstraint [AllPorts] vt]


oneOpPoly, twoOpPoly, threeOpPoly :: NodeType
oneOpPoly   = NodeType 1 [PortConstraint [AllPorts, PortNum 0]                       VTAny]
twoOpPoly   = NodeType 2 [PortConstraint [AllPorts, PortNum 0, PortNum 1]            VTAny]
threeOpPoly = NodeType 3 [PortConstraint [AllPorts, PortNum 0, PortNum 1, PortNum 2] VTAny]


oneOpNum, twoOpNum, threeOpNum :: NodeType
oneOpNum   = NodeType 1 [PortConstraint [AllPorts, PortNum 0]                       VTNumeric]
twoOpNum   = NodeType 2 [PortConstraint [AllPorts, PortNum 0, PortNum 1]            VTNumeric]
threeOpNum = NodeType 3 [PortConstraint [AllPorts, PortNum 0, PortNum 1, PortNum 2] VTNumeric]


oneOpMono, twoOpMono, threeOpMono :: ValueType -> NodeType
oneOpMono vt   = NodeType 1 [PortConstraint [AllPorts, PortNum 0]                       vt]
twoOpMono vt   = NodeType 2 [PortConstraint [AllPorts, PortNum 0, PortNum 1]            vt]
threeOpMono vt = NodeType 3 [PortConstraint [AllPorts, PortNum 0, PortNum 1, PortNum 2] vt]


oneOpLogic, twoOpLogic, threeOpLogic :: NodeType
oneOpLogic   = NodeType 1 [PortConstraint [AllPorts] VTBool                                                        ]
twoOpLogic   = NodeType 2 [PortConstraint [AllPorts] VTBool, PortConstraint [PortNum 0, PortNum 1]            VTAny]
threeOpLogic = NodeType 3 [PortConstraint [AllPorts] VTBool, PortConstraint [PortNum 0, PortNum 1, PortNum 2] VTAny]


---------------------------------------------------------------------------------
-- Functions for modifying port types while connecting ports
---------------------------------------------------------------------------------
promoteConstr :: PortId -> ValueType -> PortConstraint -> Maybe PortConstraint
promoteConstr pid vtype pc@(PortConstraint pids pType) = if pid `elem` pids
    then PortConstraint pids <$> typePromote vtype pType
    else Just pc


-- TODO remove the constraint?
-- promote all the constraints with this port as element
applyType :: PortId -> ValueType -> NodeType -> Maybe NodeType
applyType pid vtype (NodeType pc constrs) = NodeType pc <$> newConstrs
    where newConstrs = sequence $ map (promoteConstr pid vtype) constrs
        

-- TODO for now this doesn't do anything but it could re-add the port to the
-- pids list and relax constraints if necessary
unapplyType :: PortId -> ValueType -> NodeType -> Maybe NodeType
unapplyType _ _ = Just




--replicate :: Int -> a -> a -> Vector a
--replicate = undefined 

--Node 
-- + expr = "replicate"
-- connect 0 Int
-- [
--     0 -> { _type = Int
--            _eq   = []
--            _eqs  = []
--            _equ  = []
--          }

--     1 -> { _type = Any
--            _eq   = []
--            _eqs  = [2, 3]
--            _equ  = []
--          }
--     2 -> { _type = Vector Any
--            _eq   = [3]
--            _eqs  = []
--            _equ  = [1]
--          }
--     3 -> { _type = Vector Any
--            _eq   = []
--            _eqs  = [2]
--            _equ  = [1]
--          }
-- ]



----------------------------------------------------------------------------------------
-- Utils for reading the types:
----------------------------------------------------------------------------------------
tryObject :: Text -> Maybe ValueType
tryObject txt = if txt `elem` ["Vector", "Maybe", "Std", "Playground"]
    then Just VTObject
    else Nothing


tryBool :: Text -> Maybe ValueType
tryBool txt = if txt `elem` ["True", "true"]
    then Just VTBool
    else Nothing

tryInt :: Text -> Maybe ValueType
tryInt txt = let str = Text.unpack txt in if ('.' `notElem` str) && (all isDigit str)
    then Just VTInt
    else Nothing

tryFloat :: Text -> Maybe ValueType
tryFloat txt = let str = Text.unpack txt in if ('.' `elem` str) && (all (\c -> isDigit c || c == '.')  str)
    then Just VTFloat
    else Nothing

tryString :: Text -> Maybe ValueType
tryString _ = Just VTString


readType :: Text -> ValueType
readType txt = fromJust $ msum matches  -- it's bound to return Just
    where matches = ($ txt) <$> [tryObject, tryBool, tryInt, tryFloat, tryString]
