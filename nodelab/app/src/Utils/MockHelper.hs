{-# LANGUAGE OverloadedStrings #-}

module Utils.MockHelper where

import           Utils.PreludePlus
import           Control.Monad

import           Data.Char         (isDigit)
import           Data.List         (all)
import           Data.Maybe        (fromJust)
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
tryVal _ = Just $ NodeType 1 []


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
                  , ("++",       twoOpMono VTString)
                  , ("toString", NodeType 1 [PortConstraint [AllPorts] VTString])
                  , ("truncate", oneOpMono VTFloat)
                  , ("round",    oneOpMono VTFloat)
                  , ("floor",    oneOpMono VTFloat)
                  , ("ceiling",  oneOpMono VTFloat)
                  ]





-- yes, we do the same computation twice -- that's a workaround right here, don't mind :)
getNodeType :: Text -> NodeType
getNodeType expr = case msum $ ($ expr) <$> [tryKnown, tryDef, tryFormat, tryVal] of
    Just portType -> portType
    Nothing    -> NodeType 0 []

----------------------------------------------------------------------------------------
-- Type utils
----------------------------------------------------------------------------------------
typesEq :: ValueType -> ValueType -> Bool
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
typePromote _              _              = Nothing

    
-- common port types:
oneOpPoly, twoOpPoly, threeOpPoly :: NodeType
oneOpPoly   = NodeType 1 [PortConstraint [AllPorts, PortNum 1]                     VTAny]
twoOpPoly   = NodeType 2 [PortConstraint [AllPorts, PortNum 1, PortNum 2]           VTAny]
threeOpPoly = NodeType 3 [PortConstraint [AllPorts, PortNum 1, PortNum 2, PortNum 3] VTAny]


oneOpNum, twoOpNum, threeOpNum :: NodeType
oneOpNum   = NodeType 1 [PortConstraint [AllPorts, PortNum 1]                     VTNumeric]
twoOpNum   = NodeType 2 [PortConstraint [AllPorts, PortNum 1, PortNum 2]           VTNumeric]
threeOpNum = NodeType 3 [PortConstraint [AllPorts, PortNum 1, PortNum 2, PortNum 3] VTNumeric]


oneOpMono, twoOpMono, threeOpMono :: ValueType -> NodeType
oneOpMono vt   = NodeType 1 [PortConstraint [AllPorts, PortNum 1]                     vt]
twoOpMono vt   = NodeType 2 [PortConstraint [AllPorts, PortNum 1, PortNum 2]           vt]
threeOpMono vt = NodeType 3 [PortConstraint [AllPorts, PortNum 1, PortNum 2, PortNum 3] vt]


oneOpLogic, twoOpLogic, threeOpLogic :: NodeType
oneOpLogic   = NodeType 1 [PortConstraint [AllPorts] VTBool                                                     ]
twoOpLogic   = NodeType 2 [PortConstraint [AllPorts] VTBool, PortConstraint [PortNum 1, PortNum 2]    VTAny       ]
threeOpLogic = NodeType 3 [PortConstraint [AllPorts] VTBool, PortConstraint [PortNum 1, PortNum 2, PortNum 3] VTAny]


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


----------------------------------------------------------------------------------------
-- Utils for reading the types:
----------------------------------------------------------------------------------------
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
    where matches = ($ txt) <$> [tryBool, tryInt, tryFloat, tryString]
