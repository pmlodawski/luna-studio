{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.React.Model.Port
    ( module NodeEditor.React.Model.Port
    , module X
    )
    where

import           Common.Prelude                    hiding (set)
import           Data.Convert                      (Convertible (convert))
import           LunaStudio.Data.Angle             (Angle)
import           LunaStudio.Data.LabeledTree       (LabeledTree (LabeledTree))
import qualified LunaStudio.Data.LabeledTree       as LabeledTree
import           LunaStudio.Data.Port              as X hiding (InPort, OutPort, Port (..), PortState (..), name, portId, state, valueType,
                                                         _Connected, _NotConnected, _WithDefault)
import qualified LunaStudio.Data.Port              as Empire
import qualified LunaStudio.Data.PortDefault       as Empire
import           LunaStudio.Data.PortRef           as X (AnyPortRef (InPortRef', OutPortRef'), InPortRef (InPortRef),
                                                         OutPortRef (OutPortRef))
import           LunaStudio.Data.Position          (Position)
import           LunaStudio.Data.TypeRep           (TypeRep (..))
import           NodeEditor.Data.Color             (Color)
import qualified NodeEditor.Data.Color             as Color
import           NodeEditor.React.Model.Constants  (lineHeight, nodeRadius)
import           NodeEditor.React.Model.InputField (InputField, content, mkInputField)


type IsAlias = Bool
type IsSelf  = Bool
type IsOnly  = Bool

data Mode = Normal
          | Invisible
          | Inactive
          | TypeNotMatched
          | Highlighted
          | Moved Position
          | NameEdit
          deriving (Eq, Show, Typeable, Generic, NFData)

instance Default Mode where
    def = Normal

data PortValue = IntValue  Integer
               | RealValue Double
               | BoolValue Bool
               | TextValue InputField
               deriving (Eq, Generic, NFData, Show)

data PortDefault = Expression Text
                 | Constant   PortValue
                 deriving (Eq, Generic, NFData, Show)

data PortState = NotConnected | Connected | WithDefault PortDefault deriving (Eq, Generic, NFData, Show)

makePrisms ''PortValue
makePrisms ''PortDefault
makePrisms ''PortState

instance Convertible Empire.PortValue PortValue where
    convert (Empire.IntValue  i) = IntValue i
    convert (Empire.RealValue r) = RealValue r
    convert (Empire.BoolValue b) = BoolValue b
    convert (Empire.TextValue s) = TextValue . mkInputField $ convert s

instance Convertible Empire.PortDefault PortDefault where
    convert (Empire.Expression s) = Expression $ convert s
    convert (Empire.Constant   c) = Constant   $ convert c

instance Convertible Empire.PortState PortState where
    convert (Empire.NotConnected)  = NotConnected
    convert (Empire.Connected)     = Connected
    convert (Empire.WithDefault v) = WithDefault $ convert v


instance Convertible PortValue Empire.PortValue where
    convert (IntValue  i) = Empire.IntValue i
    convert (RealValue r) = Empire.RealValue r
    convert (BoolValue b) = Empire.BoolValue b
    convert (TextValue f) = Empire.TextValue . convert $ f ^. content

instance Convertible PortDefault Empire.PortDefault where
    convert (Expression s) = Empire.Expression $ convert s
    convert (Constant   c) = Empire.Constant   $ convert c

instance Convertible PortState Empire.PortState where
    convert (NotConnected)  = Empire.NotConnected
    convert (Connected)     = Empire.Connected
    convert (WithDefault v) = Empire.WithDefault $ convert v

data Port i = Port
        { _portId    :: i
        , _name      :: Text
        , _valueType :: TypeRep
        , _state     :: PortState
        , _mode      :: Mode
        } deriving (Eq, Functor, Generic, NFData, Show, Typeable)

type InPort     = Port InPortId
type OutPort    = Port OutPortId
type AnyPort    = Port AnyPortId
type EitherPort = Either InPort OutPort

makeLenses ''Port

color :: Getter (Port i) Color
color = valueType . to Color.fromType

isInMode :: Mode -> Port i -> Bool
isInMode m p = case (m, p ^. mode) of
    (Moved _, Moved _) -> True
    (m1, m2)           -> m1 == m2

isInNormalMode :: Port i -> Bool
isInNormalMode = isInMode Normal

isInvisible :: Port i -> Bool
isInvisible = isInMode Invisible

ensureVisibility :: Mode -> Mode
ensureVisibility Invisible = Normal
ensureVisibility m = m

isHighlighted :: Port i -> Bool
isHighlighted = isInMode Highlighted

isInMovedMode :: Port i -> Bool
isInMovedMode = isInMode (Moved def)

isInNameEditMode :: Port i -> Bool
isInNameEditMode = isInMode NameEdit

getPositionInSidebar :: Port i -> Maybe Position
getPositionInSidebar p = case p ^. mode of
    Moved pos -> Just pos
    _         -> Nothing

visibleOutPorts :: OutPortTree (Port i) -> [Port i]
visibleOutPorts (LabeledTree (OutPorts []) p) = [p]
visibleOutPorts (LabeledTree (OutPorts ps) _) = concatMap visibleOutPorts ps

visibleInPorts :: InPortTree (Port i) -> [Port i]
visibleInPorts root@(LabeledTree (InPorts maySelf _ args') _) = findSelfAndOrHead <> map (view LabeledTree.value) args' where
    h                 = findHead root
    findSelfAndOrHead = (if h ^. state == Connected then [h] else []) <> maybeToList (findSelf <$> maySelf)
    findHead (LabeledTree (InPorts _ mayHead' _) p') = if p' ^. state == Connected then p' else maybe p' findHead mayHead'
    findSelf (LabeledTree (InPorts maySelf' _ _) p') = if p' ^. state == Connected then p' else maybe p' findSelf maySelf'


instance Convertible InPort  AnyPort    where convert = fmap InPortId'
instance Convertible OutPort AnyPort    where convert = fmap OutPortId'
instance Convertible InPort  EitherPort where convert = convert . fmap InPortId'
instance Convertible OutPort EitherPort where convert = convert . fmap OutPortId'

instance Convertible AnyPort EitherPort where
    convert (Port (InPortId'  i) n nt s m) = Left  $ Port i n nt s m
    convert (Port (OutPortId' i) n nt s m) = Right $ Port i n nt s m

instance Convertible EitherPort AnyPort where
    convert (Left  port) = InPortId'  <$> port
    convert (Right port) = OutPortId' <$> port

instance Convertible (Empire.Port i) (Port i) where
    convert p = Port
        {- portId    -} (p ^. Empire.portId)
        {- name      -} (p ^. Empire.name)
        {- nodeType  -} (p ^. Empire.valueType)
        {- state     -} (convert $ p ^. Empire.state)
        {- mode      -} Normal

instance Convertible (Port i) (Empire.Port i) where
    convert p = Empire.Port
        {- portId    -} (p ^. portId)
        {- name      -} (p ^. name)
        {- nodeType  -} (p ^. valueType)
        {- state     -} (convert $ p ^. state)


portGap :: Double -> Angle
portGap r = 0.2 * nodeRadius / r -- to avoid gap narrowing

portAngle :: Int -> Angle
portAngle numOfPorts = pi / fromIntegral numOfPorts

portAngleStart :: Bool -> Int -> Int -> Double -> Angle
portAngleStart isShape num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = if isShape then (portGap r)/2 else 0
        t      = portAngle numOfPorts
    in  pi - number * t + gap

portAngleStop :: Bool -> Int -> Int -> Double -> Angle
portAngleStop isShape num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = if isShape then (portGap r)/2 else 0
        t      = portAngle numOfPorts
    in  pi - number * t + t - gap

argumentConstructorOffsetY :: Int -> Double
argumentConstructorOffsetY numOfPorts = (fromIntegral numOfPorts + 1) * lineHeight


