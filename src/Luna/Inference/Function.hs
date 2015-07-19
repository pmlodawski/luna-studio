{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Luna.Inference.Function where

import Flowbox.Prelude
import qualified Luna.Inference.Type     as Type
import           Luna.Inference.Type     hiding (Function, Instance)
import qualified Luna.Inference.Class    as Class
--import           Luna.Inference.Value    (Value)
import           Luna.Inference.Instance
import qualified Data.Sequence           as Seq
import           Luna.Inference.RawData
import           Data.Convert



type Function = Instance Type.Function



--arg :: Text -> Arg
--arg = flip Arg def . Just

argOf :: Text -> Object -> Arg
argOf n v = Arg (Just n) (Just v)

arity :: Type.Function -> Int
arity = Seq.length . view args



class ArgCons a b where
    arg :: a -> b

instance ArgCons Object Arg where
    arg v = def & Type.val .~ Just v

instance Convertible s Text => ArgCons s Arg where
    arg n = def & Type.name .~ Just (convert n)

instance Convertible s Text => ArgCons s (Object -> Arg) where
    arg n v = arg n & Type.val .~ Just v


--instance IsList (Seq.Seq a) where
--    type Item (Seq.Seq a) = a
--    fromList = Seq.fromList
--    toList   = Seq.toList

--mkFunction :: ToRArgs args => (args -> out) -> [Arg] -> Function
--mkFunction f args = Instance (Type.Function (fromList args)) (packRawData $ toDataFunc f)

--toDataFunc :: ToRArgs args => (args -> out) -> Repr
--toDataFunc f = packRawData . f . toRArgs

--appSimple :: Repr -> [Object] -> Object
--appSimple f objs = mkObject . f $ fmap unpackRawData objs

class Curry a where
    curry :: a -> Function -> Object

--instance Curry Value where

--curry :: Object -> Function -> Object
--curry f =

class ToRArgs args where
    toRArgs :: [RawData] -> args

instance ToRArgs () where
    toRArgs [] = ()

instance ToRArgs as => ToRArgs (a, as) where
    toRArgs (a:as) = (unpackRawData a, toRArgs as)

