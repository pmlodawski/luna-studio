module JS.NodeGraph where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import           JS.Node
import           Object.UITypes

import           Data.JSString (pack)
import           Data.JSString.Text  (lazyTextToJSString)

import           JavaScript.Array    (JSArray)
import qualified JavaScript.Array    as JSArray
import           Empire.API.Data.Node (NodeId)

import GHCJS.Marshal


displayNodeVector :: NodeId -> [Float] -> IO ()
displayNodeVector nodeId vals = putStrLn "displayNodeVector" -- do
--     nodeRef <- getNode nodeId
--     valsRef <- mapM toJSVal_pure vals
--     displayVector nodeRef $ JSArray.fromList valsRef

-- createNodeAt :: Int -> Vector2 Double -> Text -> Int -> IO ()
-- createNodeAt nodeId (Vector2 px py) expr wid = do
--     newNodeAt nodeId px py expr wid
--
