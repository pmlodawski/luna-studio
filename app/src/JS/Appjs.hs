module JS.Appjs where

import JS.Bindings

setNodeUnselected :: Int -> IO ()
setNodeUnselected nodeId =
    getNode nodeId >>= setUnselected

setNodeSelected :: Int -> IO ()
setNodeSelected nodeId = do
    getNode nodeId >>= setSelected
    moveToTopZ nodeId

setNodeFocused :: Int -> IO ()
setNodeFocused nodeId = do
    unfocusAllNodes
    getNode nodeId >>= setFocused
    moveToTopZ nodeId

setNodeUnfocused :: Int -> IO ()
setNodeUnfocused nodeId = do
    unfocusAllNodes
    getNode nodeId >>= setFocused
    moveToTopZ nodeId



unselectAllNodes :: IO ()
unselectAllNodes =
    forM_ getNodes setNodeUnselected

-- unfocusAllNodes :: IO ()
-- unfocusAllNodes =
--     forM_ getNodes


-- function unfocusAllNodes() {
--   _.each(nodes, function(node){
--       if (node.selected() === 2) node.selected(1);
--   });
-- }