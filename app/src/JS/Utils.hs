module JS.Utils where


-- screenToGl :: (Int, Int) -> (Int, Int)
-- screenToGl (x, y) = (x - $$.screenSize.x / 2, - y + $$.screenSize.y / 2)

-- screenToNormalizedGl :: (Int, Int) -> (Int, Int)
-- screenToNormalizedGl (x, y) = (x / $$.screenSize.x * 2 - 1, -(y / $$.screenSize.y) * 2 + 1)

-- glToWorkspace :: (Int, Int) -> (Int, Int)
-- glToWorkspace (x, y) = (x / $$.camFactor.value + $$.camPan.x, y / $$.camFactor.value + $$.camPan.y)

-- screenToWorkspace :: (Int, Int) -> (Int, Int)
-- screenToWorkspace = glToWorkspace . screenToGl

-- workspaceToScreen :: (Int, Int) -> (Int, Int)
-- workspaceToScreen (x, y) = ((x  - $$.camPan.x) * $$.camFactor.value + $$.halfScreen.x , (-y + $$.camPan.y) * $$.camFactor.value + $$.halfScreen.y)
