export shortcuts = (f) =>
  'cancel': => f 'Cancel'
  'accept': => f 'Accept'
  # camera
  'center-graph': => f 'CenterGraph'
  'pan-down':     => f 'PanDown'
  'pan-left':     => f 'PanLeft'
  'pan-right':    => f 'PanRight'
  'pan-up':       => f 'PanUp'
  'reset-camera': => f 'ResetCamera'
  'reset-pan':    => f 'ResetPan'
  'reset-zoom':   => f 'ResetZoom'
  'zoom-in':      => f 'ZoomIn'
  'zoom-out':     => f 'ZoomOut'
  # clipboard
  'copy':  => f 'Copy'
  'cut':   => f 'Cut'
  'paste': => f 'Paste', atom.clipboard.read() #TODO
  # navigation
  'exit-graph':    => f 'ExitGraph'
  'go-cone-down':  => f 'GoConeDown'
  'go-cone-left':  => f 'GoConeLeft'
  'go-cone-right': => f 'GoConeRight'
  'go-cone-up':    => f 'GoConeUp'
  'go-down':       => f 'GoDown'
  'go-left':       => f 'GoLeft'
  'go-next':       => f 'GoNext'
  'go-prev':       => f 'GoPrev'
  'go-right':      => f 'GoRight'
  'go-up':         => f 'GoUp'
  # nodes
  'autolayout-all-nodes':        => f 'AutolayoutAllNodes'
  'autolayout-selected-nodes':   => f 'AutolayoutSelectedNodes'
  'close-visualization-preview': => f 'CloseVisualizationPreview'
  'collapse-to-function':        => f 'CollapseToFunction'
  'edit-selected-nodes':         => f 'EditSelectedNodes'
  'expand-selected-nodes':       => f 'ExpandSelectedNodes'
  'open-visualization-preview':  => f 'OpenVisualizationPreview'
  'remove-selected-nodes':       => f 'RemoveSelectedNodes'
  'select-all':                  => f 'SelectAll'
  'unfold-selected-nodes':       => f 'UnfoldSelectedNodes'
  'zoom-visualization':          => f 'ZoomVisualization'
  # undo/redo
  'core:redo': => f 'Redo'
  'core:undo': => f 'Undo'
  # MockMonads
  'mock-add-monad':    => f 'MockAddMonad'
  'mock-clear-monads': => f 'MockClearMonads'
  # searcher
  'searcher-open': (e)        => f 'SearcherOpen', e.detail
  'searcher-edit-expression': => f 'SearcherEditExpression'
  # debug
  'debug-layer-0': => f 'EnableDebugLayer', "0"
  'debug-layer-1': => f 'EnableDebugLayer', "1"
  'debug-layer-2': => f 'EnableDebugLayer', "3"
  'debug-layer-3': => f 'EnableDebugLayer', "2"
  'debug-layer-4': => f 'EnableDebugLayer', "4"
  'debug-layer-5': => f 'EnableDebugLayer', "5"
  'debug-layer-6': => f 'EnableDebugLayer', "6"
  'debug-layer-7': => f 'EnableDebugLayer', "7"
  'debug-layer-8': => f 'EnableDebugLayer', "8"
  'debug-layer-9': => f 'EnableDebugLayer', "9"
