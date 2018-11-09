import * as path    from 'path'
import * as logger  from 'luna-logger'

import * as Backend from './backend'
import * as Enum    from './enum'

import {LunaStudio} from './LunaStudio'


# TODO: We should never keep such functions attached to window.
window.listVisualizers = => [] #TODO
window.getInternalVisualizersPath = => '' #TODO
window.getLunaVisualizersPath = => '' #TODO
window.getInternalVisualizers = => {} #TODO
window.getLunaVisualizers = => [] #TODO
window.getProjectVisualizers = => [] #TODO
window.getImportedVisualizers = => {} #TODO

############
### Main ###
############

# TODO: This should be obtained from the backend, not hardcoded here
messages = Enum.make(
  'Init', 
  'ProjectSet', 
  'FileOpened',
  'SetProject',
  'GetBuffer'
)

main = () -> 
  logger.group 'Launching Luna Studio', =>
    logger.info 'Initializing backend connectors'
    backend = await Backend.initialize()

    logger.info 'Initializing frontend'
    ls = new LunaStudio backend
    logger.group 'Launching frontend', =>
    ls.launch()

main()
