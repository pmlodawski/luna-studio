import * as logger  from 'luna-logger'

import * as Backend from './backend'

import {LunaStudio} from './LunaStudio'


############
### Main ###
############


main = () -> 
  logger.group 'Launching Luna Studio', =>
    backend = await logger.group 'Initializing backend connectors', =>
      Backend.initialize()
    ls = logger.group 'Initializing frontend', =>
      new LunaStudio backend
    logger.group 'Launching frontend', =>
      ls.launch()

main()
