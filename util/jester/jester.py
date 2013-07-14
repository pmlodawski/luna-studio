###########################################################################
## Copyright (C) Flowbox, Inc # All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2013
###########################################################################

from jester import jester
import errno, sys

import logging
logger = logging.getLogger(__name__)

def main():
    jester.init()
    from jester.config import base # needed
    jester.resolve()

    try:
        jester.run()
    except Exception as e:
        logger.error(str(e))
        sys.exit(1)


if __name__ == "__main__":
    main()
