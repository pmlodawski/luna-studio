import logging
from jester.utils import text
from logging import Formatter

logger          = logging.root
consoleHandler  = logging.StreamHandler()

class ColorFormatter(Formatter):
    def format(self, record):
        prefix = ''
        if record.levelno == logging.DEBUG:
            prefix = text.color.lblue('[DEBUG]   ')
        elif record.levelno == logging.INFO:
            prefix = text.color.lgreen('[INFO]    ')
        elif record.levelno == logging.WARNING:
            prefix = text.color.lyellow('[WARNING] ')
        elif record.levelno == logging.ERROR:
            prefix = text.color.lred('[ERROR]   ')
        elif record.levelno == logging.CRITICAL:
            prefix = text.color.lred('[CRITICAL]')
        record.msg = prefix + record.msg
        return super(ColorFormatter, self).format(record)

def init(verbose=False):
    if verbose:
        level = logging.DEBUG
    else:
        level = logging.INFO

    logger.setLevel(logging.DEBUG)
    consoleHandler.setLevel(level)

    if level == logging.DEBUG:
        fmt = '%(message)s (%(name)s:%(lineno)s)', '%Y:%m:%d:%H:%M:%S'
    else:
        fmt = '%(message)s', '%Y:%m:%d:%H:%M:%S'

    formatter = ColorFormatter(*fmt)
    consoleHandler.setFormatter(formatter)
    logger.addHandler(consoleHandler)

    logger.debug("logger initialized")






