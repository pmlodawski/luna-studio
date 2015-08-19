import inspect
import logging
import logging.config
from pathlib import Path
import yaml
from io_utils import fmt
from clint.textui import colored

BASE_CONFIG = Path('repo_manager/logger_config.yaml')


def main_logger():
    parent = inspect.stack()[1][0]
    return logging.getLogger(parent.f_globals["__file__"])


def init_logger():
    with BASE_CONFIG.open() as fh:
        cfg = yaml.load(fh)
        logging.config.dictConfig(cfg)


class CustomFormatter(logging.Formatter):
    def format(self, record):

        level_name = record.levelname

        if record.levelno == logging.CRITICAL:
            level_name = colored.red(record.levelname)
            level_name.bold = True
        elif record.levelno == logging.ERROR:
            level_name = colored.red(record.levelname)
        elif record.levelno == logging.WARNING:
            level_name = colored.yellow(record.levelname)
        elif record.levelno == logging.INFO:
            level_name = colored.blue(record.levelname)
        elif record.levelno == logging.DEBUG:
            level_name = colored.black(record.levelname)

        # size of colour-codes
        level_name = level_name.ljust(10+len(level_name)-len(record.levelname))

        fmttime = self.formatTime(record, datefmt=self.datefmt)
        fmttime = colored.black(fmttime)

        name = record.name
        name = colored.black(name)

        supermsg = super(CustomFormatter, self).format(record)

        if record.levelno >= logging.ERROR:
            supermsg = colored.red(supermsg)

        return fmt("{level_name} {name} {fmttime}: {supermsg}")

