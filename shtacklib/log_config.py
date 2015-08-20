from contextlib import contextmanager
import inspect
import logging
import logging.config
from pathlib import Path
import sys
import traceback
import yaml
from io_utils import fmt
from clint.textui import colored

BASE_CONFIG = Path('repo_manager/logger_config.yaml')


def main_logger(init=False):
    if init:
        init_logger()

    parent = inspect.stack()[1][0]
    return logging.getLogger(parent.f_globals["__file__"])


def init_logger():
    with BASE_CONFIG.open() as fh:
        cfg = yaml.load(fh)
        logging.config.dictConfig(cfg)


class CustomAdapter(logging.LoggerAdapter):
    def process(self, msg, kwargs):
        action_name = colored.cyan(fmt("({self.extra[action_name]})"))
        msg = fmt("{action_name} {msg}")
        return msg, kwargs

    def _common(self, method, msg, *args, **kwargs):
        parent = inspect.stack()[2][0]
        msg = fmt(msg, parent=parent)

        msg, _ = self.process(msg, kwargs)

        if 'exc_info' in kwargs:
            tb = traceback.format_exception(*sys.exc_info())
            tb = ''.join(tb)
            tb = colored.black(tb)
            msg = msg + '\n' + tb

            kwargs = kwargs.copy()
            del kwargs['exc_info']

        return getattr(self.logger, method)(msg, *args, **kwargs)

    def debug(self, msg, *args, **kwargs):
        self._common('debug', msg, *args, **kwargs)

    def info(self, msg, *args, **kwargs):
        self._common('info', msg, *args, **kwargs)

    def warning(self, msg, *args, **kwargs):
        self._common('warning', msg, *args, **kwargs)

    def warn(self, msg, *args, **kwargs):
        self._common('warn', msg, *args, **kwargs)

    def error(self, msg, *args, **kwargs):
        self._common('error', msg, *args, **kwargs)

    def critical(self, msg, *args, **kwargs):
        self._common('critical', msg, *args, **kwargs)

    def exception(self, msg, *args, **kwargs):
        self._common('error', msg, *args, **kwargs)


@contextmanager
def logging_action(log, action_name):
    yield CustomAdapter(log, {'action_name': action_name})


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

        if record.levelno == logging.DEBUG:
            supermsg = colored.black(supermsg)

        return fmt("{level_name} {name} {fmttime} {supermsg}")

