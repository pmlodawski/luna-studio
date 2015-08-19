from contextlib import contextmanager
import inspect
from io_utils import fprint
import platform


@contextmanager
def releasing(obj):
    try:
        yield obj
    finally:
        obj.release()


@contextmanager
def caveat(msg, *exc, platforms=None):
    if not exc:
        exc = Exception
    try:
        yield
    except exc as e:
        this_system = platform.system()
        if not platform or this_system == platforms or this_system in platforms:
            parent = inspect.stack()[1][0]
            fprint(msg,
                   parent=parent,
                   colour='yellow')
        raise e
