from contextlib import contextmanager
import inspect
import os
from pathlib import Path
import shutil
from io_utils import fprint, ferror, finfo
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


@contextmanager
def suppress_callback(*exc, on_caugth_exc=None):
    try:
        yield
    except exc:
        try:
            if on_caugth_exc:
                on_caugth_exc()
        except TypeError as e:
            ferror("There was a problem with callback")
            raise e


@contextmanager
def tempdir_debuggable(*, root_pth, name, preserve=False):
    """Works in a similar way to tempfile.TemporaryDirectory but on exception does NOT remove the dir, only displays
    the path, to allow debugging."""

    name_pth = Path(root_pth) / name

    already_existed = name_pth.is_dir() or name_pth.is_symlink()

    if already_existed:
        if preserve:
            finfo("'{name_pth}' already exists, preseving")
        else:
            finfo("'{name_pth}' already exists, removing")
            shutil.rmtree(str(name_pth),
                          onerror=lambda exc_fun, path, exc_ifo: os.unlink(str(name_pth)))

    else:
        finfo("Creating '{name_pth}'")
        name_pth.mkdir(parents=True)

    try:
        yield str(name_pth), already_existed
    except Exception as e:
        ferror("Not removing temp directory '{name_pth}' since some error occured, please investigate")
        raise e
    else:
        finfo("Removing temp dir '{name_pth}'")
        shutil.rmtree(str(name_pth))