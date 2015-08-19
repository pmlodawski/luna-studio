from contextlib import contextmanager


@contextmanager
def releasing(obj):
    try:
        yield obj
    finally:
        obj.release()
