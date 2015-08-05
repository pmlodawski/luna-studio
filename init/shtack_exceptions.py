import inspect
from io_utils import fmt


class ShtackExceptions(Exception):
    def __init__(self, *args, parent=None, **kwargs):
        if not parent:
            parent = inspect.stack()[1][0]

        fmt_args = kwargs.copy()
        fmt_args.setdefault('colour', 'red')
        fmt_args['parent'] = parent
        super().__init__(fmt(*args, **fmt_args))


class ShtackHookAbort(Exception):
    pass
