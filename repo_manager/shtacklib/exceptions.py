import inspect
from io_utils import fmt
# noinspection PyUnresolvedReferences
from clint.textui import colored


class ShtackExceptions(Exception):
    def __init__(self, *args, parent=None, noformat=False, **kwargs):
        if noformat:
            super().__init__(colored.red(args[0]))
        else:
            if not parent:
                parent = inspect.stack()[1][0]

            fmt_args = kwargs.copy()
            fmt_args['parent'] = parent

            super().__init__(colored.red(fmt(args[0], **fmt_args)))


class ShtackHookAbort(ShtackExceptions):
    def __init__(self, *args, parent=None, **kwargs):
        if not parent:
            parent = inspect.stack()[1][0]

        fmt_args = kwargs.copy()
        fmt_args['parent'] = parent

        super().__init__("Aborted: " + fmt(args[0], **fmt_args),
                         noformat=True)


class ShtackWrongStackVersion(Exception):
    pass
