from functools import wraps
import inspect
import sys
import shutil


def eprint(*args, **kwargs):
    """ Print to stderr """
    print(*args, **dict(kwargs, file=sys.stderr))


def try_colour(x, colour):
    try:
        from clint.textui import colored, puts
        puts(getattr(colored, colour)(x))
    except ImportError:
        print(x)


def with_stage_header(*, colour='cyan', description_override=None):
    """ Prints nice header info from function's docstring.

    :param colour: The colour of the header or no colour if 'None'.

    :type colour: String | None
    """

    try:
        from clint.textui import colored, puts
    except ImportError:
        colour = None

    def aux(stage_fun):
        """
        :param stage_fun: The function to wrap. Takes one argument: the ArgumentParser result.

        :type stage_fun: (Any) -> None
        :rtype: None
        """

        descr = description_override or stage_fun.__doc__ or ""
        descr = [x.lstrip() for x in descr.splitlines()]
        if descr[-1] == "":
            descr.pop()
        descr = '\n# '.join(descr)

        terminal_width = shutil.get_terminal_size((80, 20)).columns

        header = "\n" \
                 "{:#^{width}}\n" \
                 "# {descr}\n" \
                 "".format("#", width=terminal_width, descr=descr)

        @wraps(stage_fun)
        def wrapper():
            if not colour:
                print(header)
            else:
                puts(getattr(colored, colour)(header))

            return stage_fun()
        return wrapper

    return aux


def fprint(x=None, *, parent=None, notrunc=False, colour=None, **kwargs):
    """ Format the string and print to stdout.

    Additionally, if the passed string is multiline, strips the greatest common whitespace prefix, so
    the message looks sane. E.g.:

        fprint('''
        Hi there
        How do you do?
        ''')

    will print the message *without* any whitespace on the left.

    Moreover:

    - if kwargs is passed: use `print` passing it `kwargs`
    - if colour is passed: try to use `put` from `clint` library, fallback to `print`
    - if none is set, use `print`

    :param x: The string to format.
    :param parent: The stack frame to look up for variables. 'None' means: take from the caller.
    :param notrunc: Usually, we truncate left spaces so multi-line \"\"\" look ok!
    :param kwargs: Optional arguments passed to 'print'.
    :param colour: Name of the colour

    :type x: String
    :type parent: Any
    :type notrunc: bool
    :type colour: str
    :rtype: None
    """

    if not parent:
        parent = inspect.stack()[1][0]



    if x and not notrunc:
        x_lines = [
            line.expandtabs(tabsize=4)
            for line in x.splitlines()
        ]
        if not x_lines[0].rstrip():
            x_lines = x_lines[1:]
        if x_lines and not x_lines[-1].rstrip():
            x_lines = x_lines[:-1]
        min_indent = min(
            len(line) - len(line.lstrip())
            for line in x_lines
            if line
        )
        x = '\n'.join(
            line[min_indent:]
            for line in x_lines
        )

    if not x:
        x = ""
    x = fmt(x, parent=parent)

    try:
        if not kwargs and colour:
            from clint.textui import colored, puts
            kwargs or puts(getattr(colored, colour)(x))
    except ImportError:
        print(x)
    else:
        print(x, **kwargs)


def fmt(x, *, parent=None):
    """ Format the string Ruby-like.

    :param x: The string to format.
    :param parent: The stack frame to look up for variables. 'None' means: take from the caller.

    :type x: String
    :type parent: Any
    :rtype: String
    """

    if not parent:
        parent = inspect.stack()[1][0]

    var = parent.f_globals
    var.update(parent.f_locals)

    return x.format(**var)
