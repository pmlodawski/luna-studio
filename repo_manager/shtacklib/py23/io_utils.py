def try_colour(x, colour):
    try:
        from clint.textui import colored, puts
        puts(getattr(colored, colour)(x))
    except ImportError:
        print(x)
