import sys

from colors import print_error


def handle_out(code):
    if code != 0 and code != "":
        print_error("ERROR")
        sys.exit(code)

def autocall(args):
    handle_out(call(args))