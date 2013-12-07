from utils.colorama import Fore, init
init()

class bcolors:
    INFO    = Fore.GREEN
    WARNING = Fore.YELLOW
    ERROR   = Fore.RED 
    RESET   = Fore.RESET

    def disable(self):
        self.INFO = ''
        self.WARNING = ''
        self.ERROR = ''
        self.RESET = ''

def format_info(s):
    return bcolors.INFO + s + bcolors.RESET

def format_warning(s):
    return bcolors.WARNING + s + bcolors.RESET

def print_info(s):
    print format_info(s)

def print_warning(s):
    print format_warning(s)

def print_error(s):
    print bcolors.ERROR + s + bcolors.RESET