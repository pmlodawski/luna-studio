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


def print_info(s):
    print bcolors.INFO + s + bcolors.RESET

def print_warning(s):
    print bcolors.WARNING + s + bcolors.RESET

def print_error(s):
    print bcolors.ERROR + s + bcolors.RESET