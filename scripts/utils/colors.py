class bcolors:
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    INFO = '\033[92m'
    WARNING = '\033[93m'
    ERROR = '\033[91m'
    ENDC = '\033[0m'

    def disable(self):
        self.HEADER = ''
        self.BLUE = ''
        self.INFO = ''
        self.WARNING = ''
        self.ERROR = ''
        self.ENDC = ''


def print_info(s):
    print 
    print bcolors.INFO + "=== %s ===" % s + bcolors.ENDC

def print_error(s):
    print
    print bcolors.ERROR + s + bcolors.ENDC