###########################################################################
## Copyright (C) Flowbox, Inc # All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2013
###########################################################################

class color:
    NONE   = '\033[0m'

    BLACK   = '\033[30m'
    RED     = '\033[31m'
    GREEN   = '\033[32m'
    YELLOW  = '\033[33m'
    BLUE    = '\033[34m'
    VIOLET  = '\033[35m'
    CYAN    = '\033[36m'
    WHTE    = '\033[37m'
    NOCOLOR = '\033[39m'

    LBLACK   = '\033[90m'
    LRED     = '\033[91m'
    LGREEN   = '\033[92m'
    LYELLOW  = '\033[93m'
    LBLUE    = '\033[94m'
    LVIOLET  = '\033[95m'
    LCYAN    = '\033[96m'
    LWHTE    = '\033[97m'
    LNOCOLOR = '\033[99m'

    BGBLACK   = '\033[40m'
    BGRED     = '\033[41m'
    BGGREEN   = '\033[42m'
    BGYELLOW  = '\033[43m'
    BGBLUE    = '\033[44m'
    BGVIOLET  = '\033[45m'
    BGCYAN    = '\033[46m'
    BGWHTE    = '\033[47m'
    BGNOCOLOR = '\033[49m'

    BGLBLACK   = '\033[100m'
    BGLRED     = '\033[101m'
    BGLGREEN   = '\033[102m'
    BGLYELLOW  = '\033[103m'
    BGLBLUE    = '\033[104m'
    BGLVIOLET  = '\033[105m'
    BGLCYAN    = '\033[106m'
    BGLWHTE    = '\033[107m'
    BGLNOCOLOR = '\033[109m'


    BOLD        = '\033[1m'
    BOLDE       = '\033[2m'
    UNDERLINE   = '\033[4m'
    UNDERLINEE  = '\033[8m'

    OK          = LGREEN
    WARNING     = LYELLOW
    ERROR       = LRED
    INFO        = LBLUE

    @staticmethod
    def lblue(txt):
        return color.LBLUE + txt + color.NONE

    @staticmethod
    def lgreen(txt):
        return color.LGREEN + txt + color.NONE

    @staticmethod
    def lyellow(txt):
        return color.LYELLOW + txt + color.NONE

    @staticmethod
    def lred(txt):
        return color.LRED + txt + color.NONE
    

