import argparse
import cPickle
from subprocess import call

class ConfigObject:
    target = None

def configure(args):
    conf        = ConfigObject()
    conf.target = args.target
    sconf       = cPickle.dumps(conf)
    with open ('.jester', 'w') as file:
        cfgtxt = file.write(sconf)

parser           = argparse.ArgumentParser(description='Compile multitarget Haskell code.')
action_parsers   = parser.add_subparsers()

parser_configure = action_parsers.add_parser('configure', help='Configure project.')
parser_configure.set_defaults(func=configure)
parser_configure.add_argument('--target', '-t', nargs=1, choices=['native', 'js'], default='native',
                            help='Target compilation platform')
parser_build     = action_parsers.add_parser('build',     help='Build project.')
parser_clean     = action_parsers.add_parser('clean',     help='Clean project.')




def read_jester_config():
    with open ('.jester', 'r') as file:
        cfgtxt = file.read()

def read_cabal_config():
    import os
    cwd =  os.getcwd()
    files = os.listdir(cwd)

    # Search for config file
    configfile = None
    for file in files:
        if file.endswith('.cabal'):
            configfile = file
            break

    if not configfile:
        raise Exception ("No .cabal config file found.")

    # Read the file
    with open (file, 'r') as file:
        cfgtxt = file.read()

    # Parse the file
    from jester.cabal.configparser import Parser
    parser = Parser()
    config = parser.parse(cfgtxt)
    return config

    for k, v in config.iteritems():
        print k,v

def main():
    args, _= parser.parse_known_args()
    args.func(args)
    try:
        read_jester_config()
    except:
        print "Run the 'configure' command first"
        return

main()



# def s_ident(scanner, token): return token
# def s_operator(scanner, token): return "op%s" % token
# def s_float(scanner, token): return float(token)
# def s_int(scanner, token): return int(token)



# scanner = re.Scanner([
#     (r"[a-zA-Z_]\w*", s_ident),
#     (r"\d+\.\d*", s_float),
#     (r"\d+", s_int),
#     (r"=|\+|-|\*|/", s_operator),
#     (r"\s+", None),
#     ])

# print scanner.scan("sum = 3*foo + 312.50 + bar")