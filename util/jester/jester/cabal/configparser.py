import re

def enum(*sequential, **named):
    enums = dict(zip(sequential, sequential), **named)
    return type('Enum', (), enums)

class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return 'Token(%s, %s)' % (self.type, self.value)

tokcls = enum('SECTION', 'TXT', 'SEPARATOR', 'COMMENT')

def s_section   (scanner, value): return Token(tokcls.SECTION, value[:-1].lstrip().rstrip())
def s_txt       (scanner, value): return Token(tokcls.TXT, value.lstrip().rstrip())
def s_separator (scanner, value): return Token(tokcls.SEPARATOR, value)
def s_comment   (scanner, value): return Token(tokcls.COMMENT, value)


scanner = re.Scanner([
    (r"--[^\n]*", s_comment),
    (r"[^:,\n]+\s*:", s_section),
    (r"[^:,\n\s](-?[^:,\n-]+)*", s_txt),
    (r",", s_separator),
    (r"\s+", None),
    ])

class Parser:
    def tokenize(self, s):
        tokens, rest = scanner.scan(s)
        if rest:
            raise Exception ("Parser error. Could not parse:\n"+rest)
        return tokens

    def parse(self, s):
        tokens = self.tokenize(s)
        section = []
        out = {'__global__':section}
        for token in tokens:
            if token.type == tokcls.SECTION:
                section = []
                out[token.value] = section
            elif token.type == tokcls.TXT:
                section.append(token.value)
            else:
                pass
        return out