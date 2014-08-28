import re

def main():
    with open('Errors3.hs', 'r') as file:
        txt = file.read()

    pat = re.compile(r'^[ \t]*!{-#\s*LANGUAGE\s*(?P<name>[a-zA-Z]*)\s*(?P<opts>.*)#-}', re.MULTILINE)

    matches = []
    for match in pat.finditer(txt):
        name = match.group('name')
        matches.append(match)
    
    matches.reverse()

    for match in matches:
        txt = txt[:match.start()] + txt[match.end():]

    matches.reverse()

    names = [match.group('name') for match in matches]

    print(names)

main()