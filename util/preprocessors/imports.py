#!/usr/bin/python

import sys
import re

args = sys.argv[1:]
srcpath, dstpath, outputpath = args[:3]

print '%%[preprocessing] %s %s\t%% %s' %(dstpath,' '*(40-len(dstpath)), outputpath)

with open(srcpath, 'r') as f:
	src = f.read()

def process(src):
	prog = re.compile(r"(^import!\s+(?P<path>(\w+)(.(\w+))*)\s*\(\s*(?P<imports>(\w+\s*(\(\s*\.\.\s*\))?)(\s*,\s*(\w+(\(\.\.\))?))*)\s*\))", re.MULTILINE)

	matches = []
	for m in prog.finditer(src):
		print 
		path        = m.group('path')
		pathlist    = path.split('.')
		imports     = m.group('imports')
		importslist = imports.split(',')
		clsname     = pathlist[-1] 
		if clsname in importslist:
			importslist[importslist.index(clsname)] = '%s(..)'%clsname
			new_importlist = []
			new_importlist.append('import qualified %s as %s' %(path, clsname))
			new_importlist.append('import %s (%s)' %(path, ','.join(importslist)))
			matches.append((m.start(), m.end(),'\n'.join(new_importlist)))
	return matches

matches = process(src)
matches.reverse()
for match in matches:
	start,end,txt = match
	src = src[:start]+txt+src[end:]

with open(outputpath, 'w') as f:
	f.write(src)
