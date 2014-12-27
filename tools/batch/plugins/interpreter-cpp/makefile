###########################################################################
## Copyright (C) Flowbox, Inc / All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2014
###########################################################################

# http://stackoverflow.com/questions/8690049/building-a-dynamic-library-with-haskell-and-using-it-from-c

all:
	ghc -c StartEnd.c
	ghc -no-hs-main main.cpp  \
	../interpreter/a.out      \
	StartEnd.o                \
	-o test -L. -lstdc++ -threaded

test: all
	LD_LIBRARY_PATH=$LD_LIBRARY_PATH:../interpreter ./test

clean:
	rm -rf test *.o