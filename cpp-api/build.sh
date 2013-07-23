g++ main.cpp \
-I include/  \
-I /usr/lib/ghc-7.4.1/include/  \
--std=c++11 \
'-fno-stack-protector' '-Wl,--hash-size=31' '-Wl,--reduce-memory-overheads' \
'../dist/build/luna/luna-tmp/Luna/DefManager.o' \
'../dist/build/luna/luna-tmp/Luna/Library.o' \
'../dist/build/luna/luna-tmp/Luna/System/UniPath.o' \
'../dist/build/luna/luna-tmp/Luna/Graph.o' \
'../dist/build/luna/luna-tmp/Luna/Node.o' \
'../dist/build/luna/luna-tmp/Luna/NodeDef.o' \
'../dist/build/luna/luna-tmp/Luna/Samples.o' \
'../dist/build/luna/luna-tmp/Luna/Tools/CodeGenerator.o' \
'../dist/build/luna/luna-tmp/Luna/Tools/Graphviz.o' \
'../dist/build/luna/luna-tmp/Luna/Tools/TypeChecker.o' \
'../dist/build/luna/luna-tmp/Main.o' \
'../dist/build/luna/luna-tmp/Luna/Common.o' \
'../dist/build/luna/luna-tmp/Luna/Edge.o' \
'../dist/build/luna/luna-tmp/Luna/DefaultValue.o' \
'../dist/build/luna/luna-tmp/Luna/Data/List.o' \
'-L/home/piotr/.cabal/lib/graphviz-2999.16.0.0/ghc-7.4.1' '-L/home/piotr/.cabal/lib/wl-pprint-text-1.1.0.0/ghc-7.4.1' '-L/home/piotr/.cabal/lib/temporary-1.1.2.4/ghc-7.4.1' '-L/usr/lib/ghc-7.4.1/process-1.1.0.1' '-L/home/piotr/.cabal/lib/polyparse-1.8/ghc-7.4.1' '-L/usr/lib/ghc-7.4.1/text-0.11.2.0' '-L/home/piotr/.cabal/lib/dlist-0.5/ghc-7.4.1' '-L/home/piotr/.cabal/lib/colour-2.3.3/ghc-7.4.1' '-L/home/piotr/.cabal/lib/split-0.2.2/ghc-7.4.1' '-L/home/piotr/.cabal/lib/multimap-1.2.1/ghc-7.4.1' '-L/usr/lib/ghc-7.4.1/fgl-5.4.2.4' '-L/usr/lib/ghc-7.4.1/mtl-2.1.1' '-L/usr/lib/ghc-7.4.1/transformers-0.3.0.0' '-L/usr/lib/ghc-7.4.1/directory-1.1.0.2' '-L/usr/lib/ghc-7.4.1/unix-2.5.1.0' '-L/usr/lib/ghc-7.4.1/old-time-1.1.0.0' '-L/usr/lib/ghc-7.4.1/old-locale-1.0.0.4' '-L/usr/lib/ghc-7.4.1/filepath-1.3.0.0' '-L/home/piotr/.cabal/lib/cereal-0.3.5.2/ghc-7.4.1' '-L/usr/lib/ghc-7.4.1/containers-0.4.2.1' '-L/usr/lib/ghc-7.4.1/deepseq-1.3.0.0' '-L/usr/lib/ghc-7.4.1/bytestring-0.9.2.1' '-L/usr/lib/ghc-7.4.1/array-0.4.0.0' '-L/usr/lib/ghc-7.4.1/base-4.5.0.0' '-L/usr/lib/ghc-7.4.1/integer-gmp-0.4.0.0' '-L/usr/lib/ghc-7.4.1/ghc-prim-0.2.0.0' '-L/usr/lib/ghc-7.4.1' '-lHSgraphviz-2999.16.0.0' '-lHSwl-pprint-text-1.1.0.0' '-lHStemporary-1.1.2.4' '-lHSprocess-1.1.0.1' '-lHSpolyparse-1.8' '-lHStext-0.11.2.0' '-lHSdlist-0.5' '-lHScolour-2.3.3' '-lHSsplit-0.2.2' '-lHSmultimap-1.2.1' '-lHSfgl-5.4.2.4' '-lHSmtl-2.1.1' '-lHStransformers-0.3.0.0' '-lHSdirectory-1.1.0.2' '-lHSunix-2.5.1.0' '-lrt' '-lutil' '-ldl' '-lpthread' '-lHSold-time-1.1.0.0' '-lHSold-locale-1.0.0.4' '-lHSfilepath-1.3.0.0' '-lHScereal-0.3.5.2' '-lHScontainers-0.4.2.1' '-lHSdeepseq-1.3.0.0' '-lHSbytestring-0.9.2.1' '-lHSarray-0.4.0.0' '-lHSbase-4.5.0.0' '-lHSinteger-gmp-0.4.0.0' '-lgmp' '-lHSghc-prim-0.2.0.0' '-lHSrts' '-lffi' '-lm' '-lrt' '-ldl' '-u' 'ghczmprim_GHCziTypes_Izh_static_info' '-u' 'ghczmprim_GHCziTypes_Czh_static_info' '-u' 'ghczmprim_GHCziTypes_Fzh_static_info' '-u' 'ghczmprim_GHCziTypes_Dzh_static_info' '-u' 'base_GHCziPtr_Ptr_static_info' '-u' 'base_GHCziWord_Wzh_static_info' '-u' 'base_GHCziInt_I8zh_static_info' '-u' 'base_GHCziInt_I16zh_static_info' '-u' 'base_GHCziInt_I32zh_static_info' '-u' 'base_GHCziInt_I64zh_static_info' '-u' 'base_GHCziWord_W8zh_static_info' '-u' 'base_GHCziWord_W16zh_static_info' '-u' 'base_GHCziWord_W32zh_static_info' '-u' 'base_GHCziWord_W64zh_static_info' '-u' 'base_GHCziStable_StablePtr_static_info' '-u' 'ghczmprim_GHCziTypes_Izh_con_info' '-u' 'ghczmprim_GHCziTypes_Czh_con_info' '-u' 'ghczmprim_GHCziTypes_Fzh_con_info' '-u' 'ghczmprim_GHCziTypes_Dzh_con_info' '-u' 'base_GHCziPtr_Ptr_con_info' '-u' 'base_GHCziPtr_FunPtr_con_info' '-u' 'base_GHCziStable_StablePtr_con_info' '-u' 'ghczmprim_GHCziTypes_False_closure' '-u' 'ghczmprim_GHCziTypes_True_closure' '-u' 'base_GHCziPack_unpackCString_closure' '-u' 'base_GHCziIOziException_stackOverflow_closure' '-u' 'base_GHCziIOziException_heapOverflow_closure' '-u' 'base_ControlziExceptionziBase_nonTermination_closure' '-u' 'base_GHCziIOziException_blockedIndefinitelyOnMVar_closure' '-u' 'base_GHCziIOziException_blockedIndefinitelyOnSTM_closure' '-u' 'base_ControlziExceptionziBase_nestedAtomically_closure' '-u' 'base_GHCziWeak_runFinalizzerBatch_closure' '-u' 'base_GHCziTopHandler_flushStdHandles_closure' '-u' 'base_GHCziTopHandler_runIO_closure' '-u' 'base_GHCziTopHandler_runNonIO_closure' '-u' 'base_GHCziConcziIO_ensureIOManagerIsRunning_closure' '-u' 'base_GHCziConcziSync_runSparks_closure' '-u' 'base_GHCziConcziSignal_runHandlers_closure'

# to get this command run 
# $ ghc Main.hs -v
# in src directory, see *** Linker part and change paths to *.o files
# good luck ;>
# http://stackoverflow.com/questions/3859340/calling-haskell-from-c-code