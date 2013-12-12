GEN_SRC :=  generated/attributes.pb.cc \
			generated/crumb.pb.cc \
			generated/expr.pb.cc \
			generated/file-system.pb.cc \
			generated/graph.pb.cc \
			generated/library.pb.cc \
			generated/lit.pb.cc \
			generated/nodedefault.pb.cc \
			generated/module.pb.cc \
			generated/pat.pb.cc \
			generated/project.pb.cc \
			generated/server-api.pb.cc \
			generated/type.pb.cc
GEN_OBJ := $(patsubst %.cc,%.o, $(GEN_SRC))

.PHONY: all clean hs

all: main 

%.o: %.cc
	$(CXX) -c  $< -o $@

main.o: main.cpp
	$(CXX) -std=gnu++11 -Wall -c  $< -o $@

main: main.o $(GEN_OBJ) 
	$(CXX) $^ -o $@ -lprotobuf -pthread -lboost_system

clean:
	$(RM) -r *.o generated/*.o main