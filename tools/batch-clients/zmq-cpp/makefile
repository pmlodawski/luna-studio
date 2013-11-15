GEN_SRC := generated/server-api.pb.cc
GEN_OBJ := $(patsubst %.cc,%.o, $(GEN_SRC))

.PHONY: all clean hs gen

all: main 

%.o: %.cpp %.cc
	$(CXX) --std=c++11 -Wall -c $< -o $@

main: main.o $(GEN_OBJ) 
	$(CXX) $^ -o $@ -lprotobuf -lzmq

clean:
	$(RM) -r *.o generated/*.o main