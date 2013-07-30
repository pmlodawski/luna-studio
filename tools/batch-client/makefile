GEN_SRC := main.cpp \
gen-cpp/attrs_constants.cpp \
gen-cpp/attrs_types.cpp \
gen-cpp/batch_constants.cpp \
gen-cpp/Batch.cpp \
gen-cpp/batch_types.cpp \
gen-cpp/defs_constants.cpp \
gen-cpp/defs_types.cpp \
gen-cpp/graph_constants.cpp \
gen-cpp/graph_types.cpp \
gen-cpp/libs_constants.cpp \
gen-cpp/libs_types.cpp \
gen-cpp/types_constants.cpp \
gen-cpp/types_types.cpp

GEN_OBJ := $(patsubst %.cpp,%.o, $(GEN_SRC))

THRIFT_DIR := /usr/local/include/thrift
BOOST_DIR := /usr/local/include

INC := -I$(THRIFT_DIR) -I$(BOOST_DIR)

.PHONY: all clean

all:  batch_client

%.o: %.cpp 
	$(CXX) --std=c++11 -Wall -DHAVE_INTTYPES_H -DHAVE_NETINET_IN_H $(INC) -c $< -o $@

batch_client: main.o $(GEN_OBJ)
	$(CXX) $^ -o $@ -L/usr/local/lib -lthrift

clean:
	$(RM) *.o gen-cpp/*.o batch_client