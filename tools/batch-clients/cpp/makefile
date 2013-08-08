GEN_SRC := main.cpp \
generated/attrs_constants.cpp \
generated/attrs_types.cpp \
generated/batch_constants.cpp \
generated/Batch.cpp \
generated/batch_types.cpp \
generated/defs_constants.cpp \
generated/defs_types.cpp \
generated/graph_constants.cpp \
generated/graph_types.cpp \
generated/libs_constants.cpp \
generated/libs_types.cpp \
generated/projects_constants.cpp \
generated/projects_types.cpp \
generated/types_constants.cpp \
generated/types_types.cpp

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
	$(RM) *.o generated/*.o batch_client