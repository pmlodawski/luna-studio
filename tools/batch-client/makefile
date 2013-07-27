GEN_SRC := main.cpp gen-cpp/*.cpp
GEN_OBJ := $(patsubst %.cpp,%.o, $(GEN_SRC))

THRIFT_DIR := /usr/local/include/thrift
BOOST_DIR := /usr/local/include

INC := -I$(THRIFT_DIR) -I$(BOOST_DIR)

.PHONY: all clean

all:  Batch_client
# Batch_server
%.o: %.cpp
	$(CXX) -Wall -DHAVE_INTTYPES_H -DHAVE_NETINET_IN_H $(INC) -c $< -o $@

# Batch_server: Batch_server.o $(GEN_OBJ)
#         $(CXX) $^ -o $@ -L/usr/local/lib -lthrift 

Batch_client: main.o $(GEN_OBJ)
	$(CXX) $^ -o $@ -L/usr/local/lib -lthrift 

clean:
	$(RM) *.o Batch_server Batch_client