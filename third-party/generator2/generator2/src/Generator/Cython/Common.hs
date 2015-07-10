module Generator.Cython.Common where

import Control.Lens ((&))

cyHeader :: String
cyHeader =
     [ "# distutils: language = c++"
     , ""
     , "from libcpp.string cimport string"
     , "from libcpp.vector cimport vector"
     , "from libc.stdint cimport int64_t"
     , "from cppstreams cimport *"
     , ""
     , ""
     ] & unlines


cppStreamsCode :: String
cppStreamsCode =
     [ "# distutils: language = c++"
     , ""
     , ""
     , "from libcpp.string cimport string"
     , "from libcpp.vector cimport vector"
     , "from libc.stdint cimport int64_t"
     , ""
     , "cdef extern from \"<memory>\" namespace \"std\":"
     , "    cdef cppclass shared_ptr[T]:"
     , "        T *get():"
     , "            pass"
     , ""
     , "# C++ streams:"
     , "cdef extern from \"<iostream>\" namespace \"std\":"
     , "    cdef cppclass ostream:"
     , "        ostream & write(const char*, int) except +"
     , "        ostream & flush() except +"
     , ""
     , "    cdef cppclass istream:"
     , "        pass"
     , ""
     , ""
     , "cdef extern from \"<iostream>\" namespace \"std::ios_base\":"
     , "    cdef cppclass open_mode:"
     , "        pass"
     , "    cdef open_mode binary"
     , ""
     , ""
     , "cdef extern from \"<fstream>\" namespace \"std\":"
     , "    cdef cppclass ofstream(ostream):"
     , "        # constructors"
     , "        ofstream(const char*) except +"
     , "        ofstream(const char*, open_mode) except+"
     , ""
     , "    cdef cppclass ifstream(istream):"
     , "        # constructors"
     , "        ifstream(const char*) except +"
     , "        ifstream(const char*, open_mode) except+"
     , ""
     , ""
     , "ctypedef istream Input"
     , "ctypedef ostream Output"
     ] & unlines
