ifeq ($(VTK_INCLUDE),)
  $(error "ERROR: Environment variable VTK_INCLUDE not set (usually something alike -L/usr/include/vtk-8.1)")
endif

ifeq ($(VTK_LIB),)
  $(error "ERROR: Environment variable VTK_LIB not set (usually -I/usr/lib64)")
endif



# Include files
SOURCES = Convert.cpp  PeanoConverter.cpp  PeanoDataSet.cpp  PeanoMetaFile.cpp  PeanoPatch.cpp  PeanoPatchData.cpp  PeanoReader.cpp  PeanoVariable.cpp


# Configure System
INCLUDE_OMP=-fopenmp
LINK_OMP=-fopenmp


default: header
	@echo Please specify your target:
	@echo clean
	@echo build-gcc
	@echo build-gcc-nofs
	@echo build-gcc-openmp
	@echo
	@echo The -nofs option is to be used if you don\'t want the tool to create directories, 
	@echo or if your compiler/system does not support the experimental filesystem extensions
	@echo of C++.


EXECUTABLE=peano-to-vtk


build-gcc: CC=g++ -std=c++0x
build-gcc: CFLAGS=
build-gcc: LFLAGS=-lstdc++fs
build-gcc: header build


build-gcc-nofs: CC=g++ -std=c++0x
build-gcc-nofs: CFLAGS=-DnoCreateDirectory
build-gcc-nofs: LFLAGS=
build-gcc-nofs: header build

build-gcc-openmp: CC=g++ -std=c++0x
build-gcc-openmp: CFLAGS=-fopenmp
build-gcc-openmp: LFLAGS=-fopenmp -lstdc++fs
build-gcc-openmp: header build


#
# Linker Settings
# ---------------
# By default, I use the compiler command. But you might wanna modify it.
LL=$(CC)


OBJECTS=$(SOURCES:.cpp=.o)


all: header build


header:
	@echo Peano block file to VTK converter
	@echo \(C\) 2018 Dan Tuthill-Jones, Tobias Weinzierl
	@echo


build: $(OBJECTS)
	$(LL) $(OBJECTS) $(VTK_LIB) -o $(EXECUTABLE) $(LFLAGS) -lvtkIOXML -lvtkCommonExecutionModel -lvtkCommonCore -lvtkCommonDataModel -lvtkFiltersCore -lvtkFiltersGeneral
	@echo
	@echo build of Peano file converter successful
	@echo visit Peano\'s homepage at http://www.peano-framework.org


clean:
	rm -f $(EXECUTABLE)
	rm -f $(OBJECTS)


$(OBJECTS): %.o : %.cpp
	$(CC) $(CFLAGS) $(VTK_INCLUDE)  -c $< -o $@
