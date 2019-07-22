## Prepare project ##

- Create links to peano and tarch in the directory
ln -s .../peano/src/tarch
ln -s .../peano/src/peano

- Setup project (cf guidebook)
java -jar .../peano/pdt/pdt.jar --create-project petsc petsc

- Generate gluecode 
java -jar .../peano/pdt/pdt.jar --generate-gluecode petsc/project.peano-specification petsc .../peano/pdt/usrtemplates




## Compile ##

- It is literally only 
make -f petsc/makefile

- On many (non HPC) systems, PETSc is not in the path and you might have to add it manually alike
export PATH=$PATH:/opt/mpi/bin

- Then you can compile 
make -f petsc/makefile clean
make -f petsc/makefile 

- You might have to adopt the pathes in the makefile if PETSc is not installed in /opt/petsc




## Run ##

To make the run succeed, you might have to add PETSc's library to the search
path:

export LD_LIBRARY_PATH=/opt/petsc/lib




## Cleanup ##

rm petsc/State.* petsc/Cell.* petsc/VertexOperations.*
rm *.vtk peano-*
rm -rf petsc/adapters
rm -rf petsc/dastgen
rm -rf petsc/records
rm -rf petsc/repositories
rm -rf petsc/tests
rm -rf petsc/runners/Runner.h
rm -rf petsc/runners/RunnerParallelWorker.cpp
rm -rf petsc/mappings/Assemble.h
rm -rf petsc/mappings/CreateGrid.h


tar -czvf chapter-4.3.tar.gz --exclude=*.o chapter-3.4/petsc chapter-3.4/readme.txt 


