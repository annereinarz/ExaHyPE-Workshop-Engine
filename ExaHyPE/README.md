The ExaHyPE Engine                                                 {#mainpage}
==================

This directory contains the ExaHyPE *Engine* which consists of two parts:

  1. The ExaHyPE core. This is actually a Peano project and thus mainly
     consists of Peano bindings and glue code. However, it also contains
     the crucial definitions of the Solvers such as the
     solvers::ADERDGSolver, solvers::FiniteVolumesSolver and the
     exahype::solvers::LimtingADERDGSolver. It also contains the plotters.

  2. The ExaHyPE kernels. These are mostly independent implementations
     of certain schemes in C++ and Fortran. Currently, we do have
     Finite Volume MUSCL, Godunov schemes, the ADERDG scheme, limiting
     scheme. Virtually anything related to the subcell structure of
     patches is located inside this directory.

## Further reading:

To start with ExaHyPE as a user as well as a developer, it is recommended
to read the extensive guidebook. For developers, it is highly recommended
to install an IDE and setup a basic application (typically *EulerFlow*)
as a project inside the IDE with the correct include paths (c.f. the
Makefile present in this directory). Thus one is able to explore the
rich C++ classes interactively.
