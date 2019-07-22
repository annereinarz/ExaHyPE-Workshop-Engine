Prerequisites
=============

Python
------

Python 3.3 or more is required.

Dependencies
------------

By default get its dependencies from ExaHyPE's submodules

You can install locally all the dependencies using the provided bash script:

`./CodeGenerator/importJinjaAndUseSource.sh`

You then need to adapt `./CodeGenerator/codegenerator/configuration.py` with the
correct paths to the dependencies


### Jinja2

The CodeGenerator uses the template engine Jinja2 (http://jinja.pocoo.org/)

If it is not provided by your python installation you can instead use the source 
of jinja2 directly either with the bash script or by following these steps

1) Clone the source from the git repository to your desired install directory <my-path>: 
`git clone https://github.com/pallets/jinja.git <my-path>/jinja`

2) In `ExaHyPE-Engine/CodeGenerator/codegenerator/configuration.py`, 
put the correct path to the jinja directory

3) Do the same for Markupsafe (https://pypi.org/project/MarkupSafe/), jinja's
dependency


### LIBXSMM

The CodeGenerator uses LIBXSMM to perform efficient matrux-matrix operations

You can either install it with the bash script or by following these steps

1) Clone the source from the git repository to your desired install directory <my-path>: 
`git clone -b release --single-branch https://github.com/hfp/libxsmm.git <my-path>/libxsmm`

2) Compile the source code with 
`make realclean && make generator`

3) In `ExaHyPE-Engine/CodeGenerator/codegenerator/configuration.py`, 
put the correct path to the libxsmm\_gemm\_generator


Paths
-----

Every path is relative to the root of the project (inside the ExaHyPE-Engine directory).

The CodeGenerator relies on the paths provided in `configuration.py`:


The generated code will be put accordingly to the `pathToOptKernel` argument 
starting from the internal ExaHyPe, by default in a `kernel` subdirectory of your 
application.


Codegenerator
=============

To access the help: `python3 CodeGenerator/codegenerator -h`

Usage and arguments
-------------------

positional arguments:
*  pathToApplication     path to the application as given by the ExaHyPE
                        specification file (application directory as root)
*  pathToOptKernel       desired relative path to the generated code
                        (application directory as root)
*  namespace             desired namespace for the generated code
*  solverName            name of the user-solver
*  numberOfVariables     the number of quantities
*  numberOfParameters    the number of parameters (fixed quantities)
*  order                 the order of the approximation polynomial
*  dimension             the number of spatial dimensions in the simulation (2
                        or 3)
*  numerics              linear or nonlinear
*  architecture          the microarchitecture of the target device

optional arguments:
*  -h, --help            show this help message and exit
*  --useFlux             enable flux
*  --useFluxVect         enable vectorized flux (include useFlux)
*  --useNCP              enable non conservative product
*  --useNCPVect          enable vectorized non conservative product (include
                        useNCP)
*  --useSource           enable source terms
*  --useSourceVect       enable vectorized source terms (include useSource)
*  --useFusedSource      enable fused source terms (include useSource)
*  --useFusedSourceVect  enable vectorized fused source terms (include
                        useFusedSource and useSourceVect)
*  --useMaterialParam    enable material parameters
*  --usePointSources numberOfPointSources
                        enable numberOfPointSources point sources
*  --useCERKGuess        use CERK for SpaceTimePredictor inital guess
                        (nonlinear only)
*  --useGaussLobatto     use Gauss Lobatto Quadrature instead of Gauss Legendre
*  --useLimiter numberOfObservable
                        enable limiter with the given number of observable
*  --ghostLayerWidth width
                        use limiter with the given ghostLayerWidth, requires
                        useLimiter option, default = 0


Example: `` env python3 CodeGenerator/codegenerator ./MyEuler kernels/EulerSolver MyEuler::EulerSolver_kernels::aderdg MyEuler::EulerSolver 5 0 6 3 nonlinear hsw --useFluxVect``


Data format and padding
-----------------------

OUTDATED. TODO JMG update

The Codegenerator may use padding when producing architecture specific code, it may also change the index order

Using the C index order convention with index in italic being absent in dim 2


| Array | tempArray Name | Generic | Optimised | Note |
| ----- | -------------- | ------- | --------- | ---- | 
| luh | | _nDof_, nDof, nDof, nData | _nDof_, nDof, nDof, nData | unchanged for compatibility purpose|
| lQhbnd | PEANO data | 2*nDim, _nDof_, nDof, **nData** | 2*nDim, _nDof_, nDof, **nDataPad** | 2*nDim face on the square/cube |
| lFhbnd | PEANO data | 2*nDim, _nDof_, nDof, **nVar** | 2*nDim, _nDof_, nDof, **nVarPad** | 2*nDim face on the square/cube |
| lQhi | tempUnknowns | _nDof_, nDof, nDof, **nData** | _nDof_, nDof, nDof, **nDataPad** | |
| lFhi | tempFluxUnknowns | nDim+1, _nDof_, nDof, nDof, **nVar** | nDim+1, _nDof_, nDof, nDof, **nVarPad** | lFhi has nDim+1 blocks == each directions + source |
| LQi **NL** | tempSpaceTimeUnknowns[0]  | nDof, _nDof_, nDof, nDof, **nData** | _nDof_, nDof, nDof, nDof, **nDataPad** | nonlinear case |
| LQi **Lin** | tempSpaceTimeUnknowns[0] | nDof+1, _nDof_, nDof, nDof, **nData** | nDof+1, _nDof_, nDof, nDof, **nDataPad** | linear case |
| LFi **NL** | tempSpaceTimeFluxUnknowns[0]  | nDof, _nDof_, nDof, nDof, nDim+1, **nVar** | nDim+1, nDof, _nDof_, nDof, nDof, **nVarPad** | nonlinear case, +1 for source. Move dimension coordinate to mimick lFhi blocks |
| LFi **Lin** | tempSpaceTimeFluxUnknowns[0] | 2*nDim+1, nDof, _nDof_, nDof, nDof, **nVar** | 2*nDim+1, nDof, _nDof_, nDof, nDof, **nVarPad** | linear case, +1 for source. |
| rhs | tempSpaceTimeUnknowns[1] | _nDof_, nDof, nDof, nDof, **nData** | _nDof_, nDof, nDof, nDof, **nDataPad** | nonlinear case |
| gradQ | tempSpaceTimeFluxUnknowns[1] | _nDof_, nDof, nDof, nDof, nDim, **nVar** | _nDof_, nDof, nDof, nDof, nDim, **nVarPad** | Not used if no NCP |
