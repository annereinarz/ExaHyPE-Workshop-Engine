 ! 
 ! This file is part of the ExaHyPE project.
 ! Copyright (c) 2016  http://exahype.eu
 ! All rights reserved.
 !
 ! The project has received funding from the European Union's Horizon 
 ! 2020 research and innovation programme under grant agreement
 ! No 671698. For copyrights and licensing, please consult the webpage.
 !
 ! Released under the BSD 3 Open Source License.
 ! For the full license text, see LICENSE.txt
 !  
 
SUBROUTINE ADERRiemannSolverNonlinear(lQbndL,lFbndL,lQbndR,lFbndR,nv)
    USE typesDef

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE 
    ! Argument list 
    REAL, INTENT(IN)     :: lQbndL(nVar,nDOF(2),nDOF(3))              ! left space-time degrees of freedom 
    REAL, INTENT(IN)     :: lQbndR(nVar,nDOF(2),nDOF(3))              ! right space-time degrees of freedom 
    REAL, INTENT(IN)     :: nv(d)                                     ! normal vector 
    REAL, INTENT(INOUT)  :: lFbndL(nVar,nDOF(2),nDOF(3))              ! left flux 
    REAL, INTENT(INOUT)  :: lFbndR(nVar,nDOF(2),nDOF(3))              ! right flux 
    ! Local variables 
    INTEGER           :: i,j,k,l
    REAL              :: aux(d), QavL(nVar), QavR(nVar), smax
    REAL              :: LL(nVar), LR(nVar), Qav(nVar), ncp(nVar), Bn(nVar,nVar)
    
    !OPEN(UNIT=12, FILE="aoutput_lQbndL.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lQbndL

    !OPEN(UNIT=12, FILE="aoutput_lQbndR.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lQbndR
    !CALL EXIT    
    
 !   PRINT *, nv
 !   CALL EXIT    
    
    !
    ! Compute the average states from the left and the right, which we need to compute the numerical dissipation 
    QavL = 0. 
    QavR = 0. 
    DO k = 1, nDOF(3)
      DO j = 1, nDOF(2)
            aux = (/ 1.d0, wGPN(j), wGPN(k) /) 
            QavL = QavL + PRODUCT(aux(1:nDim))*lQbndL(:,j,k) 
            QavR = QavR + PRODUCT(aux(1:nDim))*lQbndR(:,j,k) 
        ENDDO
    ENDDO
    !
    Qav = 0.5*(QavL+QavR) 
    !
    ! Here, we implement a very simple Rusanov scheme with scalar dissipation (smax*Id). 
    ! We can change this into a more sophisticated Osher or HLLEM Riemann solver whenever needed! 
    !
    CALL PDEEigenvalues(LL,QavL,nv) 
    CALL PDEEigenvalues(LR,QavR,nv) 
    smax = MAX( MAXVAL(ABS(LL)), MAXVAL(ABS(LR)) ) 
    !
    ! Non-conservative product with path conservative schemes 
    CALL PDEMatrixB(Bn,Qav,nv) 
    !
    ! We now compute the numerical flux. Note that the scheme is at the moment written in 
    ! CONSERVATION FORM => no fluctuations, but real fluxes. 
    ! Later, this will be converted into the left and right fluctuations. 
    !
    DO k = 1, nDOF(3)
      DO j = 1, nDOF(2)
            lFbndL(:,j,k) = 0.5*( lFbndR(:,j,k) + lFbndL(:,j,k) ) - 0.5*smax*( lQbndR(:,j,k) - lQbndL(:,j,k) ) 
            ncp = MATMUL( Bn, lQbndR(:,j,k) - lQbndL(:,j,k) )   
            lFbndR(:,j,k) = lFbndL(:,j,k) - 0.5*ncp(:)
            lFbndL(:,j,k) = lFbndL(:,j,k) + 0.5*ncp(:)            
        ENDDO
    ENDDO
    !
    
    !PRINT *, ' --------lFbndL-ADERRiemannSolver-------------------------------- ' 
    !PRINT *, lFbndL
    !PRINT *, ' --------lFbndL-ADERRiemannSolver-------------------------------- ' 
    
    !OPEN(UNIT=12, FILE="aoutput_lFbndL.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lFbndL
    !CALL EXIT    
END SUBROUTINE ADERRiemannSolverNonlinear 