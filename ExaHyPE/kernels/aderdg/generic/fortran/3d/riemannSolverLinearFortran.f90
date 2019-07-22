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
 
SUBROUTINE ADERRiemannSolverLinear(lQbndL,lFbndL,lQbndR, lFbndR,nv)
    USE typesDef

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    ! Argument list
    REAL, INTENT(IN)     :: lQbndL(nVar,nDOF(2),nDOF(3))              ! left space-time degrees of freedom
    REAL, INTENT(IN)     :: lQbndR(nVar,nDOF(2),nDOF(3))              ! right space-time degrees of freedom
    REAL, INTENT(IN)     :: nv(d)                                     ! normal vector
    REAL, INTENT(OUT)  :: lFbndL(nVar,nDOF(2),nDOF(3))              ! left flux
    REAL, INTENT(OUT)  :: lFbndR(nVar,nDOF(2),nDOF(3))              ! right flux
    ! Local variables
    INTEGER           :: i,j,k,l
    REAL              :: aux(d), QavL(nVar), QavR(nVar), Id(nVar,nVar), smax
    REAL              :: Bn(nVar,nVar)
    REAL              :: LL(nVar), LR(nVar)
    !
    ! Identity matrix
    Id = 0.0
    DO i=1,nVar
        Id(i,i)=1.0
    ENDDO
    !
    ! Compute the average states from the left and the right, which we need to compute the numerical dissipation
    QavL = 0.
    QavR = 0.
    DO k = 1, nDOF(3)
      DO j = 1, nDOF(2)
            aux = (/ 1., wGPN(j), wGPN(k) /)
            QavL = QavL + PRODUCT(aux(1:nDim))*lQbndL(:,j,k)
            QavR = QavR + PRODUCT(aux(1:nDim))*lQbndR(:,j,k)
        ENDDO
    ENDDO
    !
    ! If necessary, compute the left and right averages of the material parameters
    !

    !@todo anisotropic material
    !IF(nParam.GT.0) THEN
    !    parL = 0.
    !    parR = 0.
    !    DO k = 1, nDOF(3)
    !      DO j = 1, nDOF(2)
    !            aux = (/ 1., wGPN(j), wGPN(k) /)
    !            parL = parL + PRODUCT(aux(1:nDim))*lparbndL(:,j,k)
    !            parR = parR + PRODUCT(aux(1:nDim))*lparbndR(:,j,k)
    !        ENDDO
    !    ENDDO
    !ENDIF
    !
    ! Here, we implement a very simple Rusanov scheme with scalar dissipation (smax*Id).
    ! We can change this into a more sophisticated Osher or HLLEM Riemann solver whenever needed!
    !
    !CALL PDEEigenvalues(LL,QavL,parL,nv)
    !CALL PDEEigenvalues(LR,QavR,parR,nv)

    !@todo anisotropic material
    CALL PDEEigenvalues(LL,QavL,nv)
    CALL PDEEigenvalues(LR,QavR,nv)

    smax = MAX( MAXVAL(ABS(LL)), MAXVAL(ABS(LR)) )
    
    !print *, smax
    !stop
    !
    ! We now compute the numerical flux. Note that the scheme is at the moment written in
    ! CONSERVATION FORM => no fluctuations, but real fluxes.
    ! Later, this will be converted into the left and right fluctuations.
    !
    CALL PDEMatrixB(Bn,0.5*(QavL+QavR),nv) ! evaluate the system matrix just once in the averaged state

    
    
    DO k = 1, nDOF(3)
       DO j = 1, nDOF(2)
          !CALL PDEMatrixB(Bn,0.5*(QavL+QavR),nv)
          !CALL PDEMatrixB(Bn,0.5*(lQbndL(:,j,k)+lQbndR(:,j,k)),nv,0.5*(lparbndL(:,j,k)+lparbndR(:,j,k))) ! evaluate the system matrix in each Gaussian point again (slow)

          lFbndL(:,j,k) = 0.5*MATMUL(Bn - Id*smax, lQbndR(:,j,k) - lQbndL(:,j,k) )
          lFbndR(:,j,k) = 0.5*MATMUL(Bn + Id*smax, lQbndR(:,j,k) - lQbndL(:,j,k) )

          !lFbndL(:,j,k) = 0.5*( lFbndR(:,j,k) + lFbndL(:,j,k) ) - 0.5*smax*( lQbndR(:,j,k) - lQbndL(:,j,k) ) 
          !lFbndR(:,j,k) = lFbndL(:,j,k) 

          
        ENDDO
     ENDDO

    !
    !
    !PRINT *, ' --------lFbndL-ADERRiemannSolver-------------------------------- '
    !PRINT *, lFbndL
    !PRINT *, ' --------lFbndL-ADERRiemannSolver-------------------------------- '

    !OPEN(UNIT=12, FILE="aoutput_lFbndL.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lFbndL
    !CALL EXIT
    !

END SUBROUTINE ADERRiemannSolverLinear
