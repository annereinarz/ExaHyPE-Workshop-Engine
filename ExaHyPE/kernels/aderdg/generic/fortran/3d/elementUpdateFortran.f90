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
 
SUBROUTINE ElementUpdate(luh,lduh,dt)
    USE typesDef

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE 
    ! Argument list 
    REAL, INTENT(INOUT) :: lduh(nVar,nDOF(1),nDOF(2),nDOF(3))      ! spatial degrees of freedom 
    REAL, INTENT(OUT)   :: luh(nVar,nDOF(1),nDOF(2),nDOF(3))       ! nonlinear flux tensor in each space-time DOF 
    DOUBLE PRECISION, INTENT(IN)  :: dt                                             ! 
    ! Local variables 
    INTEGER             :: i,j,k,l
    REAL                :: aux(d) 
    ! 
    ! Multiply with the inverse of the mass matrix. For Gauss-Legendre nodes, we simply need to divide by the Gaussian weights 
    DO k = 1, nDOF(3)
     DO j = 1, nDOF(2) 
      DO i = 1, nDOF(1) 
        aux = (/ wGPN(i), wGPN(j), wGPN(k) /) 
        lduh(:,i,j,k) = lduh(:,i,j,k)/PRODUCT(aux(1:nDim)) 
      ENDDO
     ENDDO 
    ENDDO
    !
    ! Finally, sum the contribution to the spatial degrees of freedom 
    !
    luh = luh + dt*lduh 
    !
    !OPEN(UNIT=12, FILE="aoutput_luh.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , luh
    

    !PRINT *, ' --------lduh-ElementUpdate: dt = ' , dt
    !PRINT *, lduh
    !CALL EXIT
END SUBROUTINE ElementUpdate 
    
    