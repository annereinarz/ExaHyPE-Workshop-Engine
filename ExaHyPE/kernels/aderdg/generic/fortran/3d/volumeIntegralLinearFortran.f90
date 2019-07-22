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
 ! For the full license text, see LICENSE.tx
 !

 
SUBROUTINE ADERVolumeIntegralLinear(lduh,lFhi,dx)
    USE typesDef
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE 
    ! Argument list 
    REAL, INTENT(IN)              :: lFhi(nVar,nDOF(1),nDOF(2),nDOF(3),d)    ! nonlinear flux tensor in each space-time DOF 
    REAL, INTENT(OUT)             :: lduh(nVar,nDOF(1),nDOF(2),nDOF(3))      ! spatial degrees of freedom 
    DOUBLE PRECISION, INTENT(IN)  :: dx(d)                                   ! mesh spacing
    ! Local variables 
    INTEGER           :: i,j,k,l
    REAL              :: aux(d) 
    ! 
    ! for linear non-conservative PDE, the volume integral is trivial, since it only involves the element mass matrix, which later will cancel 
    DO k = 1, nDOF(3)
        DO j = 1, nDOF(2) 
            DO i = 1, nDOF(1) 
               aux = (/ wGPN(i), wGPN(j), wGPN(k) /)
               
                lduh(:,i,j,k) = -SUM( lFhi(:,i,j,k,1:nDim), dim = 2 ) * PRODUCT(aux(1:nDim))
            ENDDO
        ENDDO
    ENDDO 
    !
    CONTINUE
    !
!!$    OPEN(UNIT=12, FILE="aoutput.txt", ACTION="write", STATUS="replace")
!!$    WRITE(12, '(ES24.16,1x)') , lduh
!!$    !CALL EXIT    
!!$    !
END SUBROUTINE ADERVolumeIntegralLinear 
    
    
