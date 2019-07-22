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
 
SUBROUTINE ADERVolumeIntegralNonlinear(lduh,lFhi_x,lFhi_y,lFhi_z,lShi,dx)
    USE typesDef
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE 
    ! Argument list 
    REAL, INTENT(IN)              :: lFhi_x(nVar,nDOF(1),nDOF(2),nDOF(3))    ! nonlinear flux tensor in each space-time DOF in x direction
    REAL, INTENT(IN)              :: lFhi_y(nVar,nDOF(2),nDOF(1),nDOF(3))    ! nonlinear flux tensor in each space-time DOF in y direction
    REAL, INTENT(IN)              :: lFhi_z(nVar,nDOF(3),nDOF(1),nDOF(2))    ! nonlinear flux tensor in each space-time DOF in z direction
    REAL, INTENT(IN)              :: lShi(nVar,nDOF(1),nDOF(2),nDOF(3))      ! nonlinear source term 
    REAL, INTENT(OUT)             :: lduh(nVar,nDOF(1),nDOF(2),nDOF(3))      ! spatial degrees of freedom 
    DOUBLE PRECISION, INTENT(IN)  :: dx(d)                                   ! mesh spacing
    ! Local variables 
    INTEGER           :: i,j,k,l
    REAL              :: aux(d) 
    ! 
    ! Initialize the update DOF 
    lduh = 0. 
    ! x - direction 
    DO k = 1, nDOF(3)
        DO j = 1, nDOF(2) 
            aux = (/ 1.d0, wGPN(j), wGPN(k) /) 
            lduh(:,:,j,k) = lduh(:,:,j,k) + MATMUL( lFhi_x(:,:,j,k), TRANSPOSE(Kxi) )*PRODUCT(aux(1:nDim))/dx(1)
        ENDDO
    ENDDO
    !
    ! y - direction (not needed in 1D) 
    IF(nDim>=2) THEN
        DO k = 1, nDOF(3)
            DO i = 1, nDOF(1) 
                aux = (/ 1.d0, wGPN(i), wGPN(k) /) 
                lduh(:,i,:,k) = lduh(:,i,:,k) + MATMUL( lFhi_y(:,:,i,k), TRANSPOSE(Kxi) )*PRODUCT(aux(1:nDim))/dx(2)
            ENDDO
        ENDDO
    ENDIF 
    !
    ! z - direction (node needed in 1D and 2D) 
    IF(nDim>=3) THEN
        DO j = 1, nDOF(2)
            DO i = 1, nDOF(1)
                aux = (/ 1.d0, wGPN(i), wGPN(j) /)  
                lduh(:,i,j,:) = lduh(:,i,j,:) + MATMUL( lFhi_z(:,:,i,j), TRANSPOSE(Kxi) )*PRODUCT(aux(1:nDim))/dx(3)
            ENDDO
        ENDDO
    ENDIF 
    !
    DO k = 1, nDOF(3) 
        DO j = 1, nDOF(2)
            DO i = 1, nDOF(1)
                aux = (/ wGPN(i), wGPN(j), wGPN(k) /)  
                lduh(:,i,j,k) = lduh(:,i,j,k) + PRODUCT(aux(1:nDim))*lShi(:,i,j,k)  
            ENDDO
        ENDDO
    ENDDO
    !
    !PRINT *, 'lFhi_x=', lfhi_x
    !PRINT *, 'lFhi_y=', lfhi_y
    !PRINT *, 'lFhi_z=', lfhi_z
    !PRINT *, 'lFhi_t=', lshi
    CONTINUE
    !
    !OPEN(UNIT=12, FILE="aoutput_lduh.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lduh
    !CALL EXIT    
    
    
    
END SUBROUTINE ADERVolumeIntegralNonlinear 
    
    
