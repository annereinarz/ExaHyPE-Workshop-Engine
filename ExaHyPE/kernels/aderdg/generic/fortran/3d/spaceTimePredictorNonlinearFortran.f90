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
 
SUBROUTINE ADERPicardLoopNonlinear(luh,dt,dx,lqh,lFh) 
    USE typesDef
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE 
    ! Argument list 
    DOUBLE PRECISION, INTENT(IN)  :: luh(nVar,nDOF(1),nDOF(2),nDOF(3))              ! spatial degrees of freedom 
    DOUBLE PRECISION, INTENT(IN)  :: dt                                             ! 
    DOUBLE PRECISION, INTENT(IN)  :: dx(d)                                          ! 
    DOUBLE PRECISION, INTENT(OUT) :: lqh(nVar,nDOF(0),nDOF(1),nDOF(2),nDOF(3))      ! space-time degrees of freedom  
    DOUBLE PRECISION, INTENT(OUT) :: lFh(nVar,d+1,nDOF(1),nDOF(2),nDOF(3),nDOF(0))    ! nonlinear flux tensor in each space-time DOF 
    ! Local variables 
    INTEGER :: i,j,k,l,iVar, iter 
    DOUBLE PRECISION    :: rhs0(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))               ! contribution of the initial condition to the known right hand side 
    DOUBLE PRECISION    :: rhs(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))                ! known right hand side 
    DOUBLE PRECISION    :: aux(d)                                                   ! auxiliary variables 
    DOUBLE PRECISION    :: lqhold(nVar,nDOF(0),nDOF(1),nDOF(2),nDOF(3))             ! old space-time degrees of freedom
    DOUBLE PRECISION    :: lqx(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))                ! spatial derivative qx of q 
    DOUBLE PRECISION    :: lqy(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))                ! spatial derivative qy of q 
    DOUBLE PRECISION    :: lqz(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))                ! spatial derivative qz of q 
    DOUBLE PRECISION    :: lqt(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))                ! time derivative qt of q 
    DOUBLE PRECISION    :: src(nVar), BgradQ(nVar), gradQ(nVar,d) 
    DOUBLE PRECISION    :: res                                                      ! residual
    DOUBLE PRECISION, PARAMETER :: tol = 1e-7                                       ! tolerance 
    !
    DO k = 1, nDOF(3) 
     DO j = 1, nDOF(2) 
      DO i = 1, nDOF(1) 
        ! Trivial initial guess (can be significantly improved) 
        DO iVar = 1, nVar
            lqh(iVar,:,i,j,k) = luh(iVar,i,j,k) 
        ENDDO 
        ! Compute the contribution of the initial condition uh to the time update. I prefer to compute it once
        ! and store it in rhs0, but if you think it is faster, you can also recompute this contribution 
        ! inside the Picard loop (DO iter = 1, N+1) 
        aux = (/ wGPN(i), wGPN(j), wGPN(k) /) 

        DO iVar = 1, nVar 
         rhs0(iVar,i,j,k,:) = PRODUCT(aux(1:nDim))*F0(:)*luh(iVar,i,j,k) 
        ENDDO
        ! 
      ENDDO 
     ENDDO
    ENDDO 
        
    ! 
    ! Discrete Picard iterations. This set of nested loops should (theoretically) be a dream for vectorization, since they are rather independent... 
    DO iter = 1, N+1   
        ! save old space-time DOF 
        !lqhold = lqh
        DO l = 1, nDOF(0) ! loop over DOF in time 
         ! Compute the fluxes (once these fluxes are available, the subsequent operations are independent from each other) 
         DO k = 1, nDOF(3) 
          DO j = 1, nDOF(2) 
           DO i = 1, nDOF(1) 
                CALL PDEFlux(lFh(:,:,i,j,k,l),lqh(:,l,i,j,k)) 
           ENDDO
          ENDDO
         ENDDO                   
         
         ! Compute the "derivatives" (contributions of the stiffness matrix) 
         ! x direction (independent from the y and z derivatives) 
        DO k = 1, nDOF(3) 
          DO j = 1, nDOF(2) 
              aux = (/ wGPN(l), wGPN(j), wGPN(k) /) 
              rhs(:,:,j,k,l) = rhs0(:,:,j,k,l) - PRODUCT(aux(1:nDim))*dt/dx(1)*MATMUL( lFh(:,1,:,j,k,l), Kxi ) 
              lqx(:,:,j,k,l) = 1.0/dx(1)*MATMUL( lqh(:,l,:,j,k), TRANSPOSE(dudx) )         ! currently used only for debugging purposes, to check if derivatives are correctly computed  
          ENDDO
         ENDDO 
         
         ! y direction (independent from the x and z derivatives) - should not be used for 1D 
         IF(nDim>=2) THEN
             DO k = 1, nDOF(3) 
              DO i = 1, nDOF(1) 
                  aux = (/ wGPN(l), wGPN(i), wGPN(k) /) 
                  rhs(:,i,:,k,l) = rhs(:,i,:,k,l) - PRODUCT(aux(1:nDim))*dt/dx(2)*MATMUL( lFh(:,2,i,:,k,l), Kxi ) 
                  lqy(:,i,:,k,l) = 1.0/dx(2)*MATMUL( lqh(:,l,i,:,k), TRANSPOSE(dudx) )     ! currently used only for debugging purposes, to check if derivatives are correctly computed 
              ENDDO
             ENDDO 
         ENDIF 
         ! z direction (independent from the x and y derivatives) - should not be used for 1D and 2D 
         IF(nDim>=3) THEN
             DO j = 1, nDOF(2) 
              DO i = 1, nDOF(1) 
                  aux = (/ wGPN(l), wGPN(i), wGPN(j) /) 
                  rhs(:,i,j,:,l) = rhs(:,i,j,:,l) - PRODUCT(aux(1:nDim))*dt/dx(3)*MATMUL( lFh(:,3,i,j,:,l), Kxi ) 
                  lqz(:,i,j,:,l) = 1.0/dx(3)*MATMUL( lqh(:,l,i,j,:), TRANSPOSE(dudx) )     ! currently used only for debugging purposes, to check if derivatives are correctly computed  
              ENDDO
             ENDDO 
         ENDIF 
         !
         DO k = 1, nDOF(3) 
          DO j = 1, nDOF(2) 
           DO i = 1, nDOF(1)
                aux = (/ wGPN(i), wGPN(j), wGPN(k) /) 
                gradQ(:,1) = lqx(:,i,j,k,l) 
                gradQ(:,2) = lqy(:,i,j,k,l) 
                gradQ(:,3) = lqz(:,i,j,k,l) 
                CALL PDESource(src,lqh(:,l,i,j,k)) 
                CALL PDENCP(BgradQ,lqh(:,l,i,j,k),gradQ) 
                lFh(:,d+1,i,j,k,l) = src - BgradQ
                rhs(:,i,j,k,l) = rhs(:,i,j,k,l) + PRODUCT(aux(1:nDim))*wGPN(l)*dt*lFh(:,d+1,i,j,k,l)   
           ENDDO
          ENDDO
         ENDDO             
         !
        ENDDO ! end loop over time DOF 
        !
        ! Multiply with (K1)^(-1) to get the discrete time integral of the discrete Picard iteration 
        ! 
        DO k = 1, nDOF(3)  
         DO j = 1, nDOF(2)  
          DO i = 1, nDOF(1)
             aux = (/ wGPN(i), wGPN(j), wGPN(k) /) 
             lqh(:,:,i,j,k) = 1./(PRODUCT(aux(1:nDim)))*MATMUL( rhs(:,i,j,k,:), TRANSPOSE(iK1) ) 
             !lqt(:,i,j,k,:) = 1.0/dt*MATMUL( lqh(:,:,i,j,k), TRANSPOSE(dudx) )         ! currently used only for debugging purposes, to check if derivatives are correctly computed  
           ENDDO
         ENDDO
        ENDDO

        !
        ! We can stop the iterations if a certain tolerance has been reached. If you do not like this unpredictable feature (it depends on the solution of the PDE) 
        ! simply comment the lines below, so each element will always do the same number of iterations in the predictor step, i.e. the same number of operations         
        !
        res = SQRT(SUM((lqh-lqhold)**2)) 
        IF(res.LT.tol) THEN
           EXIT
        ENDIF
        !
    ENDDO    
    
    !PRINT *, 'lQH=', lQH
END SUBROUTINE ADERPicardLoopNonlinear
 
    
   
SUBROUTINE ADERPredictorNonlinear(lqh,lFh,lqhi,lFhi_x,lFhi_y,lFhi_z,lShi)
    USE typesDef
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE 
    ! Argument list 
    DOUBLE PRECISION, INTENT(IN)  :: lqh(nVar,nDOF(0),nDOF(1),nDOF(2),nDOF(3))      ! space-time degrees of freedom  
    DOUBLE PRECISION, INTENT(IN)  :: lFh(nVar,d+1,nDOF(1),nDOF(2),nDOF(3),nDOF(0))    ! nonlinear flux tensor in each space-time DOF 
    DOUBLE PRECISION, INTENT(OUT) :: lqhi(nVar,nDOF(1),nDOF(2),nDOF(3))             ! time-averaged space-time degrees of freedom 
    DOUBLE PRECISION, INTENT(OUT) :: lFhi_x(nVar,nDOF(1),nDOF(2),nDOF(3))           ! time-averaged nonlinear flux tensor in each space-time DOF in x direction
    DOUBLE PRECISION, INTENT(OUT) :: lFhi_y(nVar,nDOF(2),nDOF(1),nDOF(3))           ! time-averaged nonlinear flux tensor in each space-time DOF in y direction
    DOUBLE PRECISION, INTENT(OUT) :: lFhi_z(nVar,nDOF(3),nDOF(1),nDOF(2))           ! time-averaged nonlinear flux tensor in each space-time DOF in z direction
    DOUBLE PRECISION, INTENT(OUT) :: lShi(nVar,nDOF(1),nDOF(2),nDOF(3))             ! time-averaged nonlinear source
    ! Local variables 
    INTEGER :: i,j,k,iDim
    !
    !OPEN(UNIT=12, FILE="aoutput_lqh2.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lqh
    
    !OPEN(UNIT=12, FILE="aoutput_lFh.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lFh
    !
    ! Immediately compute the time-averaged space-time polynomials 
    !
    DO k = 1, nDOF(3)  
     DO j = 1, nDOF(2)  
      DO i = 1, nDOF(1) 
         lqhi(:,i,j,k) = MATMUL( lqh(:,:,i,j,k), wGPN )
         !DO iDim = 1, nDim
         !   lFhi(:,i,j,k,iDim) = MATMUL( lFh(:,iDim,i,j,k,:), wGPN )
         !ENDDO
         lFhi_x(:,i,j,k) = MATMUL( lFh(:,1,i,j,k,:), wGPN )
         lFhi_y(:,j,i,k) = MATMUL( lFh(:,2,i,j,k,:), wGPN )
         lFhi_z(:,k,i,j) = MATMUL( lFh(:,3,i,j,k,:), wGPN )
         lShi(:,i,j,k)   = MATMUL( lFh(:,4,i,j,k,:), wGPN )
      ENDDO
     ENDDO
    ENDDO
    
    !OPEN(UNIT=12, FILE="aoutput_lqhi.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lqhi

    !OPEN(UNIT=12, FILE="aoutput_lFhi_x.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lFhi_x
    
    !OPEN(UNIT=12, FILE="aoutput_lFhi_y.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lFhi_y
    
    !OPEN(UNIT=12, FILE="aoutput_lFhi_z.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lFhi_z

    !CALL EXIT
    
    !
END SUBROUTINE ADERPredictorNonlinear



SUBROUTINE ADERExtrapolatorNonlinear(lqhi,lFhi_x,lFhi_y,lFhi_z,lQbnd,lFbnd)
    USE typesDef
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE 
    ! Argument list 
    DOUBLE PRECISION, INTENT(IN)  :: lqhi(nVar,nDOF(1),nDOF(2),nDOF(3))             ! time-averaged space-time degrees of freedom 
    DOUBLE PRECISION, INTENT(IN)  :: lFhi_x(nVar,nDOF(1),nDOF(2),nDOF(3))           ! time-averaged nonlinear flux tensor in each space-time DOF in x direction
    DOUBLE PRECISION, INTENT(IN)  :: lFhi_y(nVar,nDOF(2),nDOF(1),nDOF(3))           ! time-averaged nonlinear flux tensor in each space-time DOF in y direction
    DOUBLE PRECISION, INTENT(IN)  :: lFhi_z(nVar,nDOF(3),nDOF(1),nDOF(2))           ! time-averaged nonlinear flux tensor in each space-time DOF in z direction
    DOUBLE PRECISION, INTENT(OUT) :: lQbnd(nVar,nDOF(2),nDOF(3),6)                  ! time-averaged space-time degrees of freedom 
    DOUBLE PRECISION, INTENT(OUT) :: lFbnd(nVar,nDOF(2),nDOF(3),6)                  ! time-averaged nonlinear flux tensor in each space-time DOF 
    ! Local variables 
    INTEGER :: i,j,k
    ! 
    !
    ! Compute the bounday-extrapolated values for Q and F*n
    !
    lQbnd = 0. 
    lFbnd = 0. 
    ! x-direction: face 1 (left) and face 2 (right) 
    DO k = 1, nDOF(3) 
     DO j = 1, nDOF(2) 
        lQbnd(:,j,k,1) = MATMUL( lqhi(:,:,j,k),   FLCoeff )   ! left 
        lQbnd(:,j,k,2) = MATMUL( lqhi(:,:,j,k),   FRCoeff )   ! right 
        lFbnd(:,j,k,1) = MATMUL( lFhi_x(:,:,j,k), FLCoeff )   ! left 
        lFbnd(:,j,k,2) = MATMUL( lFhi_x(:,:,j,k), FRCoeff )   ! right 
     ENDDO
    ENDDO 
    ! y-direction: face 3 (left) and face 4 (right) 
    IF(nDim>=2) THEN
        DO k = 1, nDOF(3) 
         DO i = 1, nDOF(1) 
            lQbnd(:,i,k,3) = MATMUL( lqhi(:,i,:,k),   FLCoeff )   ! left 
            lQbnd(:,i,k,4) = MATMUL( lqhi(:,i,:,k),   FRCoeff )   ! right 
            lFbnd(:,i,k,3) = MATMUL( lFhi_y(:,:,i,k), FLCoeff )   ! left 
            lFbnd(:,i,k,4) = MATMUL( lFhi_y(:,:,i,k), FRCoeff )   ! right 
         ENDDO
        ENDDO 
    ENDIF    
    ! z-direction: face 5 (left) and face 6 (right) 
    IF(nDim>=3) THEN
        DO j = 1, nDOF(2) 
         DO i = 1, nDOF(1) 
            lQbnd(:,i,j,5) = MATMUL( lqhi(:,i,j,:),   FLCoeff )   ! left 
            lQbnd(:,i,j,6) = MATMUL( lqhi(:,i,j,:),   FRCoeff )   ! right 
            lFbnd(:,i,j,5) = MATMUL( lFhi_z(:,:,i,j), FLCoeff )   ! left
            lFbnd(:,i,j,6) = MATMUL( lFhi_z(:,:,i,j), FRCoeff )   ! right
         ENDDO
        ENDDO 
    ENDIF       
    !
    !OPEN(UNIT=12, FILE="aoutput_lFbnd.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lFbnd

    !OPEN(UNIT=12, FILE="aoutput_lQbnd.txt", ACTION="write", STATUS="replace")
    !WRITE(12, '(ES24.16,1x)') , lQbnd
    !CALL EXIT    

END SUBROUTINE ADERExtrapolatorNonlinear
    
    
