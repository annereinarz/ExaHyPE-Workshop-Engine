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
 
SUBROUTINE ADERSpaceTimePredictorLinear(lqhi,lFhi,lQbnd,lFbnd,luh,dt,dx) 
    USE typesDef

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE 
    ! Argument list 
    DOUBLE PRECISION, INTENT(IN)  :: luh(nVar,nDOF(1),nDOF(2),nDOF(3))              ! spatial degrees of freedom 
    DOUBLE PRECISION, INTENT(IN)  :: dt                                             ! 
    DOUBLE PRECISION, INTENT(IN)  :: dx(d)                                          ! 
    DOUBLE PRECISION, INTENT(OUT) :: lqhi(nVar,nDOF(1),nDOF(2),nDOF(3))             ! time-averaged space-time degrees of freedom 
    !DOUBLE PRECISION, INTENT(OUT) :: lFhi(nVar,d,nDOF(1),nDOF(2),nDOF(3))           ! time-averaged nonlinear flux tensor in each space-time DOF
    DOUBLE PRECISION, INTENT(OUT) ::lFhi(nVar,nDOF(1),nDOF(2),nDOF(3),d)
    ! shall become                   lFhi(nVar,nDOF(1),nDOF(2),nDOF(3),d)
    
    !DOUBLE PRECISION, INTENT(OUT) :: lQbnd(nVar,6,nDOF(2),nDOF(3))                  ! time-averaged space-time degrees of freedom 
    DOUBLE PRECISION, INTENT(OUT) ::  lQbnd(nVar,nDOF(2),nDOF(3),6)
    ! shall become                   lQbnd(nVar,nDOF(2),nDOF(3),6)
    !DOUBLE PRECISION, INTENT(OUT) :: lFbnd(nVar,6,nDOF(2),nDOF(3))                  ! time-averaged nonlinear flux tensor in each space-time DOF 
    DOUBLE PRECISION, INTENT(OUT) ::  lFbnd(nVar,nDOF(2),nDOF(3),6)
    ! shall become                   lFbnd(nVar,nDOF(2),nDOF(3),6)
    !
    ! Local variables 
    INTEGER :: i,j,k,l,iVar,iDim, iter 
    DOUBLE PRECISION    :: rhs0(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))               ! contribution of the initial condition to the known right hand side 
    DOUBLE PRECISION    :: rhs(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))                ! known right hand side 
    DOUBLE PRECISION    :: lqh(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0)+1)              ! space-time degrees of freedom
    ! the time DOFs should be last - otherwise we loose the block structure.
    ! the variable ordering of lqh in the linear and nonlinear case are distinct
    DOUBLE PRECISION    :: lFh(nVar,d,nDOF(1),nDOF(2),nDOF(3),nDOF(0))              ! nonlinear flux tensor in each space-time DOF (original)
    !DOUBLE PRECISION    :: lFh(nVar,nDOF(1),nDOF(2),nDOF(3),d,nDOF(0))              ! nonlinear flux tensor in each space-time DOF (changed by Ken) 
    ! lFh shall remain as it is
    DOUBLE PRECISION    :: aux(d), w                                                ! auxiliary variables 
    DOUBLE PRECISION    :: gradQ(nVar,d,nDOF(1),nDOF(2),nDOF(3),nDOF(0))            ! spatial gradient of q
    DOUBLE PRECISION    :: lqhold(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))             ! old space-time degrees of freedom 
    DOUBLE PRECISION    :: lqx(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))                ! spatial derivative qx of q 
    DOUBLE PRECISION    :: lqy(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))                ! spatial derivative qy of q 
    DOUBLE PRECISION    :: lqz(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))                ! spatial derivative qz of q 
    DOUBLE PRECISION    :: lqt(nVar,nDOF(1),nDOF(2),nDOF(3),nDOF(0))                ! time derivative qt of q 
    DOUBLE PRECISION    :: res                                                      ! residual 
    DOUBLE PRECISION    :: dtavFac                                                  ! integral average of the time power terms of the Taylor series
    DOUBLE PRECISION, PARAMETER :: tol = 1e-7                                       ! tolerance 
!
    ! Init the time derivatives with the point sources (if applicable) 
    ! 
    lqh = 0. 
    !
!    DO i = 1, nPointSource 
!        IF(iElem.EQ.PointSrc(i)%iElem) THEN
!           DO l = 1, nDOF(0)  
!            DO kk = 1, nDOF(3)
!             DO jj = 1, nDOF(2) 
!              DO ii = 1, nDOF(1) 
!               DO iVar = 1, nVar
!                  !
!                  ! Multiply source with (M)^(-1) to get the next higher time derivative
!                  !
!                  aux = (/ wGPN(ii), wGPN(jj), wGPN(kk) /)
!                  lqh(iVar,ii,jj,kk,l) = lqh(iVar,ii,jj,kk,l) + PointSrc(i)%phi(ii,jj,kk)/(PRODUCT(aux(1:nDim)))*PointSrc(i)%sigma(l,iVar)/(dt**(l-1))
!               ENDDO
!              ENDDO
!             ENDDO 
!            ENDDO 
!           ENDDO 
!        ENDIF
!    ENDDO 

    !
    ! The zeroth time derivative (time dof number 1) is the initial condition
    DO k = 1, nDOF(3)
       DO j = 1, nDOF(2)
          DO i = 1, nDOF(1)
             DO iVar = 1, nVar
                lqh(iVar,i,j,k,1) = lqh(iVar,i,j,k,1) + luh(iVar,i,j,k)
                               
             ENDDO
          ENDDO
       ENDDO
    ENDDO

     
    !
    ! For linear PDE, the fastest space-time predictor is the good old Cauchy-Kovalewski procedure
    !
    DO l = 1, nDOF(0) 
        ! Compute the derivatives in x direction (independent from the y and z derivatives)
        DO k = 1, nDOF(3)
            DO j = 1, nDOF(2)
                aux = (/ 1., wGPN(j), wGPN(k) /)
                !rhs(:,:,j,k) = - PRODUCT(aux(1:nDim))/dx(1)*MATMUL( lFh(:,1,:,j,k,l), Kxi )
                gradQ(:,1,:,j,k,l) = 1.0/dx(1)*MATMUL( lqh(:,:,j,k,l), TRANSPOSE(dudx) )         ! currently used only for debugging purposes, to check if derivatives are correctly computed

                !print *,  gradQ(:,1,:,j,k,l)
            ENDDO
         ENDDO

        ! stop
        ! y direction (independent from the x and z derivatives) - should not be used for 1D
        IF(nDim>=2) THEN
            DO k = 1, nDOF(3)
                DO i = 1, nDOF(1)
                    aux = (/ 1., wGPN(i), wGPN(k) /)
                    !rhs(:,i,:,k) = rhs(:,i,:,k) - PRODUCT(aux(1:nDim))/dx(2)*MATMUL( lFh(:,2,i,:,k,l), Kxi )
                    gradQ(:,2,i,:,k,l) = 1.0/dx(2)*MATMUL( lqh(:,i,:,k,l), TRANSPOSE(dudx) )     ! currently used only for debugging purposes, to check if derivatives are correctly computed
                    !print *,  gradQ(:,2,i,:,k,l)
                ENDDO
            ENDDO
         ENDIF

         !stop
        ! z direction (independent from the x and y derivatives) - should not be used for 1D and 2D
        IF(nDim>=3) THEN
            DO j = 1, nDOF(2)
                DO i = 1, nDOF(1)
                    aux = (/ 1., wGPN(i), wGPN(j) /)
                    !rhs(:,i,j,:) = rhs(:,i,j,:) - PRODUCT(aux(1:nDim))/dx(3)*MATMUL( lFh(:,3,i,j,:,l), Kxi )
                    gradQ(:,3,i,j,:,l) = 1.0/dx(3)*MATMUL( lqh(:,i,j,:,l), TRANSPOSE(dudx) )     ! currently used only for debugging purposes, to check if derivatives are correctly computed
                    !print *,  gradQ(:,3,i,j,:,l)
                ENDDO
            ENDDO
         ENDIF

         !stop
        ! Compute the fluxes (once these fluxes are available, the subsequent operations are independent from each other)
        DO k = 1, nDOF(3)
            DO j = 1, nDOF(2)
                DO i = 1, nDOF(1)
                    CALL PDENCP(lFh(:,:,i,j,k,l),lqh(:,i,j,k,l),gradQ(:,:,i,j,k,l))  ! PDEFlux(lFh(:,:,i,j,k,l),lqh(:,i,j,k,l),lpar(:,i,j,k))
                    lqh(:,i,j,k,l+1) = lqh(:,i,j,k,l+1) -SUM( lFh(:,1:nDim,i,j,k,l), dim=2 ) 
                ENDDO
            ENDDO
        ENDDO
        !
        CONTINUE
        !
        !
        ! Multiply with (M)^(-1) to get the next higher time derivative
        !
!        DO k = 1, nDOF(3)
!            DO j = 1, nDOF(2)
!                DO i = 1, nDOF(1)
!                    aux = (/ wGPN(i), wGPN(j), wGPN(k) /)
!                    lqh(:,i,j,k,l+1) = 1./(PRODUCT(aux(1:nDim)))*rhs(:,i,j,k)
!                    !lqt(:,i,j,k,:) = 1.0/dt*MATMUL( lqh(:,i,j,k,:), TRANSPOSE(dudx) )         ! currently used only for debugging purposes, to check if derivatives are correctly computed
!                ENDDO
!            ENDDO
!        ENDDO
        !
    ENDDO
    !
    ! Compute the fluxes also for the last time derivative 
!    l = nDOF(0) 
!    DO k = 1, nDOF(3)
!        DO j = 1, nDOF(2)
!            DO i = 1, nDOF(1)
!                CALL PDEFlux(lFh(:,:,i,j,k,l),lqh(:,i,j,k,l))
!            ENDDO
!        ENDDO
!    ENDDO
    !
    ! Immediately compute the time-averaged space-time polynomials
    ! lFh(nVar,nDOF(1),nDOF(2),nDOF(3),d,nDOF(0)),
    ! lFhi(nVar,nDOF(1),nDOF(2),nDOF(3),d)
    lqhi = lqh(:,:,:,:,1)
    lFhi(:,:,:,:,1) = lFh(:,1,:,:,:,1)
    lFhi(:,:,:,:,2) = lFh(:,2,:,:,:,1)
    lFhi(:,:,:,:,3) = lFh(:,3,:,:,:,1)
    dtavFac = 0.5*dt  
    DO l = 2, nDOF(0)
       lqhi(:,:,:,:)   = lqhi(:,:,:,:)   + dtavFac*lqh(:,:,:,:,l)
       lFhi(:,:,:,:,1) = lFhi(:,:,:,:,1) + dtavFac*lFh(:,1,:,:,:,l)
       lFhi(:,:,:,:,2) = lFhi(:,:,:,:,2) + dtavFac*lFh(:,2,:,:,:,l)
       lFhi(:,:,:,:,3) = lFhi(:,:,:,:,3) + dtavFac*lFh(:,3,:,:,:,l)
        !lFhi(:,:,:,:,:) = lFhi(:,:,:,:,:) + dtavFac*lFh(:,:,:,:,:,l)
        dtavFac = dtavFac*dt/REAL(l+1)
    ENDDO
    !
    ! Compute the bounday-extrapolated values for Q and F*n
    !
    lQbnd = 0.
    lFbnd = 0.
    !
    ! x-direction: face 1 (left) and face 2 (right)
    DO k = 1, nDOF(3)
        DO j = 1, nDOF(2)
            lQbnd(:,j,k,1) = MATMUL( lqhi(:,:,j,k),   FLCoeff )   ! left
            lQbnd(:,j,k,2) = MATMUL( lqhi(:,:,j,k),   FRCoeff )   ! right
            lFbnd(:,j,k,1) = MATMUL( lFhi(:,:,j,k,1), FLCoeff )   ! left
            lFbnd(:,j,k,2) = MATMUL( lFhi(:,:,j,k,1), FRCoeff )   ! right

        ENDDO
    ENDDO
    ! y-direction: face 3 (left) and face 4 (right)
    IF(nDim>=2) THEN
        DO k = 1, nDOF(3)
            DO i = 1, nDOF(1)
                lQbnd(:,i,k,3) = MATMUL( lqhi(:,i,:,k),   FLCoeff )   ! left
                lQbnd(:,i,k,4) = MATMUL( lqhi(:,i,:,k),   FRCoeff )   ! right
                lFbnd(:,i,k,3) = MATMUL( lFhi(:, i,:,k,2), FLCoeff )   ! left
                lFbnd(:,i,k,4) = MATMUL( lFhi(:, i,:,k,2), FRCoeff )   ! right

            ENDDO
        ENDDO
    ENDIF
    ! z-direction: face 5 (left) and face 6 (right)
    IF(nDim>=3) THEN
        DO j = 1, nDOF(2)
            DO i = 1, nDOF(1)
                lQbnd(:,i,j,5) = MATMUL( lqhi(:,i,j,:),   FLCoeff )   ! left
                lQbnd(:,i,j,6) = MATMUL( lqhi(:,i,j,:),   FRCoeff )   ! right
                lFbnd(:,i,j,5) = MATMUL( lFhi(:,i,j,:,3), FLCoeff )   ! left
                lFbnd(:,i,j,6) = MATMUL( lFhi(:,i,j,:,3), FRCoeff )   ! right
                
            ENDDO
        ENDDO
    ENDIF
    !
    CONTINUE
    !
    END SUBROUTINE ADERSpaceTimePredictorLinear
