!                 ***********************************
                  SUBROUTINE USER_SUSPENSION_CAE_GAIA
!                 ***********************************
!
     &  (DCLA,NPOIN,XMVS,CSTAEQ)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief    Computation of Cae by user
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     DCLA   Sediment grain diameter
!>@param[in]     NPOIN  Number of points
!>@param[in]     XMVS   Sediment density
!>@param[in,out] CSTAEQ Equilibrium sediment concentration
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)       :: NPOIN
      DOUBLE PRECISION, INTENT(IN)       :: DCLA, XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT)    :: CSTAEQ
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                            :: IPOIN
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!======================================================================!
!
      DO IPOIN=1,NPOIN
        CSTAEQ%R(IPOIN) = 0.D0
      ENDDO
!
!  FOLLOWING LINES NEED TO BE COMMENTED OUT
!
      WRITE(LU,*) 'WITH SUSPENSION TRANSPORT FORMULA FOR ALL SANDS = 0
     &               SUSPENSION_CAE_USER_GAIA HAVE TO BE PROGRAMMED
     &               BY USER'
      CALL PLANTE(1)
      STOP
!
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE USER_SUSPENSION_CAE_GAIA
