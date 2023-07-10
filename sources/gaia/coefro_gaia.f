!                   **********************
                    SUBROUTINE COEFRO_GAIA
!                   **********************
!
     &(CF,H,KS,NPOIN,KARMAN)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the quadratic friction coefficient cf.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] CF     Quadratic friction coefficient
!>@param[in]     H      Water depth
!>@param[in]     KS     Total bed roughness
!>@param[in]     NPOIN  Number of points
!>@param[in]     KARMAN Von karman constant
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN
      DOUBLE PRECISION,INTENT(IN):: KARMAN
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CF
      TYPE(BIEF_OBJ),INTENT(IN) :: KS,H
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N
      DOUBLE PRECISION AUX
      INTRINSIC MAX,LOG
!
!-----------------------------------------------------------------------
!
!  CONSTRUCTION OF THE FRICTION COEFFICIENT ACCORDING TO NIKURADSE LAW
!
      DO N=1,NPOIN
        IF(KS%R(N).LE.0.D0) THEN
          WRITE(LU,*) 'FROTTEMENT NON DEFINI DANS COEFRO AU POINT ',N
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!        AUX=30.D0/EXP(1.D0) =11.036D0
      DO N=1,NPOIN
        AUX = MAX(1.001D0,H%R(N)*11.036D0/KS%R(N))
        CF%R(N) = 2.D0 / (LOG( AUX)/KARMAN )**2
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
