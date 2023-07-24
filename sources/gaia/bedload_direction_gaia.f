!                   *********************************
                    SUBROUTINE BEDLOAD_DIRECTION_GAIA
!                   *********************************
!
     &  (U2D, V2D, NPOIN, PI, THETAC)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the thetac angle (flow direction).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     NPOIN  Number of points
!>@param[in]     PI     Pi
!>@param[in]     U2D    Depth-averaged velocity x-direction
!>@param[in]     V2D    Depth-averaged velocity y-direction
!>@param[in,out] THETAC Current angle to the x axis
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,
     &    EX_BEDLOAD_DIRECTION => BEDLOAD_DIRECTION_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!     GLOBAL VARIABLES
!     -------------------
      TYPE(BIEF_OBJ),  INTENT(IN)  :: U2D, V2D
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: PI
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: THETAC
!
!     LOCAL VARIABLES
!     ------------------
      INTEGER                     :: I
      DOUBLE PRECISION, PARAMETER :: LOCAL_ZERO = 1.D-6
!
!======================================================================!
!======================================================================!
!
      DO I = 1, NPOIN
        IF (ABS(U2D%R(I)) <= LOCAL_ZERO) THEN
          IF (V2D%R(I) < = LOCAL_ZERO) THEN
            THETAC%R(I) = -PI*0.5D0
          ELSE
            THETAC%R(I) =  PI*0.5D0
          ENDIF
        ELSE
          THETAC%R(I) = ATAN(V2D%R(I) / U2D%R(I))
          IF (U2D%R(I) < 0.D0) THEN
            THETAC%R(I) = PI + THETAC%R(I)
          ENDIF
        ENDIF
      END DO
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_DIRECTION_GAIA
