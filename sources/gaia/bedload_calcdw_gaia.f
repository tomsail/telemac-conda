!                   ******************************
                    SUBROUTINE BEDLOAD_CALCDW_GAIA
!                   ******************************
!
     &  (UCW, UW, TW, NPOIN, PI, UW1, UW2, TW1, TW2)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes quadratic velocities and periods
!!       (case with waves).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     NPOIN Number of points
!>@param[in]     PI    Pi
!>@param[in]     TW    Wave period
!>@param[in,out] TW1   Mid wave period, current in the wave direction
!>@param[in,out] TW2   Mid wave period, current in the opposite direction
!>@param[in]     UCW   Current projected in the wave direction
!>@param[in]     UW    Orbital wave velocity
!>@param[in,out] UW1   Work array
!>@param[in,out] UW2   Work array
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_CALCDW => BEDLOAD_CALCDW_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!     2/ GLOBAL VARIABLES
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UCW, UW, TW
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: PI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UW1, UW2, TW1, TW2
!
!     3/ LOCAL VARIABLES
!
      INTEGER                     :: I
      DOUBLE PRECISION            :: UCMOY, RAP
      DOUBLE PRECISION            :: ACOSMRAP, ACOSPRAP, SQRTRAP
      DOUBLE PRECISION, PARAMETER :: ZERO = 1.D-06
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      DO I = 1,NPOIN
        UCMOY = ABS(UCW%R(I))
        ! ****************** !
        !    I - WAVES ONLY  ! (_IMP_)
        ! ****************** !
        IF (UCMOY <= ZERO) THEN
          UW1%R(I) = UW%R(I)
          UW2%R(I) = UW%R(I)
          TW1%R(I) = TW%R(I) / 2.D0
          TW2%R(I) = TW%R(I) / 2.D0
        ELSE
          RAP = UW%R(I) / UCMOY
          ! ******************** !
          ! II - WAVES ARE PREDOMINANT ! (_IMP_)
          ! ******************** !
          IF (RAP > 1.D0) THEN
            ACOSMRAP = ACOS(-1.D0/RAP)
            ACOSPRAP = ACOS( 1.D0/RAP)
            SQRTRAP  = SQRT(1.D0-1.D0/RAP**2)
            TW1%R(I) = TW%R(I)*ACOSMRAP / PI
            TW2%R(I) = TW%R(I)*ACOSPRAP / PI
            UW1%R(I) = 2.D0*UCMOY**2 + UW%R(I)**2
     &               + 3.D0*UCMOY*UW%R(I)*SQRTRAP/ACOSMRAP
            UW1%R(I) = SQRT(UW1%R(I))
            UW2%R(I) = 2.D0*UCMOY**2 + UW%R(I)**2
     &               - 3.D0*UCMOY*UW%R(I)*SQRTRAP/ACOSPRAP
            UW2%R(I) = SQRT(UW2%R(I))
          ! ********************** !
          ! III - CURRENTS ARE PREDOMINANT ! (_IMP_)
          ! ********************** !
          ELSE
            UW1%R(I) = UCW%R(I)*SQRT(2.D0 + RAP**2)
            UW2%R(I) = ZERO
            TW1%R(I) = TW%R(I)
            TW2%R(I) = ZERO
          ENDIF
        ENDIF
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END SUBROUTINE BEDLOAD_CALCDW_GAIA
