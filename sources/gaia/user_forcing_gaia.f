!                   ****************************
                    SUBROUTINE USER_FORCING_GAIA
!                   ****************************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Makes it possible to enforce wave conditions in order to avoid
!!       coupling with TOMAWAC
!!
!!         Impose values of :
!!         -  wave height:                     HW
!!         -  wave period:                     TW
!!         -  wave direction of propagation (deg wrt to Ox axis): THETAW
!
!>@warning  User subroutine; does nothing by default
!
!>@code{.f}
!!! EXAMPLE WITH NO WAVES:
!!
!!! AMPLITUDE = 0
!!  CALL OS('X=0     ',X=HW)
!!! PERIOD = 1 S
!!  CALL OS('X=C     ',X=TW,C=1.D0)
!!! ANGLE = 0
!!  CALL OS('X=0     ',X=THETAW)
!>@endcode
!>@warning  user_forcing_gaia is called at each time step in order to
!!          impose a variable forcing (tidal current, for example)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     REMOVE THIS RETURN IF YOU WANT TO ENTER INTO THIS USER SUBROUTINE
      IF (.TRUE.) RETURN
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
!     ------------------------
!     THE USER SHOULD BE AWARE
!     ++++++++++++++++++++++++
!
!     SUBROUTINE USER_FORCING_GAIA IS CALLED AT EACH TIME STEP
!     IN ORDER TO IMPOSE A VARIABLE FORCING
!     (TIDAL CURRENT, FOR EXAMPLE)
!
!     IT IS NOT SUFFICIENT TO PRESCRIBE THE FLOW RATE
!     THE MAIN VARIABLES ARE NOW THE 2D FLOW VELOCITY FIELD
!     AND THE FLOW DEPTH
!
!-----------------------------------------------------------------------
!
!     EXAMPLE 1:  WITH NO WAVES
!
      IF (.FALSE.) THEN
!       AMPLITUDE = 0
        CALL OS('X=0     ',X=HW)
!       PERIOD = 1 S
        CALL OS('X=C     ',X=TW,C=1.D0)
!       ANGLE = 0
        CALL OS('X=0     ',X=THETAW)

!       AFTER SETTING HWR, TWR AND THETAWR, PLEASE ADD:

        HW%TYPR    ='Q'
        TW%TYPR    ='Q'
        THETAW%TYPR='Q'

!       TO ENABLE THE CONTROL OF WAVE DATA
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE USER_FORCING_GAIA
