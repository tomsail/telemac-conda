!                   ********************************
                    SUBROUTINE USER_PRERES_TELEMAC2D
!                   ********************************
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    PREPARES THE USER VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N
!
!-----------------------------------------------------------------------
!
!
!=======================================================================
! COMPUTES THE MAXIMUM ELEVATION AND ASSOCIATED TIME
!=======================================================================
!
!     Only save maximum elevation after 2 hours
      IF(AT.LE.7200.D0 .AND. LT.GT.1) THEN
        DO N=1,NPOIN
!         DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
          MAXZ%R(N) = 0.D0
          IF(SORLEO(28).OR.SORIMP(28)) TMAXZ%R(N) = AT
        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END
