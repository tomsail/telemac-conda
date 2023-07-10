!                   *********************************
                    SUBROUTINE USER_CONDIH_PARTICULAR
!                   *********************************
!
!
!***********************************************************************
! ARTEMIS
!***********************************************************************
!
!brief    USER INITIALISES THE WATER DEPTH.
!
!history  J-M HERVOUET (LNH)
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      WRITE(LU,11)
11    FORMAT(1X,'CONDIH : WITH SPECIAL INITIAL CONDITIONS'
     &       ,/,'         YOU HAVE TO MODIFY USER_CONDIH_PARTICULAR')
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
