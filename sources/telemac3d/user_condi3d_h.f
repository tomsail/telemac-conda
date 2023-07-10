!                   *************************
                    SUBROUTINE USER_CONDI3D_H
!                   *************************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER INITIALISES DEPTH
!
!history  C.-T. PHAM (LNHE)
!+        24/03/2017
!+        V7P3
!+   Creation from not splitted CONDIM
!+   Called by CONDIM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_USER_CONDI3D_H => USER_CONDI3D_H
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      WRITE(LU,11)
11    FORMAT(1X,'USER_CONDI3D_H: WITH SPECIAL INITIAL CONDITIONS'
     &    ,/,1X,'                YOU HAVE TO MODIFY USER_CONDI3D_H')
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
