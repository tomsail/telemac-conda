!                   ***************************
                    SUBROUTINE USER_CONDI3D_KEP
!                   ***************************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER INITIALISES K AND EPSILON
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
      USE INTERFACE_TELEMAC3D, EX_USER_CONDI3D_KEP => USER_CONDI3D_KEP
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!     INTEGER I
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE K-EPSILON MODEL (OPTIONAL)
!     WHEN DONE: AKEP = .FALSE.
!
!     IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
!
!       HERE INITIALISES K AND EPSILON
!
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
