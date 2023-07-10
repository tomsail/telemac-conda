!                   ************************
                    SUBROUTINE USER_CONDIN_H
!                   ************************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS U, V
!
!warning  USER SUBROUTINE

!
!
!history  S REVILLON (STAGIAIRE)
!+        04/2020
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE TPXO
      USE OKADA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION Q_0
!
!-----------------------------------------------------------------------

!     INITIALISATION OF H, U, V
!    -------------------
!       FLUVIAL
!-------------------

      Q_0 = 0.0355D0
      DO I=1,NPOIN
        H%R(I) = 0.041D0 - ZF%R(I)
        U%R(I) = Q_0/H%R(I)
        V%R(I) = 0.0D0
      ENDDO
!
      RETURN
      END
