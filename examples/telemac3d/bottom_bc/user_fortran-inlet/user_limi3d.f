!                   **********************
                    SUBROUTINE USER_LIMI3D
!                   **********************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    INITIALISES TYPES OF 3D BOUNDARY CONDITIONS.
!+
!+            SETS THE VALUE OF SOME COEFFICIENTS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Creation from LIMI3D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN2
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITIONS ON VELOCITIES
!     *********************************
!     BOTTOM
!     ======
!
      IF(BC_BOTTOM.EQ.1) THEN
        DO IPOIN2 = 1,NPOIN2
          IF(SQRT((X(IPOIN2)-2000.D0)**2+(Y(IPOIN2)-2000.D0)**2)
     &       .LE.50.D0) THEN
            !KENT = 5; I.E. IMPOSED FLOW RATE
            !KENTU = 6; I.E. IMPOSED VELOCITY
            LIUBOF%I(IPOIN2) = KENT
            LIVBOF%I(IPOIN2) = KENT
            LIWBOF%I(IPOIN2) = KENT
            NLIQBED%I(IPOIN2) = 1
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
