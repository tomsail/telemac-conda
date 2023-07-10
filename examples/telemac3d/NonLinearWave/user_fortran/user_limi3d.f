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
      INTEGER IPTFR3
!
!-----------------------------------------------------------------------
!
!
      IF(NONHYD) THEN
        DO IPTFR3=1,NPTFR3
          PBORL%R(IPTFR3)  = 0.D0
          IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &       LIUBOL%I(IPTFR3).EQ.KENTU) THEN
            LIPBOL%I(IPTFR3) = KENT
          ELSE
            LIPBOL%I(IPTFR3) = KLOG
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
