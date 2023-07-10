!                   ****************************
                    SUBROUTINE USER_CONDI3D_TRAC
!                   ****************************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER INITIALISES TRACER(S)
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
      USE INTERFACE_TELEMAC3D, EX_USER_CONDI3D_TRAC => USER_CONDI3D_TRAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN,IPOIN2,IPOIN3
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
        DO IPLAN=1,NPLAN
          DO IPOIN2=1,NPOIN2
            IPOIN3 = IPOIN2 + (IPLAN-1)*NPOIN2
            IF(X(IPOIN3).LE.15.D0) TA%ADR(1)%P%R(IPOIN3) = 1.D0
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
