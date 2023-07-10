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
      INTEGER I,J,IPLAN,ITRAC,NFO1
      DOUBLE PRECISION TEMP0,TEMP1,FROUD
!
!-----------------------------------------------------------------------
!
      NFO1 = T3D_FILES(T3DFO1)%LU
      REWIND NFO1
      READ(NFO1,*) FROUD
      TEMP0 = 20.D0
      IF(ABS(FROUD-0.9D0).LT.1.D-5) THEN
        TEMP1 = 25.3485028D0
      ELSEIF (ABS(FROUD-1.6D0).LT.1.D-5) THEN
        TEMP1 = 21.8663052D0
      ELSEIF (ABS(FROUD-5.0D0).LT.1.D-5) THEN
        TEMP1 = 20.2009931D0
      ELSE
        TEMP1 = 4.D0+SQRT((TEMP0-4.D0)**2
     &                    +0.0333D0**2/(GRAV*7.D-6*0.1D0*FROUD**2))
      ENDIF

      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          DO IPLAN=1,NPLAN
            DO I=1,NPOIN2
              J=NPOIN2*(IPLAN-1)+I
              IF(IPLAN.GT.(NPLAN+1)/2) THEN
                TA%ADR(ITRAC)%P%R(J) = TEMP1
              ELSE
                TA%ADR(ITRAC)%P%R(J) = TEMP0
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
