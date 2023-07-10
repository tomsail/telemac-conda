!                   ***************************
                    SUBROUTINE USER_CONDIN_TRAC
!                   ***************************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS TRAC
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
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
      INTEGER ITRAC,I
!
!-----------------------------------------------------------------------
!
! MODIFIED BY QINGHUI ZHANG 15 JULY 2013
! PURPOSE IS TO PRESCRIBE A SPACE-DEPENDENT INITIAL TRACER VALUES
! THE TRACER VALUES FOR LEFT BOUNDARY NODES ARE 30
! CENCENTRATION FOR THE REST PART OF LEFT BOUNDARY ELEMENTS ARE INTERPOLATED
! FOR REST PART OF THE DOMAIN, CONCENTRATION IS 0
! THE MESH ELEMENT LENGTH ALONG CHANNEL BANK IS 40 METERS
! SO HERE IF THE X COORDINATE OF NODE LESS THAN 5 METERS, INITIAL VALUE: 30
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL OS('X=0     ', X=T%ADR(ITRAC)%P)
          DO I=1,NPOIN
            IF(X(I).LT.5.D0) THEN
              T%ADR(ITRAC)%P%R(I) = 30.D0
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
