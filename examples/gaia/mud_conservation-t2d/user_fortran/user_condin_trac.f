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
      INTEGER IPOIN
!
!-----------------------------------------------------------------------
!
      CALL OS( 'X=0     ' , X=T%ADR(1)%P )
      DO IPOIN=1,NPOIN
        IF((X(IPOIN)-10.05D0)**2+(Y(IPOIN)-10.05D0)**2.LT.4.D0**2)
     &  THEN
          T%ADR(1)%P%R(IPOIN) = 1.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
