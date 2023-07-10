!                   *****************
                    SUBROUTINE USER_Q
!                   *****************
!
     &(I, Q)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    USER PRESCRIBES THE DISCHARGE FOR FLOW IMPOSED
!+                LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        09/01/2004
!+        V5P6
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF THE LIQUID BOUNDARY.
!| Q              |<->| DISCHARGE VALUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_USER_Q => USER_Q, EX_Q => Q
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(INOUT) :: Q
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION RAMPE1
      DOUBLE PRECISION RAMPE2
!
!-----------------------------------------------------------------------
!
      RAMPE1 =  1800.D0
      RAMPE2 =  800.D0
!
!     TEST  POUR INITIALISER LE DEBIT PARTIE DE MONTEE DE CRUE
!
!
      Q = 1950.D0 * MIN(1.D0,AT/RAMPE1) +1.D0
!
!     TEST  POUR INITIALISER LE DEBIT PARTIE STATIONNAIRE
!
      IF(AT.GT.1800.D0.AND.AT.LT.3000.D0) THEN
        Q = 1950.D0
      ENDIF
!
!     TEST POUR INITIALISER LE DEBIT PARTIE DE DECRUE
!
      IF(AT.GE.3000.D0.AND.AT.LT.3800.D0) THEN
        Q = 1950.D0*(1.D0 - MIN(1.D0,(AT-3000.D0)/RAMPE2))
      ENDIF

      Q = DEBIT(I)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
