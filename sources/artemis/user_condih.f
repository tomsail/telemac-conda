!                   **********************
                    SUBROUTINE USER_CONDIH
!                   **********************
!
!
!***********************************************************************
! ARTEMIS
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS.
!
!history  J-M HERVOUET (LNH)
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!=====================================
! === EXAMPLE OF X,Y DEPENDENT CURRENT
!=====================================
!      IF(COURANT) THEN
!
!       AMPLC=1.D0
!       DO I=1,NPOIN
!        UC%R(I)=0.D0
!        VC%R(I)=0.D0
!        IF(X(I).GE.5.D0.AND.X(I).LT.13.D0)THEN
!          UC%R(I)=AMPLC*((X(I)-5.D0)/8.D0)
!         ELSEIF(X(I).GE.13.D0)THEN
!          UC%R(I)=AMPLC
!        ENDIF
!       ENDDO
!
!      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
