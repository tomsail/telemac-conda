!                   **********************
                    SUBROUTINE USER_CORFON
!                   **********************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE

!
!history  S REVILLON (STAGIAIRE)
!+        04/2020
!+
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION PI
!
!
      DOUBLE PRECISION, PARAMETER :: PENTE = -0.00416D0


!
!-----------------------------------------------------------------------

!    -------------
!    PENTE DU CANAL
!    -------------
        DO I=1,NPOIN
          ZF%R(I) = PENTE*X(I)
        ENDDO


      RETURN
      END
