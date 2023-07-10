!                   ********************************
                    SUBROUTINE USER_PRERES_TELEMAC2D
!                   ********************************
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    PREPARES THE USER VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
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
      DOUBLE PRECISION COEF,ARG1
!
!-----------------------------------------------------------------------
!
!=======================================================================
! COMPUTE THE EXACT WATER DEPTH
!=======================================================================
!
      COEF = SQRT(1.D0*GRAV)
      IF((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))) THEN
        DO I=1,NPOIN
          ARG1 = COEF-(X(I)-8.D0)/2.D0/MAX(AT,DT)
          PRIVE%ADR(1)%P%R(I) = MAX(0.D0,ARG1)
          PRIVE%ADR(1)%P%R(I) = 4.D0*PRIVE%ADR(1)%P%R(I)**2/9.D0/GRAV
          PRIVE%ADR(1)%P%R(I) = MIN(4.D0,PRIVE%ADR(1)%P%R(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
