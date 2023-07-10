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
      INTEGER N
      DOUBLE PRECISION COEF,ARG1
!
!=======================================================================
! COMPUTE THE EXACT WATER DEPTH
!=======================================================================
!
      COEF = SQRT(1.D0*GRAV)
      IF((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))) THEN
        DO N=1,NPOIN
          ARG1 = COEF-(X(N)-8.D0)/2.D0/MAX(AT,DT)
          PRIVE%ADR(1)%P%R(N) = MAX(0.D0,ARG1)
          PRIVE%ADR(1)%P%R(N) = 4.D0*PRIVE%ADR(1)%P%R(N)**2/9.D0/GRAV
          PRIVE%ADR(1)%P%R(N) = MIN(4.D0,PRIVE%ADR(1)%P%R(N))
        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END
