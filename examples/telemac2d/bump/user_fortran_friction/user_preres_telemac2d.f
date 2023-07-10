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
      DOUBLE PRECISION HHH
!=======================================================================
! COMPUTE THE EXACT WATER DEPTH AND VELOCITY
!=======================================================================
!
!     BEWARE WE USE 23 HERE WHICH WILL EREASE 23 IN CASE OF OILSPILL!
      IF(((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))).AND.
     &   ((LEO.AND.SORLEO(24)).OR.(IMP.AND.SORIMP(24)))) THEN
!                   HAUTEUR          VITESSE
        CALL EXACTE(PRIVE%ADR(1)%P%R,PRIVE%ADR(2)%P%R,ZF%R,X,NPOIN,1)
      ENDIF
!
!=======================================================================
! COMPUTE THE EXACT FREE SURFACE
!=======================================================================
!
      IF((LEO.AND.SORLEO(25)).OR.(IMP.AND.SORIMP(25))) THEN
        CALL OV( 'X=Y+Z   ' ,PRIVE%ADR(3)%P%R,
     &                       PRIVE%ADR(1)%P%R,ZF%R,0.D0,NPOIN)
      ENDIF
!
!=======================================================================
! COMPUTE THE EXACT FROUDE NUMBER
!=======================================================================
!
      IF((LEO.AND.SORLEO(26)).OR.(IMP.AND.SORIMP(26))) THEN
        DO N=1,NPOIN
          HHH = MAX(PRIVE%ADR(1)%P%R(N),1.D-8)
          PRIVE%ADR(4)%P%R(N)=SQRT(PRIVE%ADR(2)%P%R(N)**2/(HHH*GRAV))
        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END
