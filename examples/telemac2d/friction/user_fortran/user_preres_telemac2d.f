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
      USE INTERFACE_TELEMAC2D, EX_USER_PRERES_TELEMAC2D =>
     &                         USER_PRERES_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!=======================================================================
! COMPUTE THE EXACT WATER DEPTH
!=======================================================================
!
      IF((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))) THEN
        CALL EXACTE(PRIVE%ADR(1)%P%R,X,NPOIN)
      ENDIF
!
!=======================================================================
! COMPUTE THE EXACT FREE SURFACE
!=======================================================================
!
      IF((LEO.AND.SORLEO(25)).OR.(IMP.AND.SORIMP(25))) THEN
        CALL OV('X=Y+Z   ' ,X=PRIVE%ADR(3)%P%R,
     &                      Y=PRIVE%ADR(1)%P%R,Z=ZF%R,
     &                      DIM1=NPOIN)
      ENDIF
!
!=======================================================================
!
      RETURN
      END
