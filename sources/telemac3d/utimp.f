!                   ****************
                    SUBROUTINE UTIMP
!                   ****************
!
     & (GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    DEPRECATED. Use USER_UTIMP instead. Kept for retro
!         compatibility
!
!history  C LE NORMANT(LNH)    ; F LEPEINTRE (LNH)
!+        25/11/97
!+        V5P2
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Call of USER_UTIMP User Fortran where the modifications are done
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GRADEBL        |-->| NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
!| GRAPRDL        |-->| GRAPHIC PRINTOUT PERIOD
!| LISDEBL        |-->| NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
!| LISPRDL        |-->| LISTING PRINTOUT PERIOD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL USER_UTIMP
     & (GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
