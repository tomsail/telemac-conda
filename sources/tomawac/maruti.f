!                   *****************
                    SUBROUTINE MARUTI
!                   *****************
!
     &(NMAR,FMTMAR)
!
!***********************************************************************
! TOMAWAC   V6P3                                  21/06/2011
!***********************************************************************
!
!brief    READS THE TIDES IN A USER-DEFINED FILE FORMAT.
!
!history  F.MARCOS (LNH)
!+        30/08/95
!+        V1P0
!+
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
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF-LNHE)
!+        23/11/20012
!+        V6P3
!+   XRELV, YRELV, ZR removed, must be declared locally
!+   if necessary.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FMTMAR         |-->| TIDAL WATER LEVEL FILE FORMAT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NMAR           |-->| LOGICAL UNIT NUMBER OF TIDAL WATER LEVEL FILE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_MARUTI => MARUTI
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NMAR
      CHARACTER(LEN=8), INTENT(IN)    :: FMTMAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL USER_TIDE(NMAR,FMTMAR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
