!                   *****************
                    SUBROUTINE INTANG
!                   *****************
!
     &( LAVANT, LAPRES, IDIRE , NDIRE , DELTAD)
!
!***********************************************************************
! TOMAWAC   V6P1                                   17/06/2011
!***********************************************************************
!
!brief    GETS THE ANGULAR INDICES AROUND A GIVEN DIRECTION
!+                FOR THE NON-LINEAR INTERACTION TERM, USING THE DIA
!+               ("DISCRETE INTERACTION APPROXIMATION") METHOD
!+                PROPOSED BY HASSELMANN AND HASSELMANN (1985).
!+
!+
!+            PROCEDURE SPECIFIC TO THE CASE WHERE THE DIRECTIONS
!+                ARE EVENLY DISTRIBUTED OVER [0;2.PI].
!
!note     THE DELTAD DEVIATION SHOULD BE GIVEN IN DEGREES.
!note   LAVANT AND LAPRES ARE COMPRISED BETWEEN 1 AND NDIRE.
!
!reference  HASSELMANN S., HASSELMANN K. ET AL.(1985) :
!+                     "COMPUTATIONS AND PARAMETERIZATIONS OF THE NONLINEAR
!+                      ENERGY TRANSFER IN GRAVITY-WAVE SPECTRUM. PART2 :
!+                      PARAMETERIZATIONS OF THE NONLINEAR ENERGY TRANSFER
!+                      FOR APPLICATION IN WAVE MODELS". JPO, VOL 15, PP 1378-1391.
!
!history  M. BENOIT
!+        26/06/96
!+        V1P2
!+   CREATED
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
!+        17/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DELTAD         |-->| ANGULAR DEVIATION FROM THE STARTING DIRECTION
!| IDIRE          |-->| STARTING DIRECTION INDEX
!| LAPRES         |<--| ANGULAR INDEX FOLLOWING THE DIRECTION
!| LAVANT         |<--| ANGULAR INDEX PRECEDING THE DIRECTION
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_INTANG => INTANG
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      INTEGER, INTENT(IN)          :: NDIRE , IDIRE
      DOUBLE PRECISION, INTENT(IN) :: DELTAD
      INTEGER, INTENT(INOUT)       :: LAVANT, LAPRES
!
!.....LOCAL VARIABLES
!     """"""""""""""""""
      DOUBLE PRECISION TETA  , DTETAD
!
!
      DTETAD=360.D0/DBLE(NDIRE)
      TETA=DBLE(IDIRE-1)*DTETAD+DELTAD
!
!.....TETA IS ADJUSTED TO BE COMPRISED BETWEEN 0 AND 360 DEG.
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  100 IF (TETA.GE.360.D0) THEN
        TETA=TETA-360.D0
        GOTO 100
      ENDIF
  110 IF (TETA.LT.0.D0) THEN
        TETA=TETA+360.D0
        GOTO 110
      ENDIF
!
!.....GETS THE ANGULAR INDICES PRECEDING AND FOLLOWING TETA
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""
      LAVANT=INT(TETA/DTETAD)+1
      LAPRES=LAVANT+1
      IF (LAPRES.GT.NDIRE) LAPRES=1
!
      RETURN
      END
