!                   *****************************
                    DOUBLE PRECISION FUNCTION SPE
!                   *****************************
!
     &(F)
!
!***********************************************************************
! ARTEMIS   V8P0                                     Jan 2019
!***********************************************************************
!
!brief    COMPUTES THE ENERGY DENSITY BASED ON GODA.
!
!reference  "RANDOM SEA AND DESIGN OF MARITIME STRUCTURES",
!+                       UNIVERSITY OF TOKYO PRESS - 1985
!
!history  F. LEPEINTRE (LNH)
!+        01/06/1993
!+        V2P0
!+
!
!history  D. AELBRECHT (LNH)
!+        04/06/1999
!+        V5P1
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
!history  N.DURAND (HRW)
!+        Nov 2017
!+        V7P4
!+   Test on Fp/F changed to avoid computing exponential of too small
!+   negative a number (i.e. too small an exponential)
!+   Reverted back to original : Jan 2019
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F             |-->| FREQUENCY FOR WHICH ENERGY DENSITY IS CALCULATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_ARTEMIS, EX_SPE => SPE
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS, ONLY: FP,GAM,DELTA
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: F
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION SIGMA
!
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
!
      IF (F.LE.FP) THEN
        SIGMA = 0.07D0
      ELSE
        SIGMA = 0.09D0
      ENDIF
!
!     DELTA IS COMPUTED IN PERALE TO AVOID ESTIMATING WITH EVERY CALL TO SPE
!
      IF ( F.GE.1.D-4*FP) THEN
        SPE = DELTA/F**5 * EXP(-1.25D0*(FP/F)**4) *
     &         GAM** ( EXP( -0.5D0*( ( (F-FP)/(SIGMA*FP) ) **2 ) ) )
      ELSE
        SPE = 0D0
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
