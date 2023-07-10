!                   *****************************
                    DOUBLE PRECISION FUNCTION SPD
!                   *****************************
!
     &(TETA)
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    COMPUTES THE ENERGY DENSITY BASED ON GODA.
!
!reference  "RANDOM SEA AND DESIGN OF MARITIME STRUCTURES",
!+                       UNIVERSITY OF TOKYO PRESS - 1985
!
!code
!+ SPD(TETA) = COS( (TETA)/2 )**(2*EXPO)
!+
!+
!+ WHERE TETA IS THE WAVE PROPAGATION ANGLE
!+       (THE MAIN PROPAGATION DIRECTION IS TETA=0)
!+       EXPO IS AN EXPONENT SPECIFIED BY THE USER
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
!+        August 2017
!+        V7P3
!+   DEGRAD now defined in DECLARATIONS_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TETA           |-->| ANGLE FOR CONSIDERED WAVE PROPAGATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_ARTEMIS, EX_SPD => SPD
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS, ONLY: EXPO, DEGRAD
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      DOUBLE PRECISION, INTENT(IN) :: TETA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
      INTRINSIC COS
!
!-----------------------------------------------------------------------
!
!      EXPO=10.
      SPD = COS ( TETA*DEGRAD / 2.D0 )**(2*EXPO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
