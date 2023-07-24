!                   *****************
                    SUBROUTINE CQUEUE
!                   *****************
!
     &( JFRE  , JBIS  , COEF1 )
!
!***********************************************************************
! TOMAWAC   V6P1                                   14/06/2011
!***********************************************************************
!
!brief    ADJUSTS FREQUENCY INDICES AND COMPUTES TAIL
!
!note     THE SPECTRUM IS ASSUMED TO BE 0 FOR FREQUENCIES LOWER THAN
!+          THE FIRST DISCRETISED FREQUENCY.
!note   BEYOND THE LAST DISCRETISED FREQUENCY THE SPECTRUM IS
!+          ASSUMED TO DECREASE FOLLOWING A FREQ**(-TAILF) LAW.
!
!history  M. BENOIT
!+        26/06/96
!+        V1P2
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
!+        14/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COEF1          |---| MULTIPLYING COEF. F(JFRE)=COEF1*F(JBIS)
!| JBIS           |---| ADJUSTED INDEX IN THE INTERVAL [1;NF]
!| JFRE           |-->| FREQUENCY INDEX
!| NF             |-->| NUMBER OF FREQUENCIES
!| RAISF          |-->| FREQUENTIAL RATIO
!| TAILF          |-->| SPECTRUM QUEUE FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_CQUEUE => CQUEUE
      USE DECLARATIONS_TOMAWAC, ONLY : NF , RAISF , TAILF
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      INTEGER, INTENT(IN)             :: JFRE
      DOUBLE PRECISION, INTENT(INOUT) :: COEF1
      INTEGER, INTENT(INOUT)          :: JBIS
!
!
      IF (JFRE.GT.NF) THEN
        JBIS = NF
        COEF1= 1.D0/RAISF**(DBLE(JFRE-NF)*TAILF)
      ELSEIF (JFRE.LT.1) THEN
        JBIS = 1
        COEF1= 0.D0
      ELSE
        JBIS = JFRE
        COEF1= 1.D0
      ENDIF
!
      RETURN
      END
