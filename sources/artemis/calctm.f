!                   *****************
                    SUBROUTINE CALCTM
!                   *****************
!
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    COMPUTES VARIOUS ESTIMATES OF THE MEAN WAVE
!+                PERIOD :
!+                    T01 = M0/M1;
!+                    T02 = SQRT(M0/M2);
!+                    TM.
!+
!+           (DEFINITIONS IN THE LIST OF PARAMETERS ESTABLISHED
!+                BY THE IAHR)
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
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
!history  C.PEYRARD (LNHE)
!+        06/2014
!+        V7P0
!+   Modification of PONDER (done in artemis.f now)
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   Removed unnecessary references to PI and RADDEG
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION PONDER
!
!     INTRINSIC SQRT, ATAN2, MOD, ABS, COS, SIN
!
!-----------------------------------------------------------------------
!
! STRUCTURES
!
!
!-----------------------------------------------------------------------
!
      PONDER = 1.D0
!
!=======================================================================
! COMPUTES M0 MOMENTUM AND STORES IT IN T2
!=======================================================================
!
      CALL OS('X=YZ    ', X=T1 , Y=HHO  , Z=HHO)
      CALL OS('X=CX    ', X=T1 , C=PONDER)
      CALL OS('X=Y+Z   ', X=T2 , Y=HALE , Z=T1)
!
!=======================================================================
! T01 = M0 / M1 (adverage period from adverae frequency)
!=======================================================================
!
      CALL OS('X=Y     ', X=T3 , Y=T01)
      CALL OS('X=Y/Z   ', X=T01, Y=T2, Z=T3)
!
!=======================================================================
! T02 = SQRT( M0 / M2 ) (~ 0 - crossing period Tz)
!=======================================================================
!
      CALL OS('X=Y     ', X=T3 , Y=T02)
      CALL OS('X=Y/Z   ', X=T1 , Y=T2, Z=T3)
      CALL OS('X=SQR(Y)', X=T02, Y=T1)
!
!=======================================================================
! TM =  MT1 / M0 (adverage period)
!=======================================================================
!
      CALL OS('X=Y     ', X=T3 , Y=TM)
      CALL OS('X=Y/Z   ', X=TM , Y=T3 , Z=T2)
!
!
!=======================================================================
! MEAN DIRECTION: INCI
!=======================================================================
!
      CALL OS('X=A(Y,Z)', X=INCI, Y=MSIN, Z=MCOS)
!
      RETURN
      END
