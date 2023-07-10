!                   *****************
                    SUBROUTINE CALCMN
!                   *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES APPROXIMATE VALUES FOR THE MOMENTUMS M0, M1,
!+                M2 OF THE WAVE SPECTRUM TO CALCULATE THE MEAN PERIOD
!+                AND DIRECTION.
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
      DOUBLE PRECISION FREQ, FREQ2, PONDER
!
!-----------------------------------------------------------------------
!
! STRUCTURES
!
!-----------------------------------------------------------------------
!
      FREQ = 1.D0/PER
      FREQ2 = FREQ * FREQ
!      PONDER = 1.D0/DBLE(NDALE*NPALE)
      PONDER= 1D0
!
!=======================================================================
! M1 = INTEGRAL OF ( F * S(F) * DF )
!=======================================================================
!
      CALL OS('X=YZ    ', X=T1 , Y=HHO  , Z=HHO)
      CALL OS('X=CY    ', X=T2, Y=T1, C=FREQ)
      CALL OS('X=CX    ', X=T2, C=PONDER)
      CALL OS('X=X+Y   ', X=T01, Y=T2)
!
!=======================================================================
! M2 = INTEGRAL OF ( F**2 * S(F) * DF )
!=======================================================================
!
      CALL OS( 'X=CY    ', X=T2, Y=T1  , C=FREQ2 )
      CALL OS( 'X=CX    ', X=T2, C=PONDER )
      CALL OS( 'X=X+Y   ', X=T02, Y=T2)
!
!=======================================================================
! MT1 = INTEGRAL OF ( T * S(F) * DF )
!=======================================================================
!
      CALL OS( 'X=CY    ', X=T2 , Y=T1  , C=PER )
      CALL OS( 'X=CX    ', X=T2 , C=PONDER )
      CALL OS( 'X=X+Y   ', X=TM , Y=T2)
!
!=======================================================================
! MCOS = INTEGRAL OF ( COS(INCI) * S(F) * DF )
!=======================================================================
!
      CALL OS( 'X=COS(Y)',  X=T2 , Y=INCI)
      CALL OS( 'X=CXY   ',  X=T2 , Y=T1  , C=PONDER )
      CALL OS( 'X=X+Y   ', X=MCOS, Y=T2  )
!
!=======================================================================
! MSIN = INTEGRAL OF ( SIN(INCI) * S(F) * DF )
!=======================================================================
!
      CALL OS('X=SIN(Y)',  X=T2 , Y=INCI)
      CALL OS('X=CXY   ',  X=T2 , Y=T1   , C=PONDER )
      CALL OS('X=X+Y   ', X=MSIN, Y=T2)
!
      RETURN
      END
