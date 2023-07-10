!                   *****************
                    SUBROUTINE CALCQB
!                   *****************
!
     &(Q1,Q2,Q3)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE SQUARE ROOT OF THE TRANSCENDENT EQUATION
!+               IN QB, WHICH IS THE RATE OF BREAKING OR BROKEN WAVES.
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| Q1             |-->| INITIAL LEFT EXTREMITY OF THE SEGMENT
!| Q2             |-->| INITIAL RIGTH EXTREMITY OF THE SEGMENT
!| Q3             |<--| APPROXIMATION FOR QB
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: Q1,Q2,Q3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION FQ1,FQ2,FQ3,EPSIQB,RAP
!
      EPSIQB = 1.D-4
      RAP = Q2
!
      IF(Q2.GE.1.D0) THEN
        Q3 = 1.D0
      ELSE
        FQ3 = 1000.D0
!
! 10      FQ1 = (1.D0-Q1)+RAP*LOG(Q1)
 10     FQ1 = (1.D0-Q1)+RAP*LOG(ABS(Q1))
        FQ2 = (1.D0-Q2)+RAP*LOG(ABS(Q2))
!         FQ2 = (1.D0-Q2)+RAP*LOG(Q2)
        IF (FQ1.GE.0.D0) THEN
          Q3 = Q1
          FQ3 = EPSIQB/10.D0
        ELSE
          Q3 = Q1 - FQ1*(Q2-Q1)/(FQ2-FQ1)
          FQ3 = (1.D0-Q3)+RAP*LOG(ABS(Q3))
!            FQ3 = (1.D0-Q3)+RAP*LOG(Q3)
          IF ((FQ3*FQ1).GT.0.D0) THEN
            Q1 = Q3
          ELSE
            Q2 = Q3
          ENDIF
        ENDIF
        IF(ABS(FQ3).GE.EPSIQB) GOTO 10
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
