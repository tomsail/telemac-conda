!                   *****************
                    SUBROUTINE TRID1D
!                   *****************
!
     &(X, AA, BB, CC, DD, GAM,IMAX)
!
!***********************************************************************
! TELEMAC3D   V7P0                                         31/07/2014
!***********************************************************************
!
!brief    SOLVES A SYSTEM OF IMAX EQUATIONS WITH UNKNOWN E
!+                AT TIME N+1.
!+
!+            METHOD KNOWN AS DOUBLE SWEEPING METHOD.
!+    cf. Numerical receipes Tridiagonal matrix solver
!
!history  C. VILLARET AND T.BENSON
!+        31/07/2014
!+        V7P0
!+ Used in the vertical settling implicit algorithm
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AA,BB,CC       |-->| MATRIX DIAGONALS (EACH IMAX IN SIZE)
!| DD             |-->| RIGHT HAND SIDE OF THE MATRIX (IMAX IN SIZE)
!| GAM            |-->| WORK ARRAY (IMAX IN SIZE)
!| IMAX           |-->| NUMBER OF 1D POINTS
!| X              |<--| SOLVED RESULT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: IMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: X(IMAX),DD(IMAX),GAM(IMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: AA(IMAX),BB(IMAX),CC(IMAX)
!
      INTEGER K
      DOUBLE PRECISION EPS
!
      X(1)=DD(1)/BB(1)
      EPS=1.D-8
!
      DO K=2,IMAX
        IF( BB(K-1).LT.EPS ) THEN
          WRITE(LU,*) 'FLOATING EXCEPTION IN TRID1D'
          CALL PLANTE(1)
          STOP
        ENDIF
        GAM(K)=CC(K-1)/BB(K-1)

        BB(K) = BB(K)- AA(K)*GAM(K)
        X(K) = (DD(K)- AA(K)*X(K-1))/BB(K)
      ENDDO
!
      DO K = IMAX-1,1,-1
        X(K) = X(K) - GAM(K+1)*X(K+1)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
