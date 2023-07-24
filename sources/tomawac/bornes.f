!                   *****************
                    SUBROUTINE BORNES
!                   *****************
!
     &( B     , N     , A     , XM    , X0    , X1    )
!
!***********************************************************************
! TOMAWAC   V6P1                                   08/06/2011
!***********************************************************************
!
!brief    COMPUTES THE INTEGRATION BOUNDS FOR THE INTEGRATION
!+                OF  THE FUNCTION "FONCRO", USING GAUSS QUADRATURES.
!
!history  F. BECQ (EDF/DER/LNH)
!+        26/03/96
!+        V1P1
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
!+        08/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| PARAMETER A OF THE FUNCTION TO INTGRATE
!| B              |-->| PARAMETER B OF THE FUNCTION TO INTGRATE
!| N              |-->| EXPONENT N OF THE FUNCTION TO INTGRATE
!| X0             |<--| LOWER BOUND OF THE INTERVAL
!| X1             |<--| UPPER BOUND OF THE INTERVAL
!| XM             |-->| PARAMETER XM OF THE FUNCTION TO INTGRATE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_BORNES => BORNES
      IMPLICIT NONE
!
!
!     VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      INTEGER, INTENT(IN)             :: N
      DOUBLE PRECISION, INTENT(IN)    :: B, A, XM
      DOUBLE PRECISION, INTENT(INOUT) :: X0, X1
!
!     LOCAL VARIABLES
!     """"""""""""""""""
      INTEGER  I0    , I1    , II    , JJ    , IMAX  , INP
      DOUBLE PRECISION X(11) , Y(11) , EPS   , EPS1  , DX
!
!.....EXTERNAL FUNCTIONS
!     """"""""""""""""""
!      DOUBLE PRECISION  FONCRO
!      EXTERNAL          FONCRO
!
!
      I1  = 11
      I0  = 1
      X(I0)= 0.D0
      X(I1)= 20.D0
      Y(1) = 0.D0
      EPS1 = 0.01D0
      EPS  = 0.0001D0
      INP  = 0
!
      DO II=1,20
        DX = (X(I1)-X(I0))/10.D0
        X(1) = X(I0)
        IMAX = 0
        I0   = 1
        I1   = 11
        DO JJ=2,11
          X(JJ)=X(JJ-1)+DX
          Y(JJ)=FONCRO(X(JJ),B,N,A,XM)
          IF(Y(JJ).EQ.0.D0.AND.JJ.EQ.2.AND.INP.EQ.0D0) THEN
            X(I1) = X(I1)/10.D0
            INP   = 1
            GOTO 10
          END IF
          IF(Y(JJ).LT.Y(JJ-1)) THEN
            IF(IMAX.EQ.0) THEN
              IMAX = JJ-1
              EPS  = EPS1*Y(IMAX)
            END IF
            IF (Y(JJ).LT.EPS) THEN
              I1 = JJ
              EXIT
            END IF
          ELSEIF(IMAX.EQ.0.AND.Y(JJ).LT.EPS.AND.JJ.NE.2) THEN
            I0 = JJ
          END IF
        END DO
        IF((I1-I0).GT.2) EXIT
   10   CONTINUE
      ENDDO ! II
!
      X0 = X(I0)
      X1 = X(I1)
!
      RETURN
      END
