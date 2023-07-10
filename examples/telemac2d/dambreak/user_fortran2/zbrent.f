!                       *****************
                        SUBROUTINE ZBRENT
!                       *****************
!
     &(FC1,EPS,X1,X2,ITMAX)
!
!***********************************************************************
! BIEF VERSION 3.0           18/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!  FONCTION  :  SOLUTION D'UNE EQUATION DONT UN ZERO UNIQUE EST ENTRE
!               LES POINTS X1 ET X2.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   FC1          | -->| FONCTION DONT ON CHERCHE LE ZERO
! |                |    | DOIT ETRE DEFINIE EN DOUBLE PRECISION
! |                |    | PAR AILLEURS.
! |   EPS          | -->| PRECISION CHERCHEE.
! |   X1,X2        | -->| ENCADREMENT DE LA SOLUTION ENTREE
! |                |<-->| X2 = SOLUTION EN SORTIE.
! |   ITMAX        | -->| NOMBRE MAXIMUM D'ITERATIONS.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!  FONCTION APPELEE : FC1
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION A,B,C,D,E,X1,X2,FA,FB,FC,EPS,EPS2,XM,S,P,Q,R
!
      INTEGER ITMAX,ITER
!
      DOUBLE PRECISION FC1
      EXTERNAL FC1
!
      INTRINSIC ABS,SIGN,MIN
!
!-----------------------------------------------------------------------
!
!  ON VERIFIE QU'ON ENCADRE BIEN LA SOLUTION :
!
      A=X1
      B=X2
      FA=FC1(A)
      FB=FC1(B)
      IF(FB*FA.GT.0.D0) THEN
        IF (LNG.EQ.1) WRITE(LU,*) 'ZBRENT : FC1(X1)*FC1(X2) EST POSITIF'
        IF (LNG.EQ.2) WRITE(LU,*) 'ZBRENT : ROOT MUST BE BRACKETED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!  ITERATIONS :
!
      FC=FB
      DO ITER=1,ITMAX
        IF(FB*FC.GT.0.D0) THEN
          C=A
          FC=FA
          D=B-A
          E=D
        ENDIF
        IF(ABS(FC).LT.ABS(FB)) THEN
          A=B
          B=C
          C=A
          FA=FB
          FB=FC
          FC=FA
        ENDIF
        EPS2=0.5D0*EPS
        XM=0.5D0*(C-B)
        IF(ABS(XM).LE.EPS2.OR.FB.EQ.0.D0)THEN
          X2=B
          RETURN
        ENDIF
        IF(ABS(E).GE.EPS2.AND.ABS(FA).GT.ABS(FB)) THEN
          S=FB/FA
          IF(A.EQ.C) THEN
            P=2.D0*XM*S
            Q=1.D0-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.D0*XM*Q*(Q-R)-(B-A)*(R-1.D0))
            Q=(Q-1.D0)*(R-1.D0)*(S-1.D0)
          ENDIF
          IF(P.GT.0.D0) Q=-Q
          P=ABS(P)
          IF(2*P.LT.MIN(3.D0*XM*Q-ABS(EPS2*Q),ABS(E*Q))) THEN
            E=D
            D=P/Q
          ELSE
            D=XM
            E=D
          ENDIF
        ELSE
          D=XM
          E=D
        ENDIF
        A=B
        FA=FB
        IF(ABS(D).GT.EPS2) THEN
          B=B+D
        ELSE
          B=B+SIGN(EPS2,XM)
        ENDIF
        FB=FC1(B)
      ENDDO
!
      IF (LNG.EQ.1) WRITE(LU,*) 'ZBRENT : MAXIMUM D''ITERATIONS ATTEINT'
      IF (LNG.EQ.2) WRITE(LU,*) 'ZBRENT : EXCEEDING MAXIMUM ITERATIONS'
      X2=B
!
!-----------------------------------------------------------------------
!
      RETURN
      END

