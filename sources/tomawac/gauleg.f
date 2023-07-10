!                       *****************
                        SUBROUTINE GAULEG
!                       *****************
!
     &( W_LEG , X_LEG , NPOIN )
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief  COMPUTES WEIGHTS AND ABSICSSA FOR THE GAUSS-LEGENDRE QUADRATURE.
!+        THE OUTPUT ABSCISSA ARE INCLUDED BETWEEN -1 AND 1.
!+        SUBROUTINE CALLED BY PRENL3
!
!history  E. GAGNAIRE-RENOU
!+        04/2011
!+        V6P1
!+   CREATED
!
!history  G.MATTAROLO (EDF - LNHE)
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF INTEGRATION POINTS OVER OMEGA2
!| X_LEG          |<--| ABSICSSAE FOR THE GAUSS-LEGENDRE QUADRATURE
!| W_LEG          |<--| WEIGHTS FOR THE GAUSS-LEGENDRE QUADRATURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : PI
!
      USE INTERFACE_TOMAWAC, EX_GAULEG => GAULEG
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER ,INTENT(IN)             ::          NPOIN
      DOUBLE PRECISION ,INTENT(INOUT) :: W_LEG(NPOIN) , X_LEG(NPOIN)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER           I     , M     , J
      DOUBLE PRECISION  Z     , P1    , P2    , P3    , PP    , Z1
      DOUBLE PRECISION, PARAMETER  :: EPS = 3.D-14
!
      M=(NPOIN+1)/2
      DO I=1,M
        Z=COS(PI*(DBLE(I)-0.25D0)/(DBLE(NPOIN)+0.5D0))
    1   CONTINUE
        P1=1.0D0
        P2=0.0D0
        DO J=1,NPOIN
          P3=P2
          P2=P1
          P1=((2.D0*DBLE(J)-1.D0)*Z*P2-(DBLE(J)-1.D0)*P3)/DBLE(J)
        ENDDO
        PP=DBLE(NPOIN)*(Z*P1-P2)/(Z*Z-1.D0)
        Z1=Z
        Z=Z-P1/PP
        IF (ABS(Z-Z1).GT.EPS) GOTO 1
        X_LEG(I)=-Z
        X_LEG(NPOIN+1-I)=Z
        W_LEG(I)=2.D0/((1.D0-Z**2)*PP**2)
        W_LEG(NPOIN+1-I)=W_LEG(I)
      ENDDO
!
      RETURN
      END
