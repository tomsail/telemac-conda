!                       **************************
                        SUBROUTINE CARACTERISTIQUE
!                       **************************
!
     &(X,Y,NPOIN,HFINAL,TEMPS)
!
!----------------------------------------------------------------
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: NN = 1600
      INTEGER :: NPOIN
      DOUBLE PRECISION   X(NPOIN),Y(NPOIN),HFINAL(NPOIN)
      DOUBLE PRECISION   ZF0(NN), H0(NN)
      DOUBLE PRECISION   DIST,DIST1,DIST2
      DOUBLE PRECISION   XFICTIF(NN)
      DOUBLE PRECISION   XNEW(NN)
      INTEGER            I,J,II,COMPTEUR
      DOUBLE PRECISION   GRAV,D,S,STRICKLER,CFROT,DEBIT,PI
      DOUBLE PRECISION   K1,K2,K,MOYENNE_H,TEMPS,DX
!
      DX=0.01D0
      PI= 4.D0*ATAN(1.D0)
!
      DO II=1,NN
        XFICTIF(II) = (II-1)*DX
      ENDDO
      DO II=1,NN
        H0(II)=0.D0
        ZF0(II)=0.D0
        IF(XFICTIF(II).GE. 2.D0 .AND.
     &     XFICTIF(II).LE.10.D0) THEN
          ZF0(II)=0.1D0*SIN(PI*(XFICTIF(II)-2.D0)/8.D0)**2
        ENDIF
        H0(II)=0.6D0-ZF0(II)
      ENDDO
      DO II=1,NN
        XNEW(II)=0.D0
        IF(H0(II).GE.1.D0) H0(II)=0
      ENDDO
!
! INITIALISATION DES VARIABLES
!----------------------------------------------------------------
!
      DO I=1,NPOIN
        HFINAL(I)=0.D0
      ENDDO
!
!  CALCUL DE LA HAUTEUR D'EAU MOYENNE
!----------------------------------------------------------------
!
      MOYENNE_H = 0.D0
      DO I=1,NN
        MOYENNE_H = MOYENNE_H + H0(I)
      ENDDO
      MOYENNE_H = MOYENNE_H / NN
!
!  PARAMETRES ET CONSTANTES
!----------------------------------------------------------------
!
      GRAV = 9.81D0
      D = 0.000150D0
      S = 2.65D0
      STRICKLER = 50.D0
      CFROT = 2.D0*GRAV/(STRICKLER**2*MOYENNE_H**(1.D0/3.D0))
      DEBIT = 0.25D0
      K1 = SQRT(GRAV*(S-1)*D**3)
      K2 = CFROT/(2*GRAV*(S-1)*D)
!     XKV= 1.6;  N=1-1/XKV=0.375
      K=1.6D0*0.5D0*K1*K2**(5.D0/2.D0)*DEBIT**5/CFROT
!
!  CREATION DE LA SOLUTION PAR METHODE DES CARACTERISTIQUES
!----------------------------------------------------------------
!
      DO I=1,NN
        XNEW(I) = XFICTIF(I) + K*TEMPS/H0(I)**6
      ENDDO
!
!  INTERPOLATION AVEC L'ANCIEN AXE DES ABSCISSES
!----------------------------------------------------------------
!
      COMPTEUR=0
      DO I=1,NPOIN
        COMPTEUR=0
        DO J=1,NN-1
          DIST =XNEW(J+1)-XNEW(J)
          DIST1=XNEW(J+1)-X(I)
          DIST2=X(I)-XNEW(J)
          IF(DIST1.GE.0 .AND. DIST2.GE.0 .AND.COMPTEUR.EQ.0) THEN
            HFINAL(I)=0.6D0-(DIST1*H0(J+1)+DIST2*H0(J))/DIST
            COMPTEUR=COMPTEUR+1
          ENDIF
          IF(COMPTEUR.EQ.0) HFINAL(I)=0.D0
        ENDDO
      ENDDO
!
!----------------------------------------------------------------
!
      RETURN
      END

