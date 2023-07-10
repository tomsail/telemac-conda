!                       *****************
                        SUBROUTINE EXACTE
!                       *****************
!
     &(HN,U,ZF,X,NPOIN,ICONS)
!
!***********************************************************************
! PROGICIEL : 'TELEMAC'       12/12/88    F. LEPEINTRE
!                             10/02/92    J-M HERVOUET (REMPLACEMENT
!                             DE ZRPOLY PAR ZBRENT, IMPLICIT NONE ET
!                             DOUBLE PRECISION)
!                             02/03/92    F. LEPEINTRE
!***********************************************************************
!
!      FONCTION:    SOLUTION EXACTE DE L'ECOULEMENT TRANSCRITIQUE
!                   SUR UN BUMP. AVEC UN RESSAUT
!
!                   PAR CONVENTION, ZF=0. AU POINT CRITIQUE
!
!      ATTENTION : ON UTILISE ICI LE FAIT QUE LE MAILLAGE
!                  EST RECTANGULAIRE.
!
!
!      ICONS = 1 : HAUTEURS CONJUGUEES CLASSIQUES
!      ICONS = 2 : HAUTEURS CONJUGUEES "NON CONSERVATIVES"
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |     HN         |<-- |  HAUTEUR D'EAU.                              |
! |     U          |<-- |  VITESSE U.
! |     ZF         | -->|  COTE DU FOND.
! |     X          | -->|  ABCISSES DES POINTS DU MAILLAGE
! |     NPOIN      | -->|  NOMBRE DE POINTS DU MAILLAGE
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D, ONLY : GRAV
      IMPLICIT NONE
!
      DOUBLE PRECISION U(132,11),ZF(132,11),X(132,11),HN(132,11)
      DOUBLE PRECISION YVF(300),YV(300),PRAD(300),YAVAL
      DOUBLE PRECISION QFIXG,ST,PRK,XRK,YRK,XRKPR,YRKPR,YCRIT
      DOUBLE PRECISION XRKCO,YRKCO,YFLU,RES
!
      INTEGER IM,JM,I,J,ICRIT,INOEUD,NOEUD,NOEUDF,ND,NG,NRK,N,IC,NPOIN
      INTEGER ICONS
!
      DOUBLE PRECISION A(4)
      COMMON/FORFC1/A
      COMMON/HCRIT/YCRIT
!
      EXTERNAL F
      DOUBLE PRECISION F
!
      INTRINSIC REAL
!
!-----------------------------------------------------------------------
!
!     LARGEUR DU CANAL EGALE A 1.
!
!     MAILLAGE DE CARRES DECOUPES EN TRIANGLES, L'ORDRE DES POINTS EST TEL
!     QU'ON PEUT FAIRE COMME SUR UN MAILLAGE REGULIER.
      IM = 132
      JM = 11
!     POINT DE PASSAGE EN CRITIQUE (DIFFERENT DU SEUIL)
!     LE FOND A ETE CALCULE POUR AVOIR LE POINT CRITIQUE A
!     LA PREMIERE MAILLE APRES LE SEUIL.
!
      ICRIT = 63
!     COTE AVAL
      YAVAL  = 0.6D0
!     DEBIT LINEIQUE (PAR M2 DE LARGEUR)
      QFIXG  = 1.D0
!     COEFFICIENT DE STRICKLER
      ST     = 40.D0
!
      DO NOEUD=1,IM-1
        ND = NOEUD + 1
        NG = NOEUD
!       PENTE RADIER ? (AVEC UN SIGNE -)
        PRAD(NOEUD) = -(ZF(ND,5)-ZF(NG,5)) / (X(ND,5)-X(NG,5))
      ENDDO
!
!     PAR RUNGE-KUTTA (METHODE DE LA TANGENTE AMELIOREE)
!     NOMBRE DE SOUS PAS
      NRK = 10000
!     PAS DE RUNGE-KUTTA
      PRK = (X(IM,5)-X(1,5))/FLOAT(NRK-1)
!
!     ON COMMENCE PAR CALCULER LA LIGNE D'EAU FLUVIALE
!     DEPUIS L'AVAL TANT QUE Y SUPERIEUR A YCRITIQUE
!
      YCRIT=(QFIXG**2/GRAV)**(1.D0/3.D0)
      YVF(IM) = YAVAL
      XRK = X(IM,5)
      YRK = YAVAL
      IC  = IM-1
!
      DO N=1,NRK
!
!       PREDICTION
        XRKPR = XRK - PRK
        YRKPR = YRK - PRK*F(YRK,IC,QFIXG,PRAD,ST)
        IF(YRKPR.LT.YCRIT) THEN
!         CHARGE INSUFFISANTE POUR PASSER LE SEUIL EN FLUVIAL
          NOEUDF = IC+1
          GOTO 30
        ENDIF
!       CORRECTION
        XRKCO = XRKPR
        YRKCO = YRK - PRK*(F(YRK  ,IC,QFIXG,PRAD,ST) +
     &                     F(YRKPR,IC,QFIXG,PRAD,ST))*0.5D0
        IF(YRKCO.LT.YCRIT) THEN
!         CHARGE INSUFFISANTE POUR PASSER LE SEUIL EN FLUVIAL
          NOEUDF = IC+1
          GOTO 30
        ENDIF
!
!       EST-ON SORTI DE LA MAILLE COURANTE ?
        IF(XRKCO.LE.X(IC,5)) THEN
!         CALCUL DE Y AU NOEUD PAR INTERPOLATION LINEAIRE
          YVF(IC) = (YRK-YRKCO)/PRK*(X(IC,5)-XRK)+ YRK
!         CHANGEMENT DE MAILLE COURANTE
          IC = IC-1
          IF (IC.EQ.0) GOTO 40
        ENDIF
!       ACTUALISATION
        XRK = XRKCO
        YRK = YRKCO
!
        ENDDO
40      DO NOEUD=1,IM
          YV(NOEUD) = YVF(NOEUD)
        ENDDO
        GOTO 60
!
30      CONTINUE
!
!
!       CALCUL DE LA LIGNE D'EAU A PARTIR DU POINT CRITIQUE
!
!       PARTIE FLUVIALE
!
!       PAR RUNGE-KUTTA (METHODE DE LA TANGENTE AMELIOREE)
!       NOMBRE DE SOUS PAS
        NRK = 10000
!       PAS DE RUNGE-KUTTA
        PRK = (X(ICRIT,5)-X(1,5))/REAL(NRK-1)
!
        YV(ICRIT) = YCRIT
        XRK = X(ICRIT,5)
        YRK = YV(ICRIT)
        IC  = ICRIT-1
!
        DO N=1,NRK
!
!       PREDICTION
          XRKPR = XRK - PRK
          YRKPR = YRK - PRK*F(YRK,IC,QFIXG,PRAD,ST)
!       CORRECTION
          XRKCO = XRKPR
          YRKCO = YRK - PRK*(F(YRK  ,IC,QFIXG,PRAD,ST)+
     &                       F(YRKPR,IC,QFIXG,PRAD,ST))/2.D0
!
!       EST-ON SORTI DE LA MAILLE COURANTE ?
          IF (XRKCO.LE.X(IC,5)) THEN
!           CALCUL DE Y AU NOEUD PAR INTERPOLATION LINEAIRE
            YV(IC) = (YRK-YRKCO)/PRK*(X(IC,5)-XRK)+ YRK
!           CHANGEMENT DE MAILLE COURANTE
            IC = IC-1
            IF (IC.EQ.0) GOTO 80
          ENDIF
!         ACTUALISATION
          XRK = XRKCO
          YRK = YRKCO
!
          ENDDO
!
!         PARTIE TORRENTIELLE
!
80        CONTINUE
!
!         PAR RUNGE-KUTTA (METHODE DE LA TANGENTE AMELIOREE)
!         NOMBRE DE SOUS PAS
          NRK = 10000
!         PAS DE RUNGE-KUTTA
          PRK = (X(IM,5)-X(ICRIT,5))/REAL(NRK-1)
!
          XRK = X(ICRIT,5)
!         0.9999 POUR PARTIR SUR LA BONNE BRANCHE DE SOLUTION
          YRK = 0.9999D0*YCRIT
          IC  = ICRIT + 1
!
      DO N=1,NRK
!
!       PREDICTION
        XRKPR = XRK + PRK
        YRKPR = YRK + PRK*F(YRK,IC-1,QFIXG,PRAD,ST)
!       CORRECTION
        XRKCO = XRKPR
        YRKCO = YRK + PRK*(F(YRK,IC-1,QFIXG,PRAD,ST)+
     &                     F(YRKPR,IC-1,QFIXG,PRAD,ST))/2.D0
!
!       EST-ON SORTI DE LA MAILLE COURANTE
        IF (XRKCO.GE.X(IC,5)) THEN
!         CALCUL DE Y AU NOEUD PAR INTERPOLATION LINEAIRE
          YV(IC) = (YRKCO-YRK)/PRK*(X(IC,5)-XRK)+ YRK
!         CHANGEMENT DE MAILLE COURANTE
          IC = IC+1
        ENDIF
!
!       ACTUALISATION
!
        XRK = XRKCO
        YRK = YRKCO
!
      ENDDO
!
!     RECHERCHE D'UN RESSAUT
!
      IF (NOEUDF.EQ.IM) GOTO 120
      DO NOEUD=NOEUDF,IM
!       HAUTEUR CONJUGUEE DU FLUVIAL H1*H2(H1+H2)=2HC**3
        YFLU = YVF(NOEUD)
        IF(ICONS.EQ.1) THEN
          RES = (-YFLU**2+SQRT(YFLU**4+8*YFLU*YCRIT**3))/(2*YFLU)
        ELSEIF(ICONS.EQ.2) THEN
          RES = (YCRIT**3/2.D0+SQRT(YCRIT**6/4.D0
     &                   +2.D0*YFLU**3*YCRIT**3))/(2*YFLU**2)
        ELSE
          WRITE(LU,*) 'ICONS = 1 OU 2, PAS ',ICONS
          STOP
        ENDIF
        IF (RES.LE.YV(NOEUD)) THEN
          DO INOEUD=NOEUD+1,IM
            YV(INOEUD)=YVF(INOEUD)
          ENDDO
          GOTO 60
        ENDIF
      ENDDO
!     PAS DE RESSAUT
120   CONTINUE
!
60    CONTINUE
!
!-----------------------------------------------------------------------
!
      DO I=1,IM
        DO J=1,JM
          HN(I,J) = YV(I)
          U(I,J) = QFIXG/HN(I,J)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

