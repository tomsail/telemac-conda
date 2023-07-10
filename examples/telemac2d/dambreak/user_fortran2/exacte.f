!                       *****************
                        SUBROUTINE EXACTE
!                       *****************
!
     &(H,U,V,YG,YD,X,T,NPOIN)
!
!***********************************************************************
! PROGICIEL : TELEMAC        07/12/88    F. LEPEINTRE (LNH) 30 71 80 18
!
!***********************************************************************
!
!     FONCTION  : SOLUTION EXACTE DE LA RUPTURE DE BARRAGE SUR
!                 FOND MOUILLE, DANS UN RECTANGLE.
!
!     ATTENTION AUX CONVENTIONS :    ******
!                                    *    *     ^
!                                    *    *     |
!                                    *    * JM  | X
!                                    *    *     |
!                                    *    *     |
!                                    *    *     |
!                                    ******     |
!                                      IM
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   H            |<-- | HAUTEUR D'EAU.
! |   U            |<-- | VITESSE EXACTE SELON X.
! |   V            |<-- | VITESSE EXACTE SELON Y.
! |   YG           | -->| HAUTEUR D'EAU A GAUCHE DU BARRAGE.
! |   YD           | -->| HAUTEUR D'EAU A DROITE DU BARRAGE.
! |   X            | -->| COORDONNEES DES POINTS (BARRAGE EN 0.)
! |   T            | -->| TEMPS (DEPART A 0.)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!   SOUS-PROGRAMMES APPELES : DEGRE3, FONCTION FC1
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY: GRAV
      IMPLICIT NONE

      INTEGER NPOIN
!
      DOUBLE PRECISION T,YG,YD,CG,CD,FC1
      DOUBLE PRECISION H(NPOIN),X(NPOIN),U(NPOIN),V(NPOIN)
!      ,H(21,81),X(21,81),U(21,81)
      DOUBLE PRECISION EPS,BG,BD,WR,URG,YRG,XC,XD,XE,XCOUR,CRG
!
      INTEGER ITMAX,NOEUD
!
      EXTERNAL FC1
      COMMON /ICOEC1/ CG,CD
!
!-----------------------------------------------------------------------
!
!     CALCUL DES VALEURS INITIALES DE LA SITUATION THEORIQUE
!     ------------------------------------------------------
!
!      IM=21
!      JM=81
!
      CG = SQRT(GRAV*YG)
      CD = SQRT(GRAV*YD)
!
!     VITESSE URG ET TIRANT D EAU YRG A GAUCHE DU RESSAUT:
!
      EPS   = 1.D-8
      BG    = CG
      BD    = CD
      ITMAX = 100
!
      CALL ZBRENT(FC1,EPS,BG,BD,ITMAX)
!
      CRG = BD
      WR  = (CRG/CD)*SQRT( (CRG**2+CD**2)/2.D0 )
      URG = WR-(CD/CRG)*SQRT( (CRG**2+CD**2)/2.D0  )
      YRG = CRG**2/GRAV
!
!     ---------------------------
!     |VIII) VALEURS THEORIQUES |
!     ---------------------------
!
!
!     DANS LE PLAN (X,T)
!     ON APPELLE C L'INTERSECTION A T AVEC X = -CG*T
!                D L'INTERSECTION A T AVEC X = (URG - CRG)*T
!                E L'INTERSECTION A T AVEC X =  WR*T
!                                    WR:VITESSE DU RESSAUT
!          A ET B LES LIMITES DU DOMAINE D'ETUDE
!          A GAUCHE DE C , EAU IMMOBILE DE COTE YG
!          ENTRE C ET D , ONDE DE DETENTE
!          ENTRE D ET E, EAU A VITESSE URG DE COTE (CRG**2)/GRAV
!          A DROITE DE E , EAU IMMOBILE DE COTE YD
!
!     ABSCISSES DES POINTS C , D ET E
      XC = 8.D0-CG * T
      XD = 8.D0+(URG - CRG) * T
      XE = 8.D0+WR * T
!
!      DO NOEUD=1,JM
      DO NOEUD=1,NPOIN
!
        XCOUR = X(NOEUD)
!
        IF (XCOUR.LE.XC) THEN
!
!         EN XCOUR LE NIVEAU EST DE YG
          H(NOEUD) = YG
          U(NOEUD) = 0.D0
          V(NOEUD) = 0.D0
          CYCLE
        ENDIF
!
        IF (XCOUR.LE.XD) THEN
!
!         EN XCOUR POINT DE L'ONDE DE DETENTE
          H(NOEUD) = ((2.D0/3.D0*CG-(XCOUR-8.D0)/(3.D0*T))**2)/GRAV
          U(NOEUD) = 2.D0*(CG - SQRT(GRAV*H(NOEUD)))
          V(NOEUD) = 0.D0
          CYCLE
        ENDIF
!
        IF (XCOUR.LE.XE) THEN
!
!         EN XCOUR TIRANT D'EAU EGAL TIRANT A GAUCHE DU RESSAUT
          H(NOEUD)= YRG
          U(NOEUD)= URG
          V(NOEUD)= 0.D0
          CYCLE
        ENDIF
!
!       EN XCOUR TIRANT D'EAU EGAL TIRANT A DROITE DU RESSAUT
        H(NOEUD)         = YD
        U(NOEUD)         = 0.D0
        V(NOEUD)         = 0.D0
!
      ENDDO
!
!      DO J = 1,JM
!        DO I = 2,IM
!          H(I,J) = H(1,J)
!          U(I,J) = U(1,J)
!        ENDDO
!      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
