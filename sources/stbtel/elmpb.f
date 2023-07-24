!                       ****************
                        SUBROUTINE ELMPB
!                       ****************
!
     &(NBPB,NUMPB,X,Y,IKLE,NCOLOR,ISDRY,NEW)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2                    A. CABAL / P. LANG SOGREAH
!***********************************************************************
!
!     FONCTION  :  ELIMINATION DES ELEMENTS APPARTENANT A PLUSIEURS
!                  SEGMENTS FRONTIERES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   NBPB         |--> | NB DE POINTS A SUPPRIMER
! |   NUMPB        |--> | NUMERO DES POINTS A SUPPRIMER
! |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
! |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
! |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS DU MAILLAGE
! | ELPSEC         | -->| INDICATEUR ELIMIN. DES ELEMENTS PARTIELLEMENT SECS
! | ISDRY(NELMAX)  |<-- | TAB INDICATEUR ELEMENTS SECS
! |                |    | = 1 POINT TOUJOURS SEC,
! |                |    | = 0 SOUS SEUSEC M D'EAU AU MOINS POUR 1 PAS DE TEMPS
! | IHAUT          | -->| NUM D'ORDRE DE LA VARIABLE HAUT D'EAU DANS FICH TEL2D
! | NVAR           | -->| NB DE VAR STOCKEES DANS LE FICHIER TEL2D
! | H              | -->| TABLEAU DES HAUTEURS D'EAU
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       |<-->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       |<-->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL
      IMPLICIT NONE
!
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,4), ISDRY(NPMAX), NEW(NPMAX)
      INTEGER, INTENT(INOUT) :: NCOLOR(NPMAX)
      INTEGER,INTENT(IN) :: NBPB, NUMPB(100)
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPMAX) , Y(NPMAX)
!
      INTEGER I, IEL, J, NELI

!
!
!     -------------------------------------------------------------
!     ELIMINATION DES ELEMENTS COMPORTANT DES POINTS A PROBLEME
!     -------------------------------------------------------------
!
      DO I=1,NBPB
        DO IEL = 1, NELEM
          IF (IKLE(IEL,1).EQ.NUMPB(I).OR.IKLE(IEL,2).EQ.NUMPB(I)
     &        .OR.IKLE(IEL,3).EQ.NUMPB(I)) THEN
            IKLE(IEL, 1) = 0
            IKLE(IEL, 2) = 0
            IKLE(IEL, 3) = 0
          ENDIF
        ENDDO
      ENDDO
!
!     ELIMINATION DES ELEMENTS
!     ------------------------
!
      NELI = 0
      IEL = 1
!     POUR CHAQUE ELEMENT FAIRE
 20   CONTINUE
        IF ((IKLE(IEL, 1).EQ.0).AND.(IKLE(IEL, 2).EQ.0).AND.
     &     (IKLE(IEL, 3).EQ.0)) THEN
          NELI = NELI + 1
          DO I = IEL, NELEM - NELI
            IKLE(I,1) = IKLE(I+1, 1)
            IKLE(I,2) = IKLE(I+1, 2)
            IKLE(I,3) = IKLE(I+1, 3)
          ENDDO
        ELSE
          IEL = IEL + 1
        ENDIF
      IF (IEL .LE. NELEM-NELI) GOTO 20
!     FIN POUR CHAQUE ELEMENT
!
      WRITE(LU,2009) NELI
!
      NELEM = NELEM - NELI
!
!      ELIMINATION DES POINTS NE FAISANT PLUS PARTIE DU MAILLAGE
!      REUTILISATION DE ISDRY POUR MARQUER LES POINTS NON UTILISEES
!      ---------------------------------------------
      DO I = 1, NPOIN
        ISDRY(I) = 0
        NEW(I) = 0
      ENDDO
!
      DO IEL = 1, NELEM
      ENDDO
!
      NELI = 0
      I = 1
!     POUR CHAQUE POINT FAIRE
      DO I = 1, NPOIN
        IF (ISDRY(I) .EQ.0) THEN
          NELI = NELI + 1
          NEW(I) = 0
        ELSE
          NEW(I) = I - NELI
        ENDIF
      ENDDO
!     FIN POUR CHAQUE POINT
!
      NELI = 0
      I = 1
!     POUR CHAQUE POINT FAIRE
 30   CONTINUE
        IF (ISDRY(I) .EQ.0) THEN
!         POINT I  A ELIMINER
          NELI = NELI + 1
!         DECALAGE DANS LE TABLEAU DES POINTS
          DO J = I, NPOIN - NELI
            X(J) = X(J+1)
            Y(J) = Y(J+1)
            NCOLOR(J) = NCOLOR(J+1)
            IF (ISDRY(J+1).GT.0) THEN
              ISDRY(J) = ISDRY(J+1) - 1
            ELSE
              ISDRY(J) = 0
            ENDIF
          ENDDO
        ELSE
          I = I + 1
        ENDIF
      IF (I .LE. NPOIN - NELI) GOTO 30
!     FIN POUR CHAQUE POINT
      WRITE(LU,2011) NELI
      NPOIN = NPOIN - NELI
!
!     ON REPERCUTE LA RENUMEROTATION DANS IKLE
!     ----------------------------------------
      DO IEL = 1, NELEM
        J = IKLE(IEL,1)
        IKLE(IEL,1) = NEW(J)
        J = IKLE(IEL,2)
        IKLE(IEL,2) = NEW(J)
        J = IKLE(IEL,3)
        IKLE(IEL,3) = NEW(J)
      ENDDO
      RETURN
!***********************************************************************
 2009 FORMAT(1X,'ELEMENTS CANCELLED IN THE MESH:',I8)
 2011 FORMAT(1X,'POINTS CANCELLED IN THE MESH:  ',I8)
      END
