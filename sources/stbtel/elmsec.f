!                       *****************
                        SUBROUTINE ELMSEC
!                       *****************
!
     &( ELPSEC, SEUSEC, TPSFIN,  X, Y, IKLE, NCOLOR, ISDRY,
     &  IHAUT, NVAR, H, WORK, NEW, STD, NGEO, TEXTE )
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2                     A. CABAL / P. LANG SOGREAH
!***********************************************************************
!
!     FONCTION  :  ELIMINATION DES ELEMENTS SECS DU MAILLAGE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
! |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
! |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS DU MAILLAGE
! | ELPSEC         | -->| INDICATEUR ELIMIN. DES ELEMENTS PARTIELLEMENT SECS
! | SEUSEC         | -->| VALEUR POUR LA DEFINITION SECHERESSE
! | ISDRY(NELMAX)  |<-- | TAB INDICATEUR ELEMENTS SECS
! |                |    | = 1 POINT TOUJOURS SEC,
! |                |    | = 0 SOUS SEUSEC M D'EAU AU MOINS POUR 1 PAS DE TEMPS
! | IHAUT          | -->| NUM D'ORDRE DE LA VARIABLE HAUT D'EAU DANS FICH TEL2D
! | NVAR           | -->| NB DE VAR STOCKEES DANS LE FICHIER TEL2D
! | H              | -->| TABLEAU DES HAUTEURS D'EAU
! | WORK           | -->| TABLEAU (REAL) DE TRAVAIL
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
! |  FICH:         |    |
! |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN
! |    NGEO       |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
! |    NLIM      |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
! |    NFO1      |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,NPOIN,NELMAX,NPMAX,FFORMAT
      USE INTERFACE_STBTEL, EX_ELMSEC => ELMSEC
      USE INTERFACE_HERMES
      IMPLICIT NONE
!
      LOGICAL, INTENT(IN) :: ELPSEC
      DOUBLE PRECISION, INTENT(IN) :: SEUSEC
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPMAX),Y(NPMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPMAX),TPSFIN(1)
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,4), ISDRY(NPMAX), NEW(NPMAX)
      INTEGER, INTENT(INOUT) :: NCOLOR(NPMAX)
      INTEGER, INTENT(IN) :: IHAUT, NVAR
      REAL, INTENT(INOUT) :: WORK(*)
      INTEGER, INTENT(IN) :: NGEO
      CHARACTER(LEN=3), INTENT(IN) :: STD
      CHARACTER(LEN=32), INTENT(IN) :: TEXTE(NVAR)
!
!
!     VARIABLES LOCALES
!
      INTEGER I, IEL, NPDT, NPSEC, NSEC
      INTEGER J, NELI, IERR
      INTEGER NP1, NP2, NP3, ISECH
!------------------------------------------------------------
      IF (NVAR.EQ.0) THEN
        WRITE(LU,2012)
        RETURN
      ENDIF
      IF (IHAUT.EQ.0) THEN
        WRITE(LU,2013)
        RETURN
      ENDIF
!     INITIALISATION DU TABLEAU ISDRY : PAS DEFAUT TOUS SECS
      DO I = 1, NPOIN
        ISDRY(I) = 1
      ENDDO
!     LECTURE DES RESULTATS TELEMAC ET REMPLISSAGE DU TABLEAU ISDRY
!     -------------------------------------------------------------

!     -----------------------
!     ON RESSORT SI LE FICHIER NE CONTENAIT AUCUN PAS DE TEMPS
      CALL GET_DATA_NTIMESTEP(FFORMAT, NGEO, NPDT, IERR)
      CALL CHECK_CALL(IERR, 'ELMSEC:GET_DATA_NTIMESTEP')

      IF (NPDT.EQ.0) THEN
        WRITE(LU,2001)
        CALL PLANTE(1)
        STOP
      ENDIF

      CALL GET_DATA_TIME(FFORMAT, NGEO, NPDT-1, TPSFIN(1), IERR)
      CALL CHECK_CALL(IERR, 'ELMSEC:GET_DATA_TIME')

      CALL GET_DATA_VALUE(FFORMAT, NGEO, NPDT-1, TEXTE(IHAUT),
     &                    H, NPOIN,IERR)
      CALL CHECK_CALL(IERR, 'ELMSEC:GET_DATA_VALUE')

      NPSEC = 0
      DO I = 1, NPOIN
        IF (H(I).GT.SEUSEC) THEN
          ISDRY(I) = 0
        ELSE
          NPSEC = NPSEC + 1
        ENDIF
      ENDDO
      WRITE(LU,2000) TPSFIN(1), NPSEC, SEUSEC
!
!     TEST DES ELEMENTS SECS OU PARTIELLEMENTS SECS
!     ---------------------------------------------
      NPSEC = 0
      NSEC = 0
!
!     PARCOURS DES ELEMENTS
      DO IEL = 1, NELEM
        NP1 = IKLE(IEL, 1)
        NP2 = IKLE(IEL, 2)
        NP3 = IKLE(IEL, 3)
        ISECH = ISDRY(NP1) * ISDRY(NP2) * ISDRY(NP3)
!       SI ISECH (PRODUIT) = 1 ELEMENT IEL TOUJOURS SEC
        IF (ISECH.EQ.1) THEN
!         POSITIONNE A 0 TOUS LES NUMEROS DES POINTS DE L'ELEMENT
          NSEC = NSEC + 1
          IKLE(IEL, 1) = 0
          IKLE(IEL, 2) = 0
          IKLE(IEL, 3) = 0
        ELSE
          IF (ELPSEC) THEN
!         TEST SI ELEMENT PARTIELLEMENT SEC
            ISECH =  ISDRY(NP1) + ISDRY(NP2) + ISDRY(NP3)
            IF (ISECH.GE.1) THEN
!             ELEMENT PARTIELLEMENT SEC A ELIMINER
!             POSITIONNE A 0 TOUS LES NUMEROS DES POINTS DE L'ELEMENT
              IKLE(IEL, 1) = 0
              IKLE(IEL, 2) = 0
              IKLE(IEL, 3) = 0
              NPSEC = NPSEC + 1
            ENDIF
!           FIN SI ELIMINATION PART. SECS
          ENDIF
        ENDIF
      ENDDO !IEL
!     FIN PARCOURS DE TOUS LES ELEMENTS
      IF (NSEC.EQ.0) THEN
        WRITE(LU,2002)
      ELSE IF (NSEC.EQ.1) THEN
        WRITE(LU,2003)
      ELSE
        WRITE(LU,2004) NSEC
      ENDIF
!
      IF (ELPSEC) THEN
        IF (NPSEC.EQ.0) THEN
          WRITE(LU,2005)
        ELSE IF (NPSEC.EQ.1) THEN
          WRITE(LU,2006)
        ELSE
          WRITE(LU,2007) NPSEC
        ENDIF
      ENDIF
!
!     S'IL N'Y A PAS D'ELEMENTS SECS OU P.SECS ON S'EN VA
      IF ((NSEC.EQ.0) .AND. (NPSEC.EQ.0)) RETURN
!
!     ELIMINATION DES ELEMENTS SECS ET PARTIELLLEMENT SECS
!     ---------------------------------------------
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
      IF (NELI .LE. 0) THEN
        WRITE(LU,2008)
      ELSE
        WRITE(LU,2009) NELI
      ENDIF
!
      NELEM = NELEM - NELI
!
!     ELIMINATION DES POINTS NE FAISANT PLUS PARTIE DU MAILLAGE
!     REUTILISATION DE ISDRY POUR MARQUER LES POINTS NON UTILISEES
!     ---------------------------------------------
      DO I = 1, NPOIN
        ISDRY(I) = 0
        NEW(I) = 0
      ENDDO
!
      DO IEL = 1, NELEM
        ISDRY(IKLE(IEL,1)) = IKLE(IEL,1)
        ISDRY(IKLE(IEL,2)) = IKLE(IEL,2)
        ISDRY(IKLE(IEL,3)) = IKLE(IEL,3)
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
      IF (ISDRY(I).EQ.0) THEN
!       POINT I  A ELIMINER
!       WRITE(LU,*) 'POINT A ELIMINER',I,':',X(I),Y(I),NCOLOR(I)
        NELI = NELI + 1
!       DECALAGE DANS LE TABLEAU DES POINTS
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
      IF (NELI .LE. 0) THEN
        WRITE(LU,2010)
      ELSE
        WRITE(LU,2011) NELI
      ENDIF
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
 2000 FORMAT(1X,'TIME ',G15.3,' : ',I8,
     &' POINT(S) WITH WATER DEPTH BELOW',G15.3)
!
 2001 FORMAT(/,1X,'SORRY, THE UNIVERSAL FILE DOES NOT CONTAIN',
     & /,1X,'ANY COMPUTATION RESULTS.',
     & /,1X,'FINDING OUT DRY ELEMENTS IS IMPOSSIBLE !')
!
 2002 FORMAT(1X,'NO COMPLETELY DRY ELEMENT IN THE MESH.')
!
 2003 FORMAT(1X,'ONLY ONE COMPLETELY DRY ELEMENT FOUND',
     & /,1X,'IN THE MESH.')
!
 2004 FORMAT(1X,'COMPLETELY DRY ELEMENTS IN THE MESH: ',I8)
!
 2005 FORMAT(1X,'NO PARTIALLY DRY ELEMENT IN THE MESH.')
!
 2006 FORMAT(1X,'ONLY ONE PARTIALLY DRY ELEMENT IN THE MESH.')
!
 2007 FORMAT(1X,'PARTIALLY DRY ELEMENTS IN THE MESH:',I8)
!
 2008 FORMAT(1X,'NO ELEMENT HAS BEEN CANCELLED IN THE MESH.')
!
 2009 FORMAT(1X,'ELEMENTS CANCELLED IN THE MESH:',I8)
!
 2010 FORMAT(1X,'NO POINT HAS BEEN CANCELLED IN THE MESH.')
!
 2011 FORMAT(1X,'POINTS CANCELLED IN THE MESH:  ',I8)
!
 2012 FORMAT(/,1X,'NO VARIABLE STORED ON THE FILE. ',
     & /,1X,'DRY ELEMENT SUPPRESSION IS IMPOSSIBLE.')
!
 2013 FORMAT(/,1X,'THE WATER DEPTH VARIABLE IS NOT STORED ON THE FILE',
     & /,1X,'DRY ELEMENT SUPPRESSION IS IMPOSSIBLE.')
      END
