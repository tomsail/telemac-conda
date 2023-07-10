!                       ************************
                        SUBROUTINE LECDON_STBTEL
!                       ************************
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2     24/10/90    J-M HERVOUET (LNH) 30 71 80 18
!                             09/11/94    P LANG / LHF
!                                08/96    P CHAILLET/ LHF
!                                01/99    A CABAL/ P LANG SOGREAH
!***********************************************************************
!
! FONCTION : LECTURE DU FICHIER CAS PAR APPEL DU LOGICIEL DAMOCLES.
!
!----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |    NCLE       | -->| NUMERO D'UNITE LOGIQUE DES MOTS-CLES DE REF.
! |    NCAS        | -->| NUMERO D'UNITE LOGIQUE DU FICHIER CAS.
! |    STD         |<-- | STANDARD  DE BINAIRE
! |    DECTRI      |<-- | DECOUPAGE DES TRIANGLES SURCONTRAINTS
! |    FOND        |<-- | TABLEAU DES NOMS DES FICHIERS DE BATHYMETRIE
! |    EPSI        |<-- | DISTANCE MINIMALE ENTRE 2 NOEUDS DU MAILLAGE
! |                |    | L'INTERPOLATION DES FONDS
! |    COLOR       |<-- | ECRITURE DE LA COULEUR DES NOEUDS
! |    NBAT        |<-- | NOMBRE DE POINTS DE BATHYMETRIE
! |    ELIDEP      |<-- | ELIMINATION DES DEPENDANCES ARRIERES
! |    NBFOND      |<-- | NOMBRE DE FICHIERS BATHY
! |    MAILLE      |<-- | MAILLEUR UTILISE :
! |                |    |   SUPERTAB VERSION 6 : SUPERTAB6 (DEFAUT)
! |                |    |   SUPERTAB VERSION 4 : SUPERTAB4
! |                |    |   SIMAIL
! |    DM          |<-- | DISTANCE MNIMALE A LA FRONTIERE POUR
! |                |    | L'INTERPOLATION DES FONDS
! |    FONTRI      |<-- | INDICATEUR DE LECTURE DES FONDS DANS TRIGRID
! |    CORTRI      |<-- | CORRECTION DES FONDS DE TRIGRID
! |    OPTASS      |    |
! |    ADDFAS      |<-- | CONDITION LIMITE DANS FICHIER ADDITIONNEL
! |    ELISEC      |<-- | INDIC ELIMINATION DES ELEMENTS SECS
! |    ELPSEC      |<-- | INDIC ELIM ELEMENTS PARTIELLEMENT SECS
! |    SEUSEC      |<-- | VALEUR POUR LA DEFINITION SECHERESSE
! |    STOTOT      |<-- | INDIC RECUP TOTALITE DES PAS DE TEMPS
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
!
! APPELE PAR : STBTEL
! APPEL DE : DAMOCL
!
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_STBTEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
! AJOUTE POUR EDAMOX:
!
      INTEGER          TROUVE(4,MAXKEYWORD)
      INTEGER          ADRES(4,MAXKEYWORD) , DIMENS(4,MAXKEYWORD)
      INTEGER          MOTINT(MAXKEYWORD)
      INTEGER          NLNG
      CHARACTER(LEN=PATH_LEN)    MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=72)     MOTCLE(4,MAXKEYWORD,2)
      DOUBLE PRECISION MOTREA(MAXKEYWORD)
      LOGICAL          DOC
      LOGICAL          MOTLOG(MAXKEYWORD)
!
! FIN DES VARIABLES AJOUTEES POUR EDAMOX:
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      DOC = .FALSE.
      NLNG=2
!
      CALL DAMOCLES( ADRES , DIMENS, MAXKEYWORD   , DOC    , LNG , LU ,
     &               MOTINT, MOTREA, MOTLOG , MOTCAR ,
     &               MOTCLE, TROUVE, NCLE  , NCAS   , .FALSE. )
!
!    AFFECTATION DES PARAMETRES SOUS LEUR NOM EN FORTRAN
!
!-----------------------------------------------------------------------
! MOTS CLE DE TYPE ENTIER
!-----------------------------------------------------------------------
!
      NBAT      = MOTINT  (ADRES(1,1))
      LGVEC     = MOTINT  (ADRES(1,2))
      NSOM      = MIN(MOTINT  (ADRES(1,3)),9)
      NSOM2     = MOTINT  (ADRES(1,4))
      ALLOCATE(SOM2(NSOM2, 2))
      MAX_SEG_PER_POINT     = MOTINT(ADRES(1,6))
!
!-----------------------------------------------------------------------
! MOTS CLE DE TYPE REEL
!-----------------------------------------------------------------------
!
      EPSI      = MOTREA  (ADRES(2,1))
      DM        = MOTREA  (ADRES(2,2))
      CORTRI    = MOTREA  (ADRES(2,3))
!
      IF (NSOM.GE.3) THEN
        DO I=1,NSOM
          SOM(I,1) = MOTREA  (ADRES(2,4)+I-1)
          SOM(I,2) = MOTREA  (ADRES(2,5)+I-1)
        ENDDO
        SOM(NSOM+1,1) = SOM(1,1)
        SOM(NSOM+1,2) = SOM(1,2)
      ENDIF
!
      IF (NSOM2.GE.3) THEN
        DO I=1,NSOM2
          SOM2(I,1) = MOTREA  (ADRES(2,6)+I-1)
          SOM2(I,2) = MOTREA  (ADRES(2,7)+I-1)
        ENDDO
      ENDIF
!
      SEUSEC =  MOTREA  (ADRES(2,8))
      DX =  MOTREA  (ADRES(2,9))
      DY =  MOTREA  (ADRES(2,10))
!
!-----------------------------------------------------------------------
! MOTS CLE DE TYPE LOGIQUE
!-----------------------------------------------------------------------
!
      DECTRI    = MOTLOG  (ADRES(3,1))
      COLOR     = MOTLOG  (ADRES(3,2))
      ELIDEP    = MOTLOG  (ADRES(3,3))
      DIV4      = MOTLOG  (ADRES(3,4))
      FONTRI    = MOTLOG  (ADRES(3,5))
      OPTASS    = MOTLOG  (ADRES(3,6))
!
      ADDFAS    = MOTLOG  (ADRES(3,7))
      PROJEX    = MOTLOG  (ADRES(3,8))
!
      IF (NSOM2.GE.3) DIV4 = .TRUE.
!
      ELISEC = MOTLOG  (ADRES(3,9))
      ELPSEC = MOTLOG  (ADRES(3,10))
      STOTOT = MOTLOG  (ADRES(3,11))
      DEBUG  = MOTLOG  (ADRES(3,12))
      CONVER = MOTLOG  (ADRES(3,13))
      SRF_BND = MOTLOG  (ADRES(3,14))
      TRANSLATE = MOTLOG  (ADRES(3,15))
      AUTO_PRECISION = MOTLOG  (ADRES(3,16))
!
!-----------------------------------------------------------------------
! MOTS CLE DE TYPE CARACTERE
!-----------------------------------------------------------------------
!
      NBFOND=0

      IF (MOTCAR(ADRES(4,8)).NE.' ') THEN
        NBFOND = NBFOND + 1
        FOND(NBFOND) = MOTCAR(ADRES(4,8))
        NOMFON = MOTCAR(ADRES(4,8))
      ENDIF
      IF (MOTCAR(ADRES(4,9)).NE.' ') THEN
        NBFOND = NBFOND + 1
        FOND(NBFOND) = MOTCAR(ADRES(4,9))
        NOMFO2 = MOTCAR(ADRES(4,9))
      ENDIF
      IF (MOTCAR(ADRES(4,10)).NE.' ') THEN
        NBFOND = NBFOND + 1
        FOND(NBFOND) = MOTCAR(ADRES(4,10))
        NOMIMP = MOTCAR(ADRES(4,10))
      ENDIF
      IF (MOTCAR(ADRES(4,17)).NE.' ') THEN
        NBFOND = NBFOND + 1
        FOND(NBFOND) = MOTCAR(ADRES(4,17))
        NOMFRC = MOTCAR(ADRES(4,17))
      ENDIF
      IF (MOTCAR(ADRES(4,18)).NE.' ') THEN
        NBFOND = NBFOND + 1
        FOND(NBFOND) = MOTCAR(ADRES(4,18))
        NOMSOU = MOTCAR(ADRES(4,18))
      ENDIF
!
      NOMGEO = MOTCAR( ADRES(4, 5) )
      NOMFOR = MOTCAR( ADRES(4, 3) )
      NOMCAS = MOTCAR( ADRES(4, 4) )
      NOMLIM = MOTCAR( ADRES(4, 7) )
      NOMRES = MOTCAR( ADRES(4, 6) )
      FFORMAT = 'SERAFIN '
      OUT_FORMAT = MOTCAR( ADRES(4, 31) )(1:8)
      NOMFO1 = MOTCAR( ADRES(4,15) )
      INFILE = MOTCAR( ADRES(4,24) )
      OUTFILE = MOTCAR( ADRES(4,25) )
      BOUNDFILE = MOTCAR( ADRES(4,26) )
      LOGFILE = MOTCAR( ADRES(4,27) )
      OUTBNDFILE = MOTCAR( ADRES(4,28) )
      OUTLOGFILE = MOTCAR( ADRES(4,29) )
      NOMBND2 = MOTCAR( ADRES(4,30) )
!
      STD       = MOTCAR ( ADRES(4,11))(1:3)
      MAILLE    = MOTCAR ( ADRES(4,14))(1:9)
      INFMT     = MOTCAR ( ADRES(4,22))(1:9)
      OUTFMT    = MOTCAR ( ADRES(4,23))(1:9)
!
      FUSION = .FALSE.
      IF (MOTCAR(ADRES(4,15)).NE.' '.AND.MAILLE.EQ.'SELAFIN')
     &   FUSION = .TRUE.
!
!-----------------------------------------------------------------------
! VERIFICATION DES VALEURS LUES
!-----------------------------------------------------------------------
!
      IF (FONTRI) NBFOND = 1
      IF (NBFOND.GT.5) THEN
        WRITE(LU,4000)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF (STD.NE.'IBM'.AND.STD.NE.'I3E'.AND.STD.NE.'STD') THEN
        WRITE(LU,4100) STD
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF (MAILLE.NE.'SUPERTAB4'.AND.MAILLE.NE.'SUPERTAB6'.AND.
     &    MAILLE.NE.'SIMAIL'   .AND.MAILLE.NE.'SELAFIN'  .AND.
     &    MAILLE.NE.'TRIGRID'  .AND.MAILLE.NE.'MASTER2'  .AND.
     &    MAILLE.NE.'FASTTABS' .AND.MAILLE.NE.'ADCIRC'   ) THEN
        WRITE(LU,4200) MAILLE
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF (MAILLE.EQ.'SUPERTAB4') THEN
! INDICATEUR DE DEBUT DE LA LISTE DES POINTS DU MAILLAGE
        NSEC11 = 15
        NSEC12 = 0
! INDICATEUR DE DEBUT DE LA LISTE DES IKLE
        NSEC2  = 71
! INDICATEUR DE POSITION DU TITRE
        NSEC3  = 151
      ELSEIF (MAILLE.EQ.'SUPERTAB6') THEN
! INDICATEUR DE DEBUT DE LA LISTE DES POINTS DU MAILLAGE
! LECTURE EN SIMPLE PRECISION
        NSEC11 = 15
! LECTURE EN DOUBLE PRECISION
        NSEC12 = 781
! INDICATEUR DE DEBUT DE LA LISTE DES IKLE
        NSEC2  = 780
! INDICATEUR DE POSITION DU TITRE
        NSEC3  = 151
      ELSEIF (MAILLE.EQ.'MASTER2') THEN
! INDICATEUR DE DEBUT DE LA LISTE DES POINTS DU MAILLAGE
        NSEC11 = 0
        NSEC12 = 2411
! INDICATEUR DE DEBUT DE LA LISTE DES IKLE
        NSEC2  = 2412
! INDICATEUR DE POSITION DU TITRE
        NSEC3  = 151
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF (ELISEC) THEN
        IF (MAILLE.NE.'SELAFIN') THEN
          WRITE(LU,4300)
          CALL PLANTE(1)
          STOP
        ENDIF
        IF (NBFOND.GT.0) THEN
          WRITE(LU,4301)
          CALL PLANTE(1)
          STOP
        ENDIF
        IF (DIV4) THEN
          WRITE(LU,4302)
          CALL PLANTE(1)
          STOP
        ENDIF
        DIV4      = .FALSE.
        FONTRI    = .FALSE.
        OPTASS    = .FALSE.
        ADDFAS    = .FALSE.
        PROJEX    = .FALSE.
      ENDIF
!
!-----------------------------------------------------------------------
!
4000  FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||||',/,
     &          1X,'LECDON . THE NUMBER OF BOTTOM TOPOGRAPHY FILES',/,
     &          1X,'         IS LIMITED TO 5 |',/,
     &          1X,'         (KEYWORD : BOTTOM TOPOGRAPHY FILE)',/,
     &          1X,'||||||||||||||||||||||||||||||||||||||||||||||',//)
!
4100  FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/,
     &          1X,'LECDON . UNKNOWN BINARY FILE STANDARD : ',A3,/,
     &          1X,'         (KEYWORD : BINARY FILE STANDARD)',/,
     &          1X,'||||||||||||||||||||||||||||||||||||||||||||',//)
!
4200  FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||||',/,
     &          1X,'LECDON . UNKNOWN TYPE OF MESH GENERATOR : ',A9,/,
     &          1X,'         (KEYWORD : MESH GENERATOR)',/,
     &          1X,'||||||||||||||||||||||||||||||||||||||||||||||',//)
 4300 FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/,
     &          1X,'LECDON . THE DRY ELEMENTS ELIMINATION IS ONLY',/,
     &          1X,'AVAILABLE WHEN USING SELAFIN FILE.',/,
     &          1X,'||||||||||||||||||||||||||||||||||||||||||||',//)
 4301 FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/,
     &          1X,'LECDON . BATHYMETRY INTERPOLATION IMPOSSIBLE',/,
     &          1X,'WHEN USING DRY ELEMENTS ELIMINATION.',/,
     &          1X,'||||||||||||||||||||||||||||||||||||||||||||',//)
 4302 FORMAT(//,1X,'||||||||||||||||||||||||||||||||||||||||||||',/,
     &          1X,'LECDON . TRIANGLE CUTTING IMPOSSIBLE',/,
     &          1X,'WHEN USING DRY ELEMENTS ELIMINATION.',/,
     &          1X,'||||||||||||||||||||||||||||||||||||||||||||',//)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
