!                       *****************
                        SUBROUTINE FM3SEL
!                       *****************
!
     &(X,Y,NPOIN,NBOR,NFIC,STD,NVAR,TEXTE,TEXTLU,VARCLA,NVARCL,
     & TITRE,SORLEO,NSOR,W,IKLE,
     & IKLES,ITRAV,NELEM,NPTFR,NDP,MXPTVS,MXELVS,DATE,TIME,
     & DEBU,SUIT,ECRI,LISTIN,IPARAM,IPOBO,X_ORIG,Y_ORIG)
!
!***********************************************************************
! PROGICIEL STBTEL V5.2       02/01/96    J-M HERVOUET (LNH) 30 71 80 18
!
!***********************************************************************
!
!     COMME FMTSEL, MAIS LA DIMENSION DE SORLEO  EST
!     PARAMETREE.
!
!     FONCTIONS :  LECTURE DU FICHIER GEOMETRIQUE AU STANDARD SELAFIN
!                  ECRITURE DU FICHIER GEOMETRIQUE AU STANDARD SELAFIN
!
!     LES FONCTIONS DE CE SOUS-PROGRAMME PEUVENT ETRE PILOTEES AVEC
!     LES ARGUMENTS DEBU, SUIT, ET ECRI
!
!     ATTENTION : 1) SI DEBU, SUIT ET ECRIT SONT A .FALSE.
!                    FM3SEL LIT LA GEOMETRIE.
!
!                 2) SI DEBU ITRAV DOIT ETRE LE TABLEAU IA DES ENTIERS
!                    ET ON NE DOIT PAS SE SERVIR DE IKLE ET IKLES
!                    CAR LE SOUS-PROGRAMME DE POINTEURS N'A PAS ENCORE
!                    ETE APPELE.
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   X,Y          |<-->| COORDONNEES DU MAILLAGE.
! |   NPOIN        |<-->| NOMBRE DE POINTS DU MAILLAGE.
! |   NBOR         | -->| NUMEROTAION GLOBALE DES POINTS DE BORD.
! |   NFIC         | -->| NUMERO DE CANAL DU FICHIER A LIRE OU ECRIRE.
! |   STAND        | -->| NON UTILISE
! |   STD          | -->| BINAIRE DU FICHIER (STD, IBM, I3E)
! |   NVAR         |<-->| NOMBRE DE VARIABLES DANS LE FICHIER
! |   TEXTE        |<-->| NOMS ET UNITES DES VARIABLES.
! |   TEXTLU       |<-->| NOMS ET UNITES DES VARIABLES QU'ON VA LIRE.
! |   VARCLA       | -->| TABLEAU CONTENANT LES VARIABLES CLANDESTI-NES.
! |   NVARCL       | -->| NOMBRE DE VARIABLES CLANDESTI-NES.
! |   TITRE        |<-->| TITRE DU FICHIER.
! |   SORLEO       | -->| VARIABLES QUE L'ON SOUHAITE ECRIRE DANS LE
! |                |    | FICHIER (TABLEAU DE 26 LOGIQUES)
! |   NSOR         | -->| DIMENSION DE SOLRLEO
! |   W            | -->| TABLEAU DE TRAVAIL CONSIDERE ICI COMME REEL
! |                |    | DE TAILLE NPOIN.
! |   IKLE         |<-->| TABLE DE CONNECTIVITE (I.E. PASSAGE DE LA
! |                |    | NUMEROTATION LOCALE DES POINTS D'UN ELEMENT
! |                |    | A LA NUMEROTATION GLOBALE
! |   IKLES        | -->| TABLEAU DE TRAVAIL SERVANT A MODIFIER IKLE
! |                |    | DIMENSION NELEM * NDP
! |   ITRAV        | -->| TABLEAU DE TRAVAIL ENTIER DE DIMENSION NPOIN
! |   NELEM        |<-->| NOMBRE D'ELEMENTS DU MAILLAGE.
! |   NPTFR        |<-->| NOMBRE DE POINTS FRONTIERE DU DOMAINE.
! |   NDP          |<-->| NOMBRE DE SOMMETS PAR ELEMENT.
! |   DEBU         | -->| ON LIT UNIQUEMENT LE DEBUT DU FICHIER POUR
! |                |    | CONNAITRE LES NOMBRES DE POINTS AVEC LESQUELS
! |                |    | ON POURRA CONSTRUIRE LES POINTEURS.
! |   SUIT         | -->| ON LIT TOUTE LA PARTIE GEOMETRIE DU FICHIER
! |                |    | POUR SE PLACER SUR LES ENREGISTREMENTS DES
! |                |    | RESULTATS.
! |   ECRI         | -->| ON ECRIT LA PARTIE GEOMETRIE DU FICHIER
! |   LISTIN       | -->| ECRITURE D'INFORMATIONS SUR LISTING (OU NON)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMMES APPELES : LIT , ECRIT
!
!***********************************************************************
!
!    LISTE DES ENREGISTREMENTS DU FICHIER GEOMETRIQUE:
!
!      1    : TITRE DE L'ETUDE
!      2    : NOMBRE DE FONCTIONS LUES SUR LA GRILLE 1 ET LA GRILLE 2.
!      3    : NOM ET UNITE DES VARIABLES
!      4    : 1,0,0,0,0,0,0,0,0,0
!      5    : NELEM,NPOIN,NDP,1
!      6    : IKLE
!      7    : IPOBO TABLEAU DE DIMENSION NPOIN, 0 POUR LES POINTS
!             INTERIEURS, UN NUMERO SINON.
!      8    : X
!      9    : Y
!
!    CE QUI SUIT N'EST PAS FAIT DANS FM3SEL.
!
!     10    : TEMPS
!     11    : VARIABLES DECLAREES EN 3 (DANS L'ORDRE DES DECLARATIONS)
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY : TYP_BND_ELEM, TYP_ELEM,
     &                                NPTIR, OUT_FORMAT
      USE INTERFACE_STBTEL, EX_FM3SEL => FM3SEL
      USE INTERFACE_HERMES
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*)
      REAL, INTENT(INOUT) :: W(*)
!                     IKLE(NELEM,NDP) IKLES(NDP,NELEM)
      INTEGER, INTENT(IN) :: NBOR(*)
      INTEGER, INTENT(INOUT) :: IKLE(*),IKLES(*),ITRAV(*)
      INTEGER, INTENT(INOUT) :: NPOIN,NVAR,MXPTVS,MXELVS,TIME(3),DATE(3)
      INTEGER, INTENT(IN) :: NFIC,NVARCL,NSOR
      INTEGER, INTENT(INOUT) :: NELEM,NPTFR,NDP
      INTEGER, INTENT(IN) :: IPARAM(10),IPOBO(*)
      LOGICAL, INTENT(IN) :: DEBU,SUIT,ECRI,LISTIN,SORLEO(*)
      CHARACTER(LEN=3), INTENT(IN) :: STD
      CHARACTER(LEN=72), INTENT(IN) :: TITRE
!                        NSOR      NSOR+NVARCL
      CHARACTER(LEN=32), INTENT(IN) :: TEXTE(*),VARCLA(NVARCL)
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTLU(*)
      INTEGER, INTENT(IN) :: X_ORIG, Y_ORIG
!
      INTEGER IELEM,I,IB(10)

      CHARACTER(LEN=32), ALLOCATABLE :: VAR_INFO(:)
      INTEGER DATETIME(6), IVAR, IERR
      CHARACTER(LEN=80) :: TITSEL
!
!-----------------------------------------------------------------------
!
      IF(ECRI) THEN
        ! COMPUTE NVAR
        NVAR=0
        DO I=1,NSOR
          IF(SORLEO(I)) NVAR = NVAR + 1
        ENDDO
        NVAR = NVAR + NVARCL

        ALLOCATE(VAR_INFO(NVAR))
        IVAR = 1
        DO I=1,NSOR
          IF(SORLEO(I)) THEN
            VAR_INFO(IVAR) = TEXTE(I)(1:32)
            IVAR = IVAR + 1
          ENDIF
        ENDDO
        IF(NVARCL.NE.0) THEN
          DO I=1,NVARCL
            VAR_INFO(IVAR) = VARCLA(I)(1:32)
            IVAR = IVAR + 1
          ENDDO
        ENDIF


        TITSEL = REPEAT(' ', 80)
        TITSEL(1:72) = TITRE

        CALL SET_HEADER(OUT_FORMAT,NFIC,TITSEL,NVAR,VAR_INFO,IERR)

        IF(NPTIR.EQ.0) THEN
          DO I=1,NPOIN
            ITRAV(I) = 0
          ENDDO
          DO I =1,NPTFR
            ITRAV(NBOR(I)) = I
          ENDDO
        ELSE
          ITRAV(1:NPTFR) = 0
        ENDIF

        CALL SET_MESH(OUT_FORMAT,NFIC,2,TYP_ELEM,NDP,NPTFR,NPTIR,
     &                NELEM,NPOIN,IKLE,ITRAV(1:NPOIN),ITRAV(1:NPOIN),
     &                X,Y,0,DATE,TIME,X_ORIG,Y_ORIG,IERR)

      ELSE IF (DEBU) THEN
        CALL GET_MESH_CONNECTIVITY(OUT_FORMAT,NFIC,TYP_ELEM,
     &          ITRAV(1+NPOIN:NELEM*NDP+NPOIN+1),NELEM,NDP,IERR)

        CALL GET_BND_IPOBO(OUT_FORMAT,NFIC,NPOIN,NPTFR,
     &                     TYP_BND_ELEM,ITRAV(1:NPOIN),IERR)
        NPTFR = 0
        IF(NPOIN.GE.1) THEN
          DO I = 1 , NPOIN
            IF(ITRAV(I).NE.0) NPTFR = NPTFR + 1
          ENDDO
        ENDIF
!       ITRAV(1) : IPOBO  ITRAV(1+NPOIN) : IKLES
!       ITRAV(1+NPOIN+NDP*NELEM) : TABLEAU DE TRAVAIL.
        CALL MXPTEL(MXPTVS,MXELVS,ITRAV(1+NPOIN),
     &              ITRAV(1+NPOIN+NDP*NELEM),
     &              NPOIN,NELEM,NDP,ITRAV,LISTIN)
!       IPOBO EST MODIFIE PAR MXPTEL

      ELSE
        ! LIT ACTION
        CALL GET_MESH_DATE(OUT_FORMAT,NFIC,DATETIME,IERR)
        CALL CHECK_CALL(IERR, 'FM3SEL:GET_MESH_DATA')
        DATE = DATETIME(1:3)
        TIME = DATETIME(4:6)

        CALL GET_MESH_NELEM(OUT_FORMAT,NFIC,TYP_ELEM,NELEM,IERR)
        CALL CHECK_CALL(IERR, 'GET_MESH_NELEM:TRIA')

        CALL GET_MESH_NPOIN(OUT_FORMAT,NFIC,TYP_ELEM,NPOIN,IERR)
        CALL CHECK_CALL(IERR, 'GET_MESH_NPOIN:TRIA')

        CALL GET_MESH_CONNECTIVITY(OUT_FORMAT,NFIC,TYP_ELEM,IKLES,
     &                             NELEM,NDP,IERR)
        CALL CHECK_CALL(IERR, 'GET_MESH_CONNECTIVITY:TRIA')

        CALL GET_MESH_NPOIN_PER_ELEMENT(OUT_FORMAT,NFIC,TYP_ELEM,
     &                                  IB(3),IERR)

        DO I      = 1,NDP
          DO IELEM  = 1,NELEM
            IKLE((I-1)*NELEM+IELEM) = IKLES((IELEM-1)*NDP+I)
          ENDDO
        ENDDO
      ENDIF
!
      IF(DEBU.AND.LISTIN) THEN
        WRITE(LU,301) TITRE
        WRITE(LU,501) NPTFR,NELEM,NPOIN
        IF(NPOIN.LT.3.OR.NPTFR.LT.3.OR.NPTFR.GE.NPOIN) THEN
          WRITE(LU,24) NPOIN,NPTFR
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!  FORMATS D'IMPRESSION :
!
24    FORMAT(1X,'FM3SEL : NUMBER OF POINTS IN THE MESH: ',1I9,/,1X,
     &          '         NUMBER OF BOUNDARY POINTS: ',1I9,/,1X,
     &          '         WRONG DATA, PROGRAMME STOPPED')
301   FORMAT(1X,//,1X,'TITLE: ',A72,/)
501   FORMAT(1X,'NUMBER OF BOUNDARY POINTS: ',1I9,/,1X,
     &'NUMBER OF ELEMENTS:',1I9,/,1X,'NUMBER OF POINTS:',1I9)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
