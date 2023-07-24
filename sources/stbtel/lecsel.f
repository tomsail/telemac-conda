!                       *****************
                        SUBROUTINE LECSEL
!                       *****************
!
     &(XINIT,YINIT,IKINIT,NPINIT,NEINIT,X,Y,IKLE,IKLES,W,TITRE,TEXTE,
     & NVARIN,NVAR2,STD,FUSION,NGEO,NFO1,IPOBO,IPARAM,DATE,
     & TIME,X_ORIG,Y_ORIG)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2           11/02/93    J.M. JANIN
!***********************************************************************
!
!   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
!               MAILLAGE DANS LE FICHIER D'ENTREE SELAFIN
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | NPOIN1         |<-- | NOMBRE REEL DE POINTS DU MAILLAGE
! |                |    | (NPOIN REPRESENTE L'INDICE MAX DES NOEUDS CAR
! |                |    | SUPERTAB LAISSE DES TROUS DANS LA NUMEROTATION
! | TYPELE         |<-- | TYPE D'ELEMENTS
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        |<-- | TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       |<-- | NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       |<-- | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
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
!-----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,NDP,NPOIN,NELMAX,FFORMAT,
     &                               TYP_ELEM,TYP_BND_ELEM
      USE INTERFACE_HERMES
      USE INTERFACE_STBTEL, EX_LECSEL => LECSEL
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(INOUT) :: XINIT(*), YINIT(*), X(*), Y(*)
      REAL, INTENT(INOUT) :: W(*)
      INTEGER, INTENT(IN) :: NGEO , NFO1
      INTEGER, INTENT(INOUT) :: IPARAM(10),DATE(3),TIME(3)
      INTEGER, INTENT(INOUT) :: NEINIT , NPINIT
      INTEGER, INTENT(INOUT) :: NVARIN , NVAR2
      INTEGER, INTENT(INOUT) :: IKINIT(NELEM,NDP)
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,NDP),IKLES(NDP,NELEM)
      INTEGER, INTENT(INOUT) :: IPOBO(*)
      LOGICAL, INTENT(IN) :: FUSION
      INTEGER, INTENT(INOUT) :: X_ORIG, Y_ORIG
      CHARACTER(LEN=72), INTENT(INOUT) :: TITRE
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(26)
      CHARACTER(LEN=3), INTENT(IN) ::  STD
!
      INTEGER NPOIN1 , NELEM1 , NPOIN2 , NELEM2
      INTEGER I , IELEM
      CHARACTER(LEN=80) FULL_TITLE

      CHARACTER(LEN=16), ALLOCATABLE :: VARUNIT(:), VARNAME(:)
      INTEGER :: IERR
      INTEGER DATETIME(6)
      INTEGER NPTFR
!
!
!=======================================================================
! LECTURE SEQUENTIELLE DU PREMIER FICHIER
!=======================================================================
!
      CALL GET_MESH_TITLE(FFORMAT, NGEO, FULL_TITLE, IERR)
      CALL CHECK_CALL(IERR, 'LECSEL:GET_MESH_TITLE')
      TITRE = FULL_TITLE(1:72)

      CALL GET_DATA_NVAR(FFORMAT, NGEO, NVARIN, IERR)
      CALL CHECK_CALL(IERR, 'LECSEL:GET_DATA_NVAR')

      IF(NVARIN.GT.26) THEN
        WRITE(LU,*) 'NVAR > 26 NOT HANDLED'
        CALL PLANTE(1)
      ENDIF

      ALLOCATE(VARUNIT(NVARIN))
      ALLOCATE(VARNAME(NVARIN))
      CALL GET_DATA_VAR_LIST(FFORMAT,NGEO,NVARIN,VARNAME,VARUNIT,IERR)
      CALL CHECK_CALL(IERR, 'LECSEL:GET_DATA_VAR_LIST')
      DO I=1,NVARIN
        TEXTE(I)(1:16) = VARNAME(I)
        TEXTE(I)(17:32) = VARUNIT(I)
      ENDDO
      DEALLOCATE(VARUNIT)
      DEALLOCATE(VARNAME)

      CALL GET_MESH_DATE(FFORMAT,NGEO,DATETIME,IERR)
      CALL CHECK_CALL(IERR, 'LECSEL:GET_MESH_DATA')
      DATE = DATETIME(1:3)
      TIME = DATETIME(4:6)

      CALL GET_MESH_NELEM(FFORMAT,NGEO,TYP_ELEM,NELEM1,IERR)
      CALL CHECK_CALL(IERR, 'GET_MESH_NELEM:TRIA')

      CALL GET_MESH_NPOIN(FFORMAT,NGEO,TYP_ELEM,NPOIN1,IERR)
      CALL CHECK_CALL(IERR, 'GET_MESH_NPOIN:TRIA')

      CALL GET_MESH_CONNECTIVITY(FFORMAT,NGEO,TYP_ELEM,IKLES,
     &                           NELEM1,NDP,IERR)
      CALL CHECK_CALL(IERR, 'GET_MESH_CONNECTIVITY:TRIA')

      CALL GET_BND_NPOIN(FFORMAT,NGEO,TYP_BND_ELEM,NPTFR,IERR)
      WRITE(LU,*) 'NPTFR FROM LECSEL ', NPTFR

      CALL GET_BND_IPOBO(FFORMAT,NGEO,NPOIN,NPTFR,
     &                   TYP_BND_ELEM,IPOBO,IERR)

      CALL GET_MESH_COORD(FFORMAT,NGEO,1,2,NPOIN1,X,IERR)
      CALL GET_MESH_COORD(FFORMAT,NGEO,2,2,NPOIN1,Y,IERR)
      CALL GET_MESH_ORIG(FFORMAT,NGEO,X_ORIG,Y_ORIG,IERR)
!
!=======================================================================
! LECTURE SEQUENTIELLE DU SECOND FICHIER EN CAS DE FUSION
!=======================================================================
!
      IF (FUSION) THEN
!
        CALL GET_MESH_NELEM(FFORMAT,NFO1,TYP_ELEM,NELEM2,IERR)
        CALL CHECK_CALL(IERR, 'GET_MESH_NELEM:TRIA')

        CALL GET_MESH_NPOIN(FFORMAT,NFO1,TYP_ELEM,NPOIN2,IERR)
        CALL CHECK_CALL(IERR, 'GET_MESH_NPOIN:TRIA')
!
        CALL GET_MESH_CONNECTIVITY(FFORMAT,NGEO,TYP_ELEM,
     &                             IKLES(1,NELEM1+1),
     &                             NELEM2,NDP,IERR)
        CALL CHECK_CALL(IERR, 'GET_MESH_CONNECTIVITY:TRIA')

        CALL GET_MESH_COORD(FFORMAT,NGEO,1,2,NPOIN2,X(NPOIN1+1),IERR)
        CALL GET_MESH_COORD(FFORMAT,NGEO,2,2,NPOIN2,Y(NPOIN1+1),IERR)
!
      ENDIF
!
!=======================================================================
! AFFECTATION DES VALEURS LUES AUX VARIABLES CONCERNEES
!=======================================================================
!
      NEINIT = NELEM
      NPINIT = NPOIN
!
!       INVERSION DE IKLES EN IKLE.
!
      DO I = 1,NDP
        DO IELEM = 1,NELEM1
          IKLE  (IELEM,I) = IKLES(I,IELEM)
          IKINIT(IELEM,I) = IKLES(I,IELEM)
        ENDDO
        IF (FUSION) THEN
          DO IELEM = NELEM1+1,NELEM
            IKLE  (IELEM,I) = IKLES(I,IELEM) + NPOIN1
            IKINIT(IELEM,I) = IKLES(I,IELEM) + NPOIN1
          ENDDO
        ENDIF
      ENDDO
!
      DO I = 1,NPOIN
        XINIT(I) = X(I)
        YINIT(I) = Y(I)
      ENDDO
!
!=======================================================================
!
      RETURN
      END
