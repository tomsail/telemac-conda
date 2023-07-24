!                       *****************
                        SUBROUTINE INISEL
!                       *****************
!
     &(NPOIN1,TYPELE,STD,NSFOND,FUSION,IHAUT,NGEO,NFO1)
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
! | IHAUT          |<-- | NUMERO DE LA VARIABLE HAUTEUR D'EAU
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
! |    NGEO        |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
! |    NLIM        |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
! |    NFO1        |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,MESH,NDP,NPOIN,
     &                TYP_ELEM,TYP_BND_ELEM,NPTIR,FFORMAT
      USE INTERFACE_STBTEL, EX_INISEL => INISEL
      USE INTERFACE_HERMES
      IMPLICIT NONE
!
      INTEGER, INTENT(INOUT) :: NPOIN1,NSFOND
      CHARACTER(LEN=11), INTENT(INOUT) :: TYPELE
      CHARACTER(LEN=3), INTENT(IN) :: STD
      LOGICAL, INTENT(IN) :: FUSION
      INTEGER, INTENT(INOUT) :: IHAUT
      INTEGER, INTENT(IN) :: NGEO , NFO1
!
      INTEGER NVAR , I , IB(10)
!
!
! COMMON
!
      CHARACTER(LEN=16), ALLOCATABLE :: VARUNIT(:), VARNAME(:)
      INTEGER :: IERR
!
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER ET RECHERCHE ENREGISTREMENT 5
!=======================================================================
!
      CALL GET_DATA_NVAR(FFORMAT, NGEO, NVAR, IERR)
      CALL CHECK_CALL(IERR, 'INISEL:GET_DATA_NVAR')

      ALLOCATE(VARUNIT(NVAR))
      ALLOCATE(VARNAME(NVAR))
      CALL GET_DATA_VAR_LIST(FFORMAT,NGEO,NVAR,VARNAME,VARUNIT,IERR)
      CALL CHECK_CALL(IERR, 'INISEL:GET_DATA_VAR_LIST')

      DO I=1,NVAR
        IF( VARNAME(I) .EQ. 'FOND            ' .OR.
     &      VARNAME(I) .EQ. 'BOTTOM          ' ) NSFOND = I
        IF( VARNAME(I) .EQ. 'HAUTEUR D''EAU   ' .OR.
     &      VARNAME(I) .EQ. 'WATER DEPTH     ' ) IHAUT = I
      ENDDO

      DEALLOCATE(VARUNIT)
      DEALLOCATE(VARNAME)
!
!=======================================================================
! AFFECTATION DES VALEURS LUES AUX VARIABLES CONCERNEES
!=======================================================================
!
      ! TEST FOR TRIANGLE
      TYP_ELEM = TRIANGLE_ELT_TYPE
      CALL GET_MESH_NELEM(FFORMAT,NGEO,TYP_ELEM,NELEM,IERR)
      CALL CHECK_CALL(IERR,'inisel:GET_MESH_NELEM:TRIANGLE')
      NDP = 3
      IF(NELEM.EQ.0) THEN
        ! TEST FOR QUADRANGLE
        TYP_ELEM = QUADRANGLE_ELT_TYPE
        CALL GET_MESH_NELEM(FFORMAT,NGEO,TYP_ELEM,NELEM,IERR)
        CALL CHECK_CALL(IERR, 'INISEL:GET_MESH_NELEM:QUADRANGLE')
        NDP = 4
        IF(NELEM.EQ.0) THEN
          WRITE(LU,*) 'NO 2D ELEMENTS IN A 2D MESH'
          CALL PLANTE(1)
        ENDIF
      ENDIF
      TYP_BND_ELEM = POINT_BND_ELT_TYPE

      CALL GET_MESH_NPOIN(FFORMAT,NGEO,TYP_ELEM,NPOIN,IERR)
      CALL CHECK_CALL(IERR, 'GET_MESH_NPOIN:TRIA')

      CALL GET_MESH_NPTIR(FFORMAT,NGEO,NPTIR,IERR)

      NPOIN1= NPOIN
!
!=======================================================================
! LECTURE SEQUENTIELLE DU SECOND FICHIER EN CAS DE FUSION
!=======================================================================
!
      IF (FUSION) THEN
!
        CALL GET_MESH_NELEM(FFORMAT,NFO1,TYP_ELEM,IB(1),IERR)
        CALL CHECK_CALL(IERR,'INISEL:GET_MESH_NELEM:TRIANGLE')

        CALL GET_MESH_NPOIN(FFORMAT,NFO1,TYP_ELEM,IB(2),IERR)
        CALL CHECK_CALL(IERR, 'GET_MESH_NPOIN:TRIA')

        CALL GET_MESH_NPOIN_PER_ELEMENT(FFORMAT,NFO1,TYP_ELEM,
     &                                  IB(3),IERR)
!
        NELEM = NELEM + IB(1)
        NPOIN = NPOIN + IB(2)
!
        IF (NDP.NE.IB(3)) THEN
          WRITE(LU,3130)
 3130     FORMAT(' INISEL : TYPES OF MESH INHOMOGENEOUS')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!=======================================================================
! MISE DES VALEURS DE MESH AU STANDARD TELEMAC
!=======================================================================
!
      IF (NDP.EQ.4) THEN
        MESH = 2
        TYPELE = 'QUADRANGLES'
      ELSEIF (NDP.EQ.3) THEN
        MESH = 3
        TYPELE = 'TRIANGLES  '
      ELSE
        WRITE(LU,3140) MESH
 3140   FORMAT(' INISEL : TYPE OF MESH NOT AVAILABLE IN TELEMAC,
     &           MESH = ',I4)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      RETURN
      END
