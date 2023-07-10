!                       *****************
                        SUBROUTINE INIADC
!                       *****************
!
     &(NPOIN1,TYPELE,NSFOND,IHAUT,NGEO,TITRE)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2      13/08/01    J.M. HERVOUET 01 30 87 80 18
!***********************************************************************
!
!   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
!               MAILLAGE DANS LE FICHIER D'ENTREE ADCIRC
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
      USE DECLARATIONS_STBTEL, ONLY: NELEM,MESH,NDP,NPOIN
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NGEO
      INTEGER, INTENT(INOUT) :: NPOIN1 , NSFOND
      INTEGER, INTENT(INOUT) :: IHAUT
!
      CHARACTER(LEN=80), INTENT(INOUT) :: TITRE
      CHARACTER(LEN=11), INTENT(INOUT) :: TYPELE
!
! COMMON
!
!
!=======================================================================
! INITIALISATIONS
!=======================================================================
!
      NSFOND = 2
      REWIND NGEO
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER ET RECHERCHE ENREGISTREMENT 5
!=======================================================================
!
      TITRE=' '
      READ(NGEO,*) TITRE(1:24)
      READ(NGEO,*) NELEM,NPOIN
      IHAUT = 0
      NDP   = 3
      NPOIN1= NPOIN
      MESH = 3
      TYPELE = 'TRIANGLES  '
!
!-----------------------------------------------------------------------
!
      RETURN
      END
