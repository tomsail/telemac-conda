!                     ***********************
                      SUBROUTINE POINT_STBTEL
!                     ***********************
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2        09/08/89    J-C GALLAND   (LNH)
!                                 19/02/93    J-M JANIN     (LNH)
!                            21/08/96    P   CHAILLET  (LHF) - FASTTABS
!                               09/98       A. CABAL / SOGREAH
!   ORIGINE : ULYSSE
!***********************************************************************
!
!     FONCTION  : CONSTRUCTION DES POINTEURS DES TABLEAUX A ET IA
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   IDIMA        | -->| DIMENSION DU TABLEAU A
! |   IDIMIA       | -->| DIMENSION DU TABLEAU IA
! |   NBAT         | -->| NOMBRE DE POINTS DE BATHY
! |   NBFOND       | -->| NOMBRE DE FICHIERS BATHY
! |   MAILLE       | -->| NOM DU MAILLEUR
! |                | -->| POUR LA LECTURE DU FICHIER SIMAIL
! | arguments rajoutes pour l'option d'elimination des elements secs
! |   ELISEC      | -->| BOOLEAN INDIQUANT SI ELIMINATION DES POINTS SECS
! |                |    | EST DEMANDEE
! | fin arguments rajoutes pour l'option d'elimination des elements secs
! |________________|____|______________________________________________
! | COMMON         |    |
! |    K...        |<-- | POINTEURS DU TABLEAU ENTIER
! |  GEO:          |    |
! |    MESH        |--> | TYPE DE MAILLAGE
! |    NDP         |--> | NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       |--> | NOMBRE TOTAL DE POINTS DU MAILLAGE
! |    NELEM       |--> | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       |<-- | DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      |<-- | DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL
      IMPLICIT NONE
!
!=======================================================================
!  POUR PREVOIR L'ELIMINATION DES TRIANGLES SURCONTRAINTS , LES VALEURS
!  DE NPOIN ET NELEM2 SONT SURDIMENSIONNEES
!=======================================================================
!
      NPMAX  = NPOIN +   INT(0.1*NELEM)
      NELMAX = NELEM + 2*INT(0.1*NELEM)
      IF(DIV4) NPMAX  = NPMAX  + 3*NELEM
      IF(DIV4) NELMAX = NELMAX + 3*NELEM
!
      RETURN
      END
