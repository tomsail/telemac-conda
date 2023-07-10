!                       *****************
                        SUBROUTINE LECADC
!                       *****************
!
     &( X , Y , ZF , IKLE , NGEO )
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2         13/08/01    J-M HERVOUET  (LNH)
!
!***********************************************************************
!
!     FONCTION  :  LECTURE DU FICHIER DE LA GEOMETRIE CREE PAR ADCIRC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   X,Y          |<-- | COORDONNEES DU MAILLAGE .
! |   X1,Y1        |<-- | COORDONNEES DU MAILLAGE LUES EN SIMPLE
! |                |    | PRECISION DANS LE FICHIER SIMAIL
! |   IKLE         |<-- | LISTE DES POINTS DE CHAQUE ELEMENT
! |   TITRE        |<-- | TITRE DU MAILLAGE
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       | -->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |  FICH:         |    |
! |    NRES        | -->| NUMERO DU CANAL DU FICHIER DE SERAFIN
! |    NGEO        | -->| NUMERO DU CANAL DU FICHIER MAILLEUR
! |    NLIM        | -->| NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
! |    NFO1        |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : -
!***********************************************************************
!
!    LISTE DES ENREGISTREMENTS DU FICHIER GEOMETRIQUE:
!             (DOCUMENTION: NOTICE SIMAIL)
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,NPOIN,NELMAX
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NGEO
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,4)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*) , Y(*),ZF(*)
!
      INTEGER I,J,IBID
!
!
!=======================================================================
!   INITIALISATION
!=======================================================================
!
!     REWIND NGEO
!
!
!=======================================================================
! LECTURE SEQUENTIELLE DES COORDONNEES
!=======================================================================
!
      DO I=1,NPOIN
        READ(NGEO,*) J,X(I),Y(I),ZF(I)
        IF(I.NE.J) THEN
          WRITE(LU,*) 'ERROR IN THE LIST OF COORDINATES LINE ',I
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!=======================================================================
! LECTURE SEQUENTIELLE DU TABLEAU IKLE
!=======================================================================
!
      DO I=1,NELEM
        READ(NGEO,*) J,IBID,IKLE(I,1),IKLE(I,2),IKLE(I,3)
      ENDDO
!
!=======================================================================
!
      RETURN
      END
