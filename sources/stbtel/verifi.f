!                       *****************
                        SUBROUTINE VERIFI
!                       *****************
!
     &(X,Y,IKLE,NCOLOR,TRAV1,EPSI,MESH,NDP,NPOIN,NELEM,NELMAX)
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2         09/08/89    J-C GALLAND  (LNH)
!***********************************************************************
!
!     FONCTION  :  ELIMINATION DES TROUS DANS LA NUMEROTATION DES NOEUDS
!                  ET RE-ORIENTATION DES ELEMENTS DU MAILLAGE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
! |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
! |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS DU MAILLAGE
! |   TRAV1,2      |<-->| TABLEAUX DE TRAVAIL
! |   EPSI         | -->| DISTANCE MINIMALE ENTRE 2 NOEUDS DU MAILLAGE
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
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
! APPEL DE : REMAIL, CIRCUL
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_STBTEL, EX_VERIFI => VERIFI
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) ::  MESH , NDP , NELMAX
      INTEGER, INTENT(INOUT) :: NPOIN, NELEM
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,4) , NCOLOR(*)
      INTEGER, INTENT(INOUT) :: TRAV1(*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: X(*) , Y(*)
      DOUBLE PRECISION, INTENT(INOUT) :: EPSI
!
      INTEGER ITEST , ITEST1 , IELEM
! COMMON
!
!
!=======================================================================
! ON VERIFIE QUE TOUS LES POINTS SONT DISTINCTS
!=======================================================================
!
      CALL REMAIL (IKLE,NCOLOR,TRAV1,X,Y,EPSI,
     &             NDP,NPOIN,NELEM,NELMAX)
!
!=======================================================================
! ON VERIFIE QUE TOUS LES ELEMENTS SONT CORRECTEMENT ORIENTES
!=======================================================================
!
      ITEST = 0
!
! CAS DES QUADRANGLES
!
      IF (MESH.EQ.2) THEN
!
        DO IELEM=1,NELEM
!
          ITEST1 = 0
          CALL CIRCUL (IKLE,ITEST1,IELEM,1,2,3,X,Y,NELMAX)
          CALL CIRCUL (IKLE,ITEST1,IELEM,2,3,4,X,Y,NELMAX)
          CALL CIRCUL (IKLE,ITEST1,IELEM,3,4,1,X,Y,NELMAX)
          CALL CIRCUL (IKLE,ITEST1,IELEM,4,1,2,X,Y,NELMAX)
          IF (ITEST1.GT.0) ITEST = ITEST + 1
!
        ENDDO
!
! CAS DES TRIANGLES
!
      ELSE IF (MESH.EQ.3) THEN
!
        DO IELEM=1,NELEM
!
          ITEST1 = 0
          CALL CIRCUL (IKLE,ITEST1,IELEM,1,2,3,X,Y,NELMAX)
          IF (ITEST1.GT.0) ITEST = ITEST + 1
!
        ENDDO
!
      ELSE
        WRITE(LU,3090) MESH
 3090   FORMAT(/,' LECSTB TYPE OF MESH NOT AVAILABLE , MESH = ',I4)
      ENDIF
!
      WRITE(LU,3100) ITEST
 3100 FORMAT(1X,'NUMBER OF ELEMENTS BADLY ORIENTED : ',I5)
!
      RETURN
      END
