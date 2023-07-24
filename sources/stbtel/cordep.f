!                       *****************
                        SUBROUTINE CORDEP
!                       *****************
!
     &(IKLE,LGVEC)
!
!***********************************************************************
! PROGICIEL: STBTEL V5.2          28/08/89  J-M HERVOUET (LNH) 3071 8018
!***********************************************************************
!
! FONCTION : CORRECTION DES DEPENDANCES ARRIERES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |  IKLE          |<-->| NUMERO GLOBAUX DES NOEUDS DE CHAQUE ELEMENT  |
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       | -->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       | -->| NOMBRE TOTAL D'ELEMTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : ECHELE
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,NELMAX
      USE INTERFACE_STBTEL, EX_CORDEP => CORDEP
      IMPLICIT NONE
!
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,4)
      INTEGER, INTENT(IN) :: LGVEC
!
      INTEGER IELEM , IEL1 , IEL2
      INTEGER I1 , I2 , I3 , J1 , J2 , J3
      INTEGER K
!
      LOGICAL DEP
!
!
!=======================================================================
!
      DO IELEM = 1,NELEM
        IEL2 = IELEM
25      CONTINUE
        DEP = .FALSE.
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        DO K = 2,LGVEC
          IEL1 = MOD(NELEM+IELEM-K,NELEM) + 1
          J1 = IKLE(IEL1,1)
          J2 = IKLE(IEL1,2)
          J3 = IKLE(IEL1,3)
          IF (I1.EQ.J1.OR.I1.EQ.J2.OR.I1.EQ.J3.OR.
     &        I2.EQ.J1.OR.I2.EQ.J2.OR.I2.EQ.J3.OR.
     &        I3.EQ.J1.OR.I3.EQ.J2.OR.I3.EQ.J3) DEP=.TRUE.
        ENDDO
        IF (DEP) THEN
          IEL2 = MOD(IEL2,NELEM) + 1
          IF (IEL2.EQ.IELEM) GOTO 40
          CALL ECHELE(IKLE,IELEM,IEL2)
          GOTO 25
        ENDIF
      ENDDO ! IELEM
!
!=======================================================================
!
40    CONTINUE
!
      RETURN
      END
