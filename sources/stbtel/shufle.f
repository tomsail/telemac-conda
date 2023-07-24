!                       *****************
                        SUBROUTINE SHUFLE
!                       *****************
!
     &(IKLE,X)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2       19/02/93  J-M JANIN   (LNH) 30 87 72 84
!***********************************************************************
!
! FONCTION : CHANGEMENT DE LA NUMEROTATION DES ELEMENTS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |  IKLE          |<-->|NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT  |
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
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : ECHELE
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL
      USE INTERFACE_STBTEL, EX_SHUFLE => SHUFLE
      IMPLICIT NONE
!
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,4)
      DOUBLE PRECISION, INTENT(IN) :: X(*)
!
      INTEGER IELEM , I1 , I2 , I3 , I4 , I
!
      DOUBLE PRECISION XA
!
!
!=======================================================================
!
      DO I = 1 , (NELEM-4)/2 , 2
        CALL ECHELE (IKLE,I,NELEM-I+1)
      ENDDO
!
!=======================================================================
!
      IF(NDP.EQ.4) THEN
!
        DO IELEM = 1 , NELEM
!
          I1 = IKLE(IELEM,1)
          I2 = IKLE(IELEM,2)
          I3 = IKLE(IELEM,3)
          I4 = IKLE(IELEM,4)
          XA = X(I1)
          IF(XA.LT.X(I2)) THEN
            XA = X(I2)
            IKLE(IELEM,1) = I2
            IKLE(IELEM,2) = I3
            IKLE(IELEM,3) = I4
            IKLE(IELEM,4) = I1
          ENDIF
          IF(XA.LT.X(I3)) THEN
            XA = X(I3)
            IKLE(IELEM,1) = I3
            IKLE(IELEM,2) = I4
            IKLE(IELEM,3) = I1
            IKLE(IELEM,4) = I2
          ENDIF
          IF(XA.LT.X(I4)) THEN
            IKLE(IELEM,1) = I4
            IKLE(IELEM,2) = I1
            IKLE(IELEM,3) = I2
            IKLE(IELEM,4) = I3
          ENDIF
!
        ENDDO
!
      ELSEIF(NDP.EQ.3) THEN
!
        DO IELEM = 1 , NELEM
!
          I1 = IKLE(IELEM,1)
          I2 = IKLE(IELEM,2)
          I3 = IKLE(IELEM,3)
          XA = X(I1)
          IF(XA.LT.X(I2)) THEN
            XA = X(I2)
            IKLE(IELEM,1) = I2
            IKLE(IELEM,2) = I3
            IKLE(IELEM,3) = I1
          ENDIF
          IF(XA.LT.X(I3)) THEN
            IKLE(IELEM,1) = I3
            IKLE(IELEM,2) = I1
            IKLE(IELEM,3) = I2
          ENDIF
!
        ENDDO
!
      ELSE
!
        WRITE(LU,*) 'UNKNOWN MESH IN SHUFLE'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
      RETURN
      END
