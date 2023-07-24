!                       *****************
                        SUBROUTINE INTERP
!                       *****************
!
     &(XINIT , YINIT , IKINIT , NPINIT , NEINIT ,
     & X , Y , NPOIN , NPMAX , SHP , ELT)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2        24/04/91    J-C GALLAND  (LNH)
!                               09/11/94    P LANG / TRIGRID (LHF)
!***********************************************************************
!
! FONCTION : INTERPOLATION DES FONDS SUR LE MAILLAGE
!
!----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |    X,Y         | -->|  COORDONNEES DES POINTS DU MAILLAGE
! |    ZF          |<-- |  COTES DU FOND
! |    XRELV,YRELV | -->|  COORDONNEES DES POINTS DE BATHY
! |    ZRELV       | -->|  COTES DES POINTS DE BATHY
! |    NBAT        | -->|  NOMBRE DE POINTS DE BATHY
! |    NBOR        | -->|  NUMEROTATION DES ELEMENTS DE BORD
! |    NPTFR       | -->|  NOMBRE DE POINTS FRONTIERE
! |    NFOND       | -->|  CANAUX DES FICHIERS DES FONDS
! |    NBFOND      | -->|  NOMBRE DE FICHIERS FONDS DONNES PAR
! |                |    |  L'UTILISATEUR (5 MAXI)
! |    FOND        | -->|  NOM DES FICHIERS DES FONDS
! |    DM          | -->|  DISTANCE MINIMALE A LA FRONTIERE
! |                |    |  POUR L'INTERPOLATION DES FONDS
! |    FONTRI      | -->|  INDICATEUR DE LECTURE DES FONDS DANS TRIGRID
! |    CORTRI      | -->|  CORRECTION DES FONDS POUR TRIGRID
! |                |    |
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
! |                |    |
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
!
! APPELE PAR : STBTEL
! APPEL DE : LECFON, FASP
!
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPINIT, NEINIT, NPOIN,NPMAX
      DOUBLE PRECISION, INTENT(IN) :: XINIT(NPINIT) , YINIT(NPINIT)
      INTEGER, INTENT(IN) :: IKINIT(NEINIT,3)
      INTEGER, INTENT(INOUT) :: ELT(NPMAX)
      DOUBLE PRECISION, INTENT(IN) :: X(NPMAX) , Y(NPMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(NPMAX,3)
!
      INTEGER IELEM , JELEM , IPOIN
      DOUBLE PRECISION XP,YP,A1,A2,A3,C1,C2,X1,X2,X3,Y1,Y2,Y3
!
!=======================================================================
!
      WRITE(LU,4)
!
4     FORMAT(//,1X,'DATA INTERPOLATION',/,
     &          1X,'------------------',/)
!
      DO IPOIN = 1,NPOIN
!
        XP = X(IPOIN)
        YP = Y(IPOIN)
        C1 = -999999.D0
!
        DO IELEM = 1,NEINIT
          X1 = XINIT(IKINIT(IELEM,1))
          X2 = XINIT(IKINIT(IELEM,2))
          X3 = XINIT(IKINIT(IELEM,3))
          Y1 = YINIT(IKINIT(IELEM,1))
          Y2 = YINIT(IKINIT(IELEM,2))
          Y3 = YINIT(IKINIT(IELEM,3))
          A1 = (X3-X2)*(YP-Y2) - (Y3-Y2)*(XP-X2)
          A2 = (X1-X3)*(YP-Y3) - (Y1-Y3)*(XP-X3)
          A3 = (X2-X1)*(YP-Y1) - (Y2-Y1)*(XP-X1)
          IF (A1.GE.0.AND.A2.GE.0.AND.A3.GE.0) GOTO 30
          C2 = MIN(A1,A2,A3) / ((X3-X2)*(Y1-Y2)-(Y3-Y2)*(X1-X2))
          IF (C2.GT.C1) THEN
            C1 = C2
            JELEM = IELEM
          ENDIF
        ENDDO
!
        WRITE(LU,*) 'EXTRAPOLATION REQUIRED FOR ',
     &              'THE NODE :',IPOIN
        IELEM = JELEM
        X1 = XINIT(IKINIT(IELEM,1))
        X2 = XINIT(IKINIT(IELEM,2))
        X3 = XINIT(IKINIT(IELEM,3))
        Y1 = YINIT(IKINIT(IELEM,1))
        Y2 = YINIT(IKINIT(IELEM,2))
        Y3 = YINIT(IKINIT(IELEM,3))
        A1 = (X3-X2)*(YP-Y2) - (Y3-Y2)*(XP-X2)
        A2 = (X1-X3)*(YP-Y3) - (Y1-Y3)*(XP-X3)
        A3 = (X2-X1)*(YP-Y1) - (Y2-Y1)*(XP-X1)
!
30      CONTINUE
        C1 = (X3-X2)*(Y1-Y2)-(Y3-Y2)*(X1-X2)
        SHP(IPOIN,1) = A1/C1
        SHP(IPOIN,2) = A2/C1
        SHP(IPOIN,3) = A3/C1
        ELT(IPOIN) = IELEM
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
