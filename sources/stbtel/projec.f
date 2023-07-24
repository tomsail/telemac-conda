!                       *****************
                        SUBROUTINE PROJEC
!                       *****************
!
     &(X , Y , ZF , XRELV , YRELV , ZRELV , NBAT ,
     & NBOR , NPTFR , NFOND , NBFOND , FOND , DM ,
     & FONTRI , CORTRI , MAILLE,NGEO,KP1BOR)
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2         24/04/91    J-C GALLAND  (LNH)
!                                 09/11/94    P LANG / TRIGRID (LHF)
!                               07/96    P CHAILLET / FASTTABS (LHF)
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
! |    MAILLE      | -->| NOM DU MAILLEUR UTILISE
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
      USE DECLARATIONS_STBTEL, ONLY: NPOIN
      USE INTERFACE_STBTEL, EX_PROJEC => PROJEC
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPTFR, NBAT, NBFOND
      INTEGER, INTENT(IN) :: NFOND(*) , NBOR(NPTFR,2)
      INTEGER, INTENT(IN) :: NGEO, KP1BOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: XRELV(*) , YRELV(*) , ZRELV(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*) , Y(*) , DM
      DOUBLE PRECISION, INTENT(IN) :: CORTRI
      DOUBLE PRECISION, INTENT(INOUT) :: ZF(*)
      CHARACTER(LEN=72), INTENT(IN) :: FOND(NBFOND)
      CHARACTER(LEN=9), INTENT(IN) ::  MAILLE
      LOGICAL, INTENT(IN) :: FONTRI
!
      INTEGER I , NPT , IVOIS
      INTEGER NP(5)
      DOUBLE PRECISION DIST , DIST2
!
!=======================================================================
!  LECTURE DES FICHIERS DES FONDS
!=======================================================================
!
      CALL LECFON (XRELV,YRELV,ZRELV,NBAT,NFOND,NBFOND,NP,NPT,
     &             FONTRI,CORTRI,MAILLE,NGEO)
!
      IF (.NOT.FONTRI) THEN
        IF (NBFOND.NE.0) WRITE(LU,4000)
!
        DO I = 1,NBFOND
          WRITE(LU,4100) I,FOND(I),I,NP(I)
        ENDDO
      ENDIF
!
!=======================================================================
!  DETERMINATION DE LA COTE DU FOND AU POINT I PAR INTERPOLATION
!  SUR LES POINTS NON EXTERIEURS AU DOMAINE
!=======================================================================
!
      CALL FASP (X,Y,ZF,NPOIN,XRELV,YRELV,ZRELV,NPT,NBOR,KP1BOR,
     &           NPTFR,DM)
!
!=======================================================================
!  CERTAINS POINTS N'ONT PU ETRE TRAITES PAR FASP FAUTE DE DONNEES
!  LEUR PROFONDEUR A ETE MISE A -1.E6
!  ON AFFECTE A CES POINTS LA PROFONDEUR DE LEUR PLUS PROCHE VOISIN.
!=======================================================================
!
      DO I=1,NPOIN
        IF(ZF(I).LT.-0.9D6) THEN
          DIST = 1.D12
          DO IVOIS = 1 , NPOIN
            DIST2 = ( X(I)-X(IVOIS) )**2 + ( Y(I)-Y(IVOIS) )**2
            IF(DIST2.LT.DIST.AND.ZF(IVOIS).GT.-0.9D6) THEN
              DIST = DIST2
              ZF(I) = ZF(IVOIS)
            ENDIF
          ENDDO
          WRITE(LU,4200) I,X(I),Y(I),ZF(I)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
 4000 FORMAT(//,1X,'INTERPOLATION OF BOTTOM TOPOGRAPHY FROM :',/,
     &          1X,'-----------------------------------------',/)
 4100 FORMAT(1X,'BOTTOM ',I1,' : ',A72,/,
     &       1X,'NUMBER OF POINTS READ IN THE BOTTOM TOPOGRAPHY FILE ',
     &       I1,' : ',I6,/)
 4200 FORMAT('POINT : ',I5,' X = ',F10.1,' Y = ',F10.1,
     &       '  NO DATA , ZF : ',F8.2)
!
      RETURN
      END
