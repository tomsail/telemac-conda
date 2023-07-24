!                       *****************
                        SUBROUTINE LECFON
!                       *****************
!
     &( XRELV , YRELV , ZRELV , NBAT , NFOND , NBFOND ,  NP ,
     &  NPT , FONTRI , CORTRI , MAILLE, NGEO )
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2         25/03/92    J-C GALLAND  (LNH)
!                                 09/11/94    P. LANG / LHF (TRIGRID)
!                                  07/96    P. CHAILLET / LHF (FASTTABS)
!***********************************************************************
!
! FONCTION : LECTURE DES FICHIERS DE BATHYMETRIE
!
!----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |    XRELV,YRELV | -->|  COORDONNEES DES POINTS DE BATHY
! |    ZRELV       | -->|  COTES DES POINTS DE BATHY
! |    NBAT        | -->|  NOMBRE DE POINTS DE BATHY
! |    NFOND       | -->|  CANAUX DES FICHIERS DES FONDS
! |    NBFOND      | -->|  NOMBRE DE FICHIERS FONDS DONNES PAR
! |                |    |  L'UTILISATEUR (5 MAXI)
! |    FOND        | -->|  NOM DES FICHIERS DES FONDS
! |    NP          | -->|  NOMBRES DE POINTS LUS PAR LECFON DANS LES
! |                |    |  FICHIERS DES FONDS
! |    NPT         | -->|  NOMBRE TOTAL DE POINTS DE BATHYMETRIE
! |    FONTRI      | -->|  INDICATEUR DE LECTURE DES FONDS DANS TRIGRID
! |    CORTRI      | -->|  VALEUR DE LA CORRECTION DES FONDS DE TRIGRID
! |    MAILLE      | -->| NOM DU MAILLEUR UTILISE
! |________________|____|______________________________________________
! | COMMON :       |    |
! |                |    |
! |  FICH:         |    |
! |    NRES        | -->|  NUMERO DU CANAL DU FICHIER GEOMETRIE
! |    NGEO        | -->|  NUMERO DU CANAL DU FICHIER UNIVERSEL
! |    NLIM        | -->|  NUMERO DU CANAL DU FICHIER DYNAM
! |    NFO1        | -->|  NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
! |________________|____|______________________________________________
!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
!
! APPELE PAR : INTERP
! APPEL DE : -
!
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(INOUT) :: XRELV(*) , YRELV(*) , ZRELV(*)
      INTEGER, INTENT(IN) :: NFOND(*) , NBAT , NBFOND
      INTEGER, INTENT(INOUT) :: NP(5), NPT
      LOGICAL, INTENT(IN) :: FONTRI
      DOUBLE PRECISION, INTENT(IN) :: CORTRI
      CHARACTER(LEN=9), INTENT(IN) ::  MAILLE
      INTEGER, INTENT(IN) :: NGEO
!
      INTEGER I
      INTEGER IDUMMY , ITRI
!
!     REELS DECLARES SIMPLES PRECISION POUR LECTURE FICHIER SINUSX
!
      REAL   XSP , YSP , ZSP
!
      CHARACTER(LEN=1)  C
!
! Ajout PCt - 11/07/96
      CHARACTER(LEN=80) LIGNE
!
!
      INTRINSIC DBLE
!
!=======================================================================
!  INITIALISATION
!=======================================================================
!
      DO I=1,NBAT
        XRELV(I)=0.D0
        YRELV(I)=0.D0
        ZRELV(I)=0.D0
      ENDDO
!
!=======================================================================
! LECTURE DES FICHIERS FOND
!=======================================================================
!
      NP(1) = 0
      NP(2) = 0
      NP(3) = 0
      NP(4) = 0
      NP(5) = 0
      NPT   = 0
!
! DANS LE CAS DU MAILLEUR TRIGRID, SI FONTRI=VRAI ON LIT LA BATHY
! DIRECTEMENT DANS LE FICHIER UNIVERSEL, SINON ON EFFECTUE LE TRAITEMENT
! NORMAL.
!
! Modification PCt le 11/07/96
! ajout du cas FASTTABS
!
      IF (FONTRI) THEN
        IF (MAILLE.EQ.'TRIGRID') THEN
          WRITE (LU,4040)
          REWIND (NGEO)
          READ (NGEO,'(//)')
1         CONTINUE
            READ (NGEO,*,END=9000,ERR=9000) IDUMMY,XSP,YSP,ITRI,ZSP
            NPT = NPT + 1
            XRELV(NPT) = DBLE(XSP)
            YRELV(NPT) = DBLE(YSP)
            ZRELV(NPT) = DBLE(-ZSP) + CORTRI
            GOTO 1
9000      CONTINUE
          NP(1) = NPT
          WRITE (LU,4050) NPT
        ELSEIF (MAILLE.EQ.'FASTTABS') THEN
!
! Ajout PCt - FASTTABS - le 11/07/1996
!
          WRITE (LU,4070)
          REWIND (NGEO)
2         CONTINUE
            READ (NGEO,'(A)',END=9010,ERR=8000) LIGNE
            IF (LIGNE(1:3).EQ.'GNN') THEN
              READ(LIGNE(4:80),*,ERR=8000,END=8000) IDUMMY,XSP,YSP,ZSP
              NPT = NPT + 1
              XRELV(NPT) = DBLE(XSP)
              YRELV(NPT) = DBLE(YSP)
              ZRELV(NPT) = DBLE(ZSP)
            ENDIF
            GOTO 2
9010      CONTINUE
        ENDIF
! temporaire
      ELSE
!
        DO I = 1,NBFOND
!
          REWIND NFOND(I)
30        READ(NFOND(I),1000,END=40) C
          IF (C(1:1).NE.'C'.AND.C(1:1).NE.'B') THEN
            BACKSPACE ( UNIT = NFOND(I) )
            NP(I)=NP(I)+1
            NPT  =NPT +1
            IF (NPT.GT.NBAT) THEN
              WRITE(LU,4020) NBAT
              CALL PLANTE(1)
              STOP
            ENDIF
!
! LECTURE FICHIER SINUSX SIMPLE PRECISION PUIS -> DOUBLE PRECISION
!
            READ (NFOND(I),*) XSP,YSP,ZSP
            XRELV(NPT) = DBLE(XSP)
            YRELV(NPT) = DBLE(YSP)
            ZRELV(NPT) = DBLE(ZSP)
!
          ENDIF
          GOTO 30
40        CONTINUE
          IF (NP(I).EQ.0) THEN
            WRITE(LU,4030) I
            CALL PLANTE(1)
            STOP
          ENDIF
!
        ENDDO! I
      ENDIF
!
! Ajout PCt - FASTTABS - le 11/07/1996
!
      RETURN
 8000 CONTINUE
      WRITE (LU,4001)
 4001 FORMAT (//,1X,'****************************'
     &        ,/,1X,'SUBROUTINE LECFON:'
     &        ,/,1X,'ERROR READING FASTTABS FILE.'
     &        ,/,1X,'****************************')
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
1000  FORMAT(A1)
4020  FORMAT(/,'****************************************************',/,
     &         'THE NUMBER OF BATHYMETRIC POINTS IS     ',/,
     &         'GREATER THAN:',                                  1I6,/,
     &         'CHANGE THE FOLLOWING PARAMETER ',/,
     &         'IN THE STEERING FILE: ',/,
     &         'MAXIMUM NUMBER OF BATHYMETRIC POINTS '             ,/,
     &         '****************************************************')
4030  FORMAT(/,'******************************************',/,
     &         'THE BOTTOM TOPOGRAPHY FILE ',I1,' IS EMPTY|',/,
     &         '******************************************',/)
4040  FORMAT(/,'****************************************',/,
     &         'SUBROUTINE LECFON',/,
     &         'READING BATHYMETRY IN TRIGRID MESH FILE',/
     &         '****************************************',/)
4050  FORMAT(/,'****************************************',/,
     &         'SUBROUTINE LECFON',/,
     &         'NUMBER OF BATHYMETRIC POINTS IN TRIGRID FILE : ',
     &         I5,/
     &         '****************************************',/)
4070  FORMAT(/,'****************************************',/,
     &         'SUBROUTINE LECFON',/,
     &         'NUMBER OF BATHYMETRIC POINTS IN FASTTABS FILE: ',
     &         I5,/
     &         '****************************************',/)
!
      END SUBROUTINE
