!                       *****************
                        SUBROUTINE LECSIM
!                       *****************
!
     &( X , Y , IKLE , NCOLOR , TITRE , NOP5 , NGEO )
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2         25/02/92    J-C GALLAND  (LNH)
!***********************************************************************
!
!     FONCTION  :  LECTURE DU FICHIER DE LA GEOMETRIE CREE PAR SIMAIL
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
! |   NCOLOR       |<-- | TABLEAU DES COULEURS DES POINTS DU MAILLAGE
! |   TITRE        |<-- | TITRE DU MAILLAGE
! |   NOP5         | -->| TABLEAU DE TRAVAIL POUR LA LECTURE DE LA SD
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
! |    NGEO       | -->| NUMERO DU CANAL DU FICHIER MAILLEUR
! |    NLIM      | -->| NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
! |    NFO1      |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
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
      USE DECLARATIONS_STBTEL, ONLY: NELEM,MESH,NPOIN,NELMAX
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(INOUT) :: X(*) , Y(*)
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,4) , NCOLOR(*)
      CHARACTER(LEN=80), INTENT(INOUT) :: TITRE
      INTEGER, INTENT(INOUT) :: NOP5(*)
      INTEGER, INTENT(IN) :: NGEO
!
      INTEGER ERR
      INTEGER I,J,K
      INTEGER LONG, NTASD
      INTEGER NCGE, NMAE , NDSDE , NNO , NCOPNP ,NPO
      INTEGER INING, NBEGM , INDIC
!
      REAL, DIMENSION(:), ALLOCATABLE :: X1,Y1
!
!
      INTRINSIC DBLE
!
!-----------------------------------------------------------------------
!
      ALLOCATE(X1(NPOIN),STAT=ERR)
      ALLOCATE(Y1(NPOIN),STAT=ERR)
!
      IF(ERR.NE.0) THEN
        WRITE(LU,8000) ERR
8000    FORMAT(1X,'LECSIM: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &            'ERROR CODE: ',1I6)
      ENDIF
!
!=======================================================================
!   INITIALISATION
!=======================================================================
!
      REWIND NGEO
!
      DO I=1,NPOIN
        X(I) = 9999999.D0
        Y(I) = 9999999.D0
        NCOLOR(I) = 99999
      ENDDO
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER (1ER ENRGISTREMENT DE LA SD)
! POUR LES LECTURES BIDON, ON UTILISE NOP5
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER (TABLEAU NOP0) . LECTURE DU TITRE
! POUR LES LECTURES BIDON, ON UTILISE NOP5
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,TITRE,(NOP5(I),I=1,11),NTASD
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER (TABLEAUX NOP1 ET ASSOCIES)
! POUR LES LECTURES BIDON, ON UTILISE NOP5
!=======================================================================
!
      IF (NTASD.GT.0) THEN
        READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
        DO I=1,NTASD
          READ(NGEO,ERR=110,END=120) LONG,(NOP5(J),J=1,LONG)
        ENDDO
      ENDIF
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER (TABLEAU NOP2)
! POUR LES LECTURES BIDON, ON UTILISE NOP5
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
      NCOPNP = NOP5(4)
      NBEGM  = NOP5(25)
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER (TABLEAU NOP3)
! POUR LES LECTURES BIDON, ON UTILISE NOP5
!=======================================================================
!
      IF (NBEGM.NE.0) THEN
        READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
      ENDIF
!
!=======================================================================
! LECTURE SEQUENTIELLE DES COORDONNEES DES NOEUDS (TABLEAU NOP4)
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,(X1(I),Y1(I),I=1,NPOIN)
      DO I=1,NPOIN
        X(I) = DBLE(X1(I))
        Y(I) = DBLE(Y1(I))
      ENDDO
!
!=======================================================================
! LECTURE SEQUENTIELLE DES IKLE (TABLEAU NOP5)
! POUR LES LECTURE BIDON, ON UTILISE NOP5 ET LONG
!=======================================================================
!
      INDIC = 0
      READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
      DO I=1,NELEM
        INDIC = INDIC +1
        NCGE  = NOP5(INDIC)
        INDIC = INDIC +1
        NMAE  = NOP5(INDIC)
        INDIC = INDIC +1
        NDSDE = NOP5(INDIC)
        INDIC = INDIC +1
        NNO   = NOP5(INDIC)
! NNO : NOMBRE DE NOEUDS PAR ELEMENT
        IF ( (NNO.EQ.4.AND.MESH.NE.2) .OR. (NNO.EQ.3.AND.MESH.NE.3) )
     &  THEN
          WRITE(LU,4000)
          CALL PLANTE(1)
          STOP
        ENDIF
        DO K=1,NNO
          INDIC = INDIC +1
          IKLE(I,K) = NOP5(INDIC)
        ENDDO
        IF (NCOPNP.NE.1) THEN
          INDIC = INDIC +1
          NPO = NOP5(INDIC)
          DO K=1,NPO
            INDIC = INDIC +1
          ENDDO
        ENDIF
!  NMAE :
        IF (NMAE.NE.0) THEN
          INDIC = INDIC +1
          INING = NOP5(INDIC)
          DO K=2,NMAE
            IF (INING.EQ.3) THEN
              INDIC = INDIC +1
              NCOLOR(IKLE(I,K-1)) = NOP5(INDIC)
            ELSE IF(INING.EQ.2) THEN
              INDIC = INDIC +1
              IF (K.GT.NNO+1) NCOLOR(IKLE(I,K-(NNO+1))) = NOP5(INDIC)
            ELSE IF(INING.EQ.1) THEN
              INDIC = INDIC +1
              IF (K.GT.2*NNO+1)
     &        NCOLOR(IKLE(I,K-(2*NNO+1))) = NOP5(INDIC)
            ENDIF
          ENDDO
        ENDIF
      ENDDO !I
!
      GOTO 80
!
 110  CONTINUE
      WRITE(LU,4100)
 120  CONTINUE
      WRITE(LU,4200)
!
 80   CONTINUE
!
!=======================================================================
!
      DEALLOCATE (X1)
      DEALLOCATE (Y1)
!
!=======================================================================
!
4000  FORMAT(//,'*********************************************',/,
     &          'LECSIM : THERE IS NO LINK BETWEEN THE NUMBER  ',/,
     &          'OF POINTS BY ELEMENT AND THE TYPE OF ELEMENTS ',/,
     &          '**********************************************',//)
4100  FORMAT(//,'**********************************************',/,
     &          'LECSIM : ERROR IN READING FILE SIMAIL         ',/,
     &          '**********************************************',//)
4200  FORMAT(//,'*************************************************',/,
     &          'LECSIM : ATTEMPT TO READ AFTER END OF FILE SIMAIL ',/,
     &          '*************************************************',//)
!
      RETURN
      END
