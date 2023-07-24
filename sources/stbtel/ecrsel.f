!                       *****************
                        SUBROUTINE ECRSEL
!                       *****************
!
     &(VAINIT,IKINIT,NPINIT,NEINIT,SHP,ELT,NPOIN,NPOIN1,NPMAX,W,
     & X,ZF,NSFOND,NCOLOR,COLOR,VAR,NVARIN,NVAROU,NVAR2,STD,FUSION,
     & NRES,NGEO,NFO1,MAILLE,TEXTE)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2           11/02/93    J.M. JANIN
!***********************************************************************
!
!   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
!               MAILLAGE DANS LE FICHIER D'ENTREE SELAFIN
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
      USE INTERFACE_STBTEL, EX_ECRSEL => ECRSEL
      USE DECLARATIONS_STBTEL, ONLY: FFORMAT, OUT_FORMAT
      USE INTERFACE_HERMES
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPINIT,NEINIT,NPOIN,NPMAX
      INTEGER, INTENT(IN) :: NPOIN1,NVAR2
      DOUBLE PRECISION, INTENT(INOUT) :: VAINIT(NPINIT)
      DOUBLE PRECISION, INTENT(IN) :: SHP(NPMAX,3)
      INTEGER, INTENT(IN) :: IKINIT(NEINIT,3),ELT(NPOIN)
      REAL, INTENT(INOUT) :: W(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      INTEGER, INTENT(IN) :: NSFOND
      INTEGER, INTENT(IN) :: NCOLOR(NPOIN)
      LOGICAL, INTENT(IN) :: COLOR,FUSION
      DOUBLE PRECISION, INTENT(INOUT) :: VAR(NPOIN)
      INTEGER, INTENT(IN) :: NVARIN,NVAROU
      CHARACTER(LEN=3), INTENT(IN) :: STD
      INTEGER, INTENT(IN) :: NGEO,NRES,NFO1
      CHARACTER(LEN=9), INTENT(IN) :: MAILLE
      CHARACTER(LEN=32), INTENT(IN) ::TEXTE(NVARIN)
!
      INTEGER NPOIN2
      INTEGER IVAR,IPOIN,I
!
      DOUBLE PRECISION TIME
      INTEGER NTIMESTEP, IERR
      CHARACTER(LEN=32) :: VARNAME
!
!=======================================================================
!
      NPOIN2 = NPINIT - NPOIN1

      NTIMESTEP = 1

      IF (MAILLE.EQ.'SELAFIN') THEN

        CALL GET_DATA_NTIMESTEP(FFORMAT, NGEO, NTIMESTEP, IERR)
        CALL CHECK_CALL(IERR, 'ECRSEL:GET_DATA_NTIMESTEP')

        DO I=0,NTIMESTEP-1
          CALL GET_DATA_TIME(FFORMAT, NGEO, I, TIME, IERR)
          CALL CHECK_CALL(IERR, 'ECRSEL:GET_DATA_TIME')

          DO IVAR = 1, NVARIN
            CALL GET_DATA_VALUE(FFORMAT, NGEO, I, TEXTE(IVAR),
     &                          VAINIT, NPOIN1, IERR)
            CALL CHECK_CALL(IERR,
     &                     'ECRSEL:GET_DATA_VALUE:'//TEXTE(IVAR))

            IF(FUSION) THEN
              CALL GET_DATA_VALUE(FFORMAT, NGEO, I, TEXTE(IVAR),
     &                            VAINIT(NPOIN1+1:NPINIT), NPOIN2, IERR)
              IF(IERR.NE.0) THEN
                VAINIT(NPOIN1+1:NPINIT) = 0.D0
              ENDIF
            ENDIF

            ! WRITTING DATA VALUE
            IF(IVAR.EQ.NSFOND) THEN
              VAR = ZF
            ELSE
              DO IPOIN = 1,NPOIN
                VAR(IPOIN) = VAINIT(IKINIT(ELT(IPOIN),1))*SHP(IPOIN,1)
     &                     + VAINIT(IKINIT(ELT(IPOIN),2))*SHP(IPOIN,2)
     &                     + VAINIT(IKINIT(ELT(IPOIN),3))*SHP(IPOIN,3)
              ENDDO
            ENDIF
            CALL ADD_DATA(OUT_FORMAT,NRES,TEXTE(IVAR),TIME,I,
     &                    IVAR==1,VAR,NPOIN,IERR)
            CALL CHECK_CALL(IERR,
     &                      'ECRSEL:ADD_DATA_VALUE:'//TEXTE(IVAR))
          ENDDO ! IVAR
        ENDDO !I
      ENDIF ! MAILLE=='SELAFIN'

      IF(NSFOND.EQ.NVARIN+1.OR.MAILLE.EQ.'ADCIRC') THEN
        VARNAME = REPEAT(' ', 32)
        VARNAME = 'BOTTOM          M'
        ! WRITING BOTTOM FOR ALL TIMESTEPS
        IVAR=MAX(NSFOND, 1)
        DO I=0,NTIMESTEP-1
          CALL ADD_DATA(OUT_FORMAT,NRES,VARNAME,0.D0,I,
     &                  IVAR==1,ZF,NPOIN,IERR)
          CALL CHECK_CALL(IERR, 'ECRSEL:ADD_DATA_VALUE:BOTTOM')
        ENDDO
      ENDIF
      IF(NVAROU.EQ.0.AND.MAILLE.NE.'ADCIRC') THEN
        VARNAME = REPEAT(' ', 32)
        VARNAME = 'MAILLAGE'
        CALL ADD_DATA(OUT_FORMAT,NRES,VARNAME,0.D0,0,
     &                .TRUE.,X,NPOIN,IERR)
        CALL CHECK_CALL(IERR, 'ECRSEL:ADD_DATA_VALUE:BOTTOM')
      ENDIF
!
!=======================================================================
!
      RETURN
      END
