!                   ********************
                    SUBROUTINE USER_BORH
!                   ********************
!
!
!***********************************************************************
! ARTEMIS
!***********************************************************************
!
!brief    TAKES INTO ACCOUNT USER-SPECIFIED BOUNDARY CONDITIONS.
!+        THEY ARE GIVEN BY SEGMENT.
!
!history  J-M HERVOUET (LNH)
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!     DEBUT : DECLARATIONS SUPPLEMENTAIRES POUR LECTURE FICHIER COWADIS
!
!     ATTENTION, POUR L'INSTANT BEAUCOUP DE CHOSES EN DUR !!
!
      LOGICAL COUPLA
!
      INTEGER NCOW,NPTH
      INTEGER NP,JB,IERR
!
      DOUBLE PRECISION ATT(1)
!
!     IL FAUT DIMENSIONNER LES TABLEAUX EN DUR
!
      DOUBLE PRECISION XCOWA(15000) ,YCOWA(15000)
      DOUBLE PRECISION HSCOWA(15000),DMCOWA(15000)
      DOUBLE PRECISION HSARTE(12000),DMARTE(12000)
!
      CHARACTER(LEN=80) TTITRE
      CHARACTER(LEN=32) VAR1, VAR2
      CHARACTER(LEN=8) :: FFORMAT
      INTEGER TYPE_ELEM
!
!-----------------------------------------------------------------------
!
! DEBUT : LECTURE FICHIER COWADIS
!
!     RECUPERATION DES RESULTATS ISSUS DE COWADIS
!     POUR DEFINIR LES CONDITIONS AUX LIMITES ARTEMIS
!
!     ATTENTION ENCORE BEAUCOUP DE CHOSES EN DUR
!
!     **********************************************************
!
!     SPECIFIER LE NUMERO DU PAS DE TEMPS DE CALCUL DANS COWADIS
!     (NPTH POURRA DEVENIR 1 MOT-CLE DANS UNE PROCHAINE VERSION)
!
      NPTH = 1
!
!     **********************************************************
!
      COUPLA = .TRUE.
!
!     (COUPLA POURRA DEVENIR 1 MOT-CLE DANS UNE PROCHAINE VERSION)
!
      NCOW   = ART_FILES(ARTBI1)%LU
      FFORMAT = 'SERAFIN '
      TYPE_ELEM = 0
!
      IF (COUPLA) THEN
!
!       LECTURE DU TITRE DU FICHIER COWADIS
!
        CALL GET_MESH_TITLE(FFORMAT,NCOW,TTITRE,IERR)
        CALL CHECK_CALL(IERR, 'BORH:GET_MESH_TITLE')
!
!       NPOIN
!
        CALL GET_MESH_NPOIN(FFORMAT,NCOW,TYPE_ELEM,NP,IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_MESH_NPOIN')
!
        WRITE(LU,*) '------------------------------------------'
        IF (LNG.EQ.1) WRITE(LU,230)
        IF (LNG.EQ.2) WRITE(LU,231)
 230    FORMAT(/,1X,'BORH : LECTURE DU FICHIER COWADIS')
 231    FORMAT(/,1X,'BORH : READING COWADIS FILE')
        IF (LNG.EQ.1) WRITE(LU,240) NP
        IF (LNG.EQ.2) WRITE(LU,241) NP
 240    FORMAT(/,1X,'NOMBRE DE POINTS DU MAILLAGE COWADIS :',1X,I7)
 241    FORMAT(/,1X,'NUMBER OF NODES OF COWADIS MESH :',1X,I7)
        IF (LNG.EQ.1) WRITE(LU,250)
        IF (LNG.EQ.2) WRITE(LU,251)
 250    FORMAT(/,1X,'MAILLAGES ARTEMIS ET COWADIS DIFFERENTS :',1X,
     &         'ON INTERPOLE')
 251    FORMAT(/,1X,'COWADIS AND ARTEMIS MESHES ARE DIFFERENT :',1X,
     &         'INTERPOLATION')
!
!       XCOWA ET YCOWA
!
        CALL GET_MESH_COORD(FFORMAT, NCOW, 1, 2, NP, XCOWA, IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_MESH_COORD:X')
        CALL GET_MESH_COORD(FFORMAT, NCOW, 2, 2, NP, YCOWA, IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_MESH_COORD:Y')
!
!       PAS DE TEMPS ET VARIABLES
!
        VAR1 = 'HAUTEUR_HM0     M               '
        VAR2 = 'TETA_MOYEN      DEG             '
        CALL GET_DATA_VALUE(FFORMAT, NCOW, NPTH, VAR1, HSCOWA, NP,IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_DATA_VALUE:HS')
        CALL GET_DATA_VALUE(FFORMAT, NCOW, NPTH, VAR2, DMCOWA, NP,IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_DATA_VALUE:DM')
        CALL GET_DATA_TIME(FFORMAT, NCOW, NPTH, ATT(1), IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_DATA_TIME')
!
!       IMPRESSIONS SUR LE LISTING
!
        IF (LNG.EQ.1) WRITE(LU,260) ATT
        IF (LNG.EQ.2) WRITE(LU,261) ATT
 260    FORMAT(/,1X,'TEMPS DU CALCUL COWADIS RETENU :',1X,F10.2,' s')
 261    FORMAT(/,1X,'TIME READ IN COWADIS FILE :',1X,F10.2,' s')
        IF (LNG.EQ.1) WRITE(LU,270)
        IF (LNG.EQ.2) WRITE(LU,271)
 270    FORMAT(/,1X,'VARIABLES DE COWADIS RETENUES :')
 271    FORMAT(/,1X,'VARIABLES READ IN COWADIS FILE :')
        WRITE(LU,280) VAR1, VAR2
 280    FORMAT(/,5X,'=> ',A32,/,5X,'=> ',A32)
!
!       MODIFICATION DE LA VARIABLE DMARTE POUR ARTEMIS :
!       CHANGEMENT DE REPERE ET D'UNITE
!
        DO I=1,NP
          DMCOWA(I) = 90.D0 - DMCOWA(I)
        ENDDO
!
        IF (LNG.EQ.1) WRITE(LU,290)
        IF (LNG.EQ.2) WRITE(LU,291)
 290    FORMAT(/,1X,'BORH : FIN DE LECTURE DU FICHIER COWADIS')
 291    FORMAT(/,1X,'BORH : END OF READING COWADIS FILE')
        WRITE(LU,*) ' '
        WRITE(LU,*) '------------------------------------------'

        REWIND(NCOW)
!
!       INTERPOLATION

        CALL FASPDA (X,Y,HSARTE,NPOIN,NPTFR,MESH%NBOR%I,
     &       XCOWA,YCOWA,HSCOWA,NP)
        CALL FASPDA (X,Y,DMARTE,NPOIN,NPTFR,MESH%NBOR%I,
     &       XCOWA,YCOWA,DMCOWA,NP)

      ENDIF
!
!
! FIN : FIN LECTURE FICHIER COWADIS
!
!--------------------------------------------------------------------
!
! CONDITIONS AUX LIMITES
! UN SEGMENT EST SOLIDE SI IL EST DE TYPE KLOG.
! UN SEGMENT EST ONDE INCIDENTE SI IL EST DE TYPE KINC.
! UN SEGMENT EST UNE ENTREE SI IL EST DE TYPE KENT.
! UN SEGMENT EST UNE SORTIE SI IL EST DE TYPE KSORT.
!
! TOUS LES ANGLES SONT EN DEGRES
!                         ------
! ---------------------------------------
! INITIALISATION DES VARIABLES PAR DEFAUT
! ---------------------------------------
      TETAB%R(:) = TETAH
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 1.D0
!
! ------------
! PAROI SOLIDE
! ------------
!

      DO I=1,NPTFR
        JB=BOUNDARY_COLOUR%I(I)
! ------------
! PAROI SOLIDE
! ------------
!
        IF(JB.GE.1.AND.JB.LE.84)THEN
          LIHBOR%I(I) = KLOG
          RP%R(I) = 0.5D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF

        IF(JB.GE.85.AND.JB.LE.142)THEN
          LIHBOR%I(I) = KLOG
          RP%R(I) = 1.D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF

        IF(JB.GE.143.AND.JB.LE.260)THEN
          LIHBOR%I(I) = KLOG
          RP%R(I) = 0.5D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
! ------------
! FRONTIERE ONDE INCIDENTE
! ------------
!
        IF(JB.GE.261.AND.JB.LE.281)THEN
          LIHBOR%I(I) = KINC
          HB%R(I)     = HSARTE(MESH%NBOR%I(I))
          TETAB%R(I)  = DMARTE(MESH%NBOR%I(I))
          TETAP%R(I)  = 0.D0
          ALFAP%R(I)  = 0.D0
        ENDIF
!
! ------------
! PAROI SOLIDE
! ------------
!
        IF(JB.GE.282.AND.JB.LE.302)THEN
          LIHBOR%I(I) = KLOG
          RP%R(I) = 0.5D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF

      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
