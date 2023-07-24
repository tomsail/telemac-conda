!                       *****************
                        SUBROUTINE COUPEV
!                       *****************
!
     &(AT,Z,U,V,W,
     & SHP,IMSEG,X2DV,Y2DV,DISTOR,IKLES,
     & ELEM,NC2DV,NPOIN2,NELEM2,NCOU,FFORMAT,IM,JM,
     & TITCAS,NVA3,TAB,TEXTLU,IENRE)
!
!***********************************************************************
! POSTEL3D VERSION 6.2   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!     FONCTION  : ECRIT POUR CHAQUE COUPE VERTICALES LES VARIABLES
!                      D'UN PAS DE TEMPS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !   IREC         ! -->! PAS DE TEMPS TRAITE                          !
! !   AT           ! -->! TEMPS CORRESPONDANT AU PAS TRAITE            !
! !   Z            ! -->! COTES DES NOEUDS                             !
! !   U,V,W        ! -->! COMPOSANTES 3D DE LA VITESSE                 !
! !   TA,TP        ! -->! CONCENTRATIONS DES TRACEURS                  !
! !   NUX,NUY,NUZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES VITESSES  !
! !   NAX,NAY,NAZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES TR.ACTIFS !
! !   NPX,NPY,NPZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES TR.PASSIFS!
! !   RI           ! -->! NOMBRE DE RICHARDSON                         !
! !   AK,EP        ! -->! VARIABLES DU MODELE K-EPSILON                !
! !   RHO          ! -->! ECARTS RELATIFS DE DENSITE                   !
! !   SHP          ! -->! COORDONNEES BARYCENTRIQUES DES PTS DE COUPE  !
! !   TAB1,2,3     !<-- ! TABLEAU DE TRAVAIL POUR PROJETER LES VAR.    !
! !   NSEG         ! -->! NOMBRE DE SEGMENTS CONSTITUANT CHAQUE COUPE  !
! !   IMSEG        ! -->! NOMBRE DE POINTS PAR SEGMENTS                !
! !   X2DV         ! -->! ABSCISSES DES SOMMETS DES COUPES VERTICALES  !
! !   Y2DV         ! -->! ORDONNEES DES SOMMETS DES COUPES VERTICALES  !
! !   DISTOR       ! -->! DISTORSION SUIVANT Z DE CHAQUE COUPE VERTICALE
! !   IKLES        ! -->! TABLE DE CONNECTIVITE                        !
! !   ELEM         ! -->! NUMERO DES ELEMENTS CONTENANT LES PTS DE COUPE
! !   NC2DV        ! -->! NOMBRE DE COUPES VERTICALES                  !
! !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
! !   NELEM2       ! -->! NOMBRE D'ELEMENTS DU MAILLAGE 2D             !
! !   NCOU         ! -->! NUMERO DE CANAL - 1 DE LA PREMIERE COUPE     !
! !   IM (LU)      ! -->! NOMBRE DE PTS DE COUPE SUIVANT L'HORIZONTALE !
! !   JM (=NPLAN)  ! -->! NOMBRE DE PTS DE COUPE SUIVANT LA VERTICALE  !
! !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
! !   TITCAS       ! -->! TITRE A PORTER SUR CHAQUE COUPE              !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : POSTEL3D
! SOUS-PROGRAMME APPELES : ECRDEB , ECRI2
!
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module

! JUNE 2012 - P.LANG / INGEROP : SERAFIN OUTPUT FORMAT
!**********************************************************************
!
      USE BIEF
      USE INTERFACE_HERMES
      USE DECLARATIONS_POSTEL3D, ONLY: PREZ => Z
      USE INTERFACE_POSTEL3D, EX_COUPEV => COUPEV
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,INTENT(IN) :: NPOIN2,NELEM2,IM,JM,NC2DV
      INTEGER, INTENT(INOUT) :: NCOU(NC2DV)
      DOUBLE PRECISION ,INTENT(IN) ::AT
      DOUBLE PRECISION , INTENT(INOUT) :: SHP(IM,3,NC2DV)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TAB
      INTEGER , INTENT(IN) :: IENRE
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN2,JM),V(NPOIN2,JM)
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,JM),W(NPOIN2,JM)
      INTEGER, INTENT(IN) :: IKLES(3,NELEM2)
      INTEGER, INTENT(IN) :: ELEM(IM,NC2DV)
      INTEGER, INTENT(INOUT) :: NVA3
      INTEGER, INTENT(IN) :: IMSEG(49,NC2DV)
      DOUBLE PRECISION, INTENT(IN) :: X2DV(50,NC2DV),Y2DV(50,NC2DV)
      DOUBLE PRECISION, INTENT(IN) :: DISTOR(NC2DV)
      CHARACTER(LEN=8),  INTENT(INOUT) :: FFORMAT
      CHARACTER(LEN=32), INTENT(IN) ::TEXTLU(100)
      CHARACTER(LEN=72), INTENT(IN) ::TITCAS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      DOUBLE PRECISION TAB1(IM,JM),TAB2(IM,JM),TAB3(IM,JM)
      DOUBLE PRECISION LGDEB,LGSEG,ALFA,COST,SINT,A1,A2,A3,U1,V1
!
      INTEGER IB(10),IC,N1,N2,N3,I,J,K,CANAL
      INTEGER ISEG,IDSEG,IFSEG
!
!     NEW VARIABLES FOR SERAFIN FORMAT
!
      INTEGER IKLE(((IM-1)*(JM-1))*2,3),IPOBO(IM*JM),NUMELEM
!
!     END OF NEW VARIABLES
!
      LOGICAL FLAG
!
!
!
      CHARACTER(LEN=32) :: VAR_NAME
      INTEGER :: IERR,IREC
      INTEGER DATE(3),TIME(3)
!
!***********************************************************************
!
!  LISTE DE FUTURS PARAMETRES DEJA PREVUS.(SEUL LES PREMIERS SERVENT)
!
      DO I=1,10
        IB(I)=0
      ENDDO
!   ECRITURE ECLATEE DES RESULTATS (CONVENTION LEONARD)
      IB(2)=1
!
!-----------------------------------------------------------------------
!
!  POUR CHAQUE COUPE VERTICALE FAIRE :
!
      IREC = 0
      DO IC = 1,NC2DV
!
!    OUVERTURE DU FICHIER + ENREGISTREMENT DES PREMIERS PARAMETRES
!    -------------------------------------------------------------
!
        CALL ECRDEB(NCOU(IC),FFORMAT,TITCAS,NVA3,.FALSE.,
     &              TEXTLU,IC,IENRE)
!
!       CALCUL DES AUTRES PARAMETRES DE L'ENTETE
!       ----------------------------------------
!
!       MAILLAGE LEONARD ASSOCIE A LA COUPE IC ET AU PAS DE TEMPS IT
!
        ISEG = 0
        IFSEG = 1
        LGDEB = 0.D0
        LGSEG = 0.D0
!
        DO I = 1,IM
!
!         COORDONNEE HORIZONTALE SUIVANT LE PLAN DE COUPE (X)
!
          IF (I.GT.IFSEG.OR.I.EQ.1) THEN
            ISEG = ISEG + 1
            IDSEG = IFSEG
            IFSEG = IFSEG + IMSEG(ISEG,IC)
            LGDEB = LGDEB + LGSEG
            LGSEG = SQRT((X2DV(ISEG+1,IC)-X2DV(ISEG,IC))**2
     &                  +(Y2DV(ISEG+1,IC)-Y2DV(ISEG,IC))**2)
          ENDIF
!
          TAB1(I,1) = LGDEB + FLOAT(I-IDSEG)*LGSEG/FLOAT(IFSEG-IDSEG)
!
!         COORDONNEE VERTICALE (Y)
!
          DO J = 1,JM
!
            TAB1(I,J) = TAB1(I,1)
            TAB2(I,J) = ( SHP(I,1,IC)*Z(IKLES(1,ELEM(I,IC)),J)
     &                  + SHP(I,2,IC)*Z(IKLES(2,ELEM(I,IC)),J)
     &                  + SHP(I,3,IC)*Z(IKLES(3,ELEM(I,IC)),J) )
     &                  * DISTOR(IC)
!
          ENDDO
        ENDDO !I
!
!       ENREGISTREMENT DES AUTRES PARAMETRES DE L'ENTETE
!       ------------------------------------------------
!
!       BUILD OF IKLE ARRAY. EACH QUADRANGLE IS DIVIDED INTO 2 TRIANGLES
!       NUMELEM VARIABLE MANAGES THE NUMBER OF ELEMENT
        NUMELEM = 1
        DO J = 1,JM-1
          DO I = 1,IM-1
            IKLE(NUMELEM,1) = ((J-1)*IM)+I
            IKLE(NUMELEM,2) = ((J-1)*IM)+I+1
            IKLE(NUMELEM,3) = ((J)*IM)+I+1
            NUMELEM = NUMELEM+1
            IKLE(NUMELEM,1) = ((J-1)*IM)+I
            IKLE(NUMELEM,2) = ((J)*IM)+I+1
            IKLE(NUMELEM,3) = ((J)*IM)+I
            NUMELEM = NUMELEM+1
          ENDDO
        ENDDO
        NUMELEM = NUMELEM-1
!       RECORD NELEM,NPOIN,NDP,1
        IB(1) = ((IM-1)*(JM-1)) * 2
        IB(2) = IM*JM
        IB(3) = 3
        IB(4) = 1
!       IKLE STORAGE
!       IPOBO ARRAY (WITH DUMMY VALUE)
        DO I=1,IB(2)
          IPOBO(I) = 0
        ENDDO
!       X AND Y COORDINATES
        DATE = (/0,0,0/)
        TIME = (/0,0,0/)
        CANAL = NCOU(IC)
        CALL SET_MESH(FFORMAT,CANAL,2,TRIANGLE_ELT_TYPE,3,0,0,NUMELEM,
     &                IB(2),IKLE,IPOBO,IPOBO,TAB1,TAB2,0,
     &                DATE,TIME,0,0,IERR)
        CALL CHECK_CALL(IERR,'COUPEV:SET_MESH')
!
!-----------------------------------------------------------------------
!
!    SORTIE DES VARIABLES
!
!    3 COMPOSANTES DE LA VITESSE
!    ---------------------------
!
!
        ISEG = 1
        IFSEG = 1 + IMSEG(1,IC)
        ALFA = ATAN2(Y2DV(2,IC)-Y2DV(1,IC),X2DV(2,IC)-X2DV(1,IC))
        FLAG = .TRUE.
!
        DO I = 1,IM
!
          IF (FLAG) COST = COS(ALFA)
          IF (FLAG) SINT = SIN(ALFA)
          FLAG = .FALSE.
!
          IF (I.EQ.IFSEG.AND.I.NE.IM) THEN
            FLAG = .TRUE.
            ISEG = ISEG + 1
            IFSEG = IFSEG + IMSEG(ISEG,IC)
            A1 = ALFA
            ALFA = ATAN2(Y2DV(ISEG+1,IC)-Y2DV(ISEG,IC),
     &                   X2DV(ISEG+1,IC)-X2DV(ISEG,IC))
            COST = COS(0.5D0*(ALFA+A1))
            SINT = SIN(0.5D0*(ALFA+A1))
          ENDIF
!
          N1 = IKLES(1,ELEM(I,IC))
          N2 = IKLES(2,ELEM(I,IC))
          N3 = IKLES(3,ELEM(I,IC))
          A1 = SHP(I,1,IC)
          A2 = SHP(I,2,IC)
          A3 = SHP(I,3,IC)
!
          DO J = 1,JM
!
            U1 = A1*U(N1,J) + A2*U(N2,J) + A3*U(N3,J)
            V1 = A1*V(N1,J) + A2*V(N2,J) + A3*V(N3,J)
!
!           COMPOSANTE TANGENTIELLE ET HORIZONTALE DE LA VITESSE (UT)
!
            TAB1(I,J) = COST*U1 + SINT*V1
!
!           COMPOSANTE VERTICALE DE LA VITESSE (W)
!
            TAB2(I,J) = (A1*W(N1,J)+A2*W(N2,J)+A3*W(N3,J))*DISTOR(IC)
!
!           COMPOSANTE NORMALE ET HORIZONTALE DE LA VITESSE (UN)
!
            TAB3(I,J) = -SINT*U1 + COST*V1
!
          ENDDO
        ENDDO !I
!
        IF (LNG.EQ.LNG_FR) VAR_NAME =
     &                        'VITESSE UT      M/S             '
        IF (LNG.EQ.LNG_EN) VAR_NAME =
     &                        'VELOCITY UT     M/S             '
        CALL ADD_DATA(FFORMAT,CANAL,VAR_NAME,AT,IREC,.TRUE.,TAB1,
     &                IM*JM,IERR)
        CALL CHECK_CALL(IERR,'COUPEV:ADD_DATA:UT')
        IF (LNG.EQ.LNG_FR) VAR_NAME =
     &                        'VITESSE W       M/S             '
        IF (LNG.EQ.LNG_EN) VAR_NAME =
     &                        'VELOCITY W      M/S             '
        CALL ADD_DATA(FFORMAT,CANAL,VAR_NAME,AT,IREC,.FALSE.,TAB2,
     &                IM*JM,IERR)
        CALL CHECK_CALL(IERR,'COUPEV:ADD_DATA:W')
        IF (LNG.EQ.LNG_FR) VAR_NAME =
     &                        'VITESSE UN      M/S             '
        IF (LNG.EQ.LNG_EN) VAR_NAME =
     &                        'VELOCITY UN     M/S             '
        CALL ADD_DATA(FFORMAT,CANAL,VAR_NAME,AT,IREC,.FALSE.,TAB3,
     &                IM*JM,IERR)
        CALL CHECK_CALL(IERR,'COUPEV:ADD_DATA:UN')
!
!       Adding z
!
        DO J = 1,JM
          DO I = 1,IM
            TAB1(I,J) = SHP(I,1,IC)
     &      *PREZ(IKLES(1,ELEM(I,IC))+(J-1)*NPOIN2)
     &                + SHP(I,2,IC)
     &      *PREZ(IKLES(2,ELEM(I,IC))+(J-1)*NPOIN2)
     &                + SHP(I,3,IC)
     &      *PREZ(IKLES(3,ELEM(I,IC))+(J-1)*NPOIN2)
          ENDDO !I
        ENDDO !J
        VAR_NAME = TEXTLU(1)
        CALL ADD_DATA(FFORMAT,CANAL,VAR_NAME,AT,IREC,.FALSE.,TAB1,
     &                IM*JM,IERR)
        CALL CHECK_CALL(IERR,'COUPEV:ADD_DATA:Z')
!
!       other variables
!
        IF (NVA3.GT.4) THEN
          DO K = 5,NVA3
            DO J = 1,JM
              DO I = 1,IM
                TAB1(I,J) = SHP(I,1,IC)
     &          *TAB%ADR(K-4)%P%R(IKLES(1,ELEM(I,IC))+(J-1)*NPOIN2)
     &                    + SHP(I,2,IC)
     &          *TAB%ADR(K-4)%P%R(IKLES(2,ELEM(I,IC))+(J-1)*NPOIN2)
     &                    + SHP(I,3,IC)
     &          *TAB%ADR(K-4)%P%R(IKLES(3,ELEM(I,IC))+(J-1)*NPOIN2)
              ENDDO !I
            ENDDO !J
            VAR_NAME = TEXTLU(K)
            CALL ADD_DATA(FFORMAT,CANAL,VAR_NAME,AT,IREC,.FALSE.,TAB1,
     &                    IM*JM,IERR)
            CALL CHECK_CALL(IERR,'COUPEV:ADD_DATA:K')
          ENDDO !K
!
        ENDIF
        CALL CLOSE_MESH(FFORMAT,CANAL,IERR)
        CALL CHECK_CALL(IERR,'COUPEV:CLOSE_MESH')
!
      ENDDO !IC
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
