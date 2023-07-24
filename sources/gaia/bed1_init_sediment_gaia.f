!                   **********************************
                    SUBROUTINE BED1_INIT_SEDIMENT_GAIA
!                   **********************************
!
     &(NSICLA,ELAY,ZF,ZR,NPOIN,XMVS0,ES,NOMBLAY,
     & DEBU,VOLU2D,NUMSTRAT,MAXVAR)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Initialise bed sediment layout
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     NSICLA    Number of sediment classes
!>@param[in,out] ELAY      Thickness of surface layer
!>@param[in]     ZF        Elevation of bottom
!>@param[out]    ZR        Non erodable bed
!>@param[in]     NPOIN     Number of points
!>@param[in]     XMVS0     Sediment density
!>@param[in,out] ES        Thicknesses as double precision
!>@param[in]     NOMBLAY   Number of bed layers
!>@param[in]     DEBU      Flag, restart on sedimentological file
!>@param[in]     VOLU2D    Integral of test functions (not assembled in
!!                         parallel)
!>@param[in]     NUMSTRAT  Number of initial physical layers
!>@param[in]     MAXVAR    Maximum number of output variables
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_GAIA, EX_BED1_INIT_SEDIMENT =>
     &    BED1_INIT_SEDIMENT_GAIA
      USE DECLARATIONS_GAIA, ONLY: MASS_MUD,MASS_SAND,
     & MASS_MUD_TOT,MASS_SAND_TOT,
     & RATIO_SAND,RATIO_MUD,RATIO_MUD_SAND,NSAND,NMUD,
     & MASSTOT,NUM_IMUD_ICLA,NUM_ISAND_ICLA,
     & ELAY0,DEBU_MASS,MASS0TOT,AVA0,XKV0,
     & CONC_MUD0,CONC_MUD,TOCE_MUD0,TOCE_MUD,
     & TRANS_MASS0,TRANS_MASS,PARTHENIADES0,PARTHENIADES,HIRANO,
     & FLUX_NEG_MUD_ACTIV_LAYER,FLUX_POS_MUD_ACTIV_LAYER,SED_THICK,
     & FLUX_NEG_SAND_ACTIV_LAYER,FLUX_POS_SAND_ACTIV_LAYER,BED_MODEL,
     & CONC_MUD_FOUND,TOCE_MUD_FOUND,PARTHENIADES_FOUND,MTRANS_FOUND

      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY: P_SUM
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN)     :: NSICLA,NPOIN,NOMBLAY,MAXVAR
      INTEGER,           INTENT(IN)     :: NUMSTRAT
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ELAY,ZF,ZR
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: VOLU2D
      DOUBLE PRECISION,  INTENT(IN)     :: XMVS0(NSICLA)
      LOGICAL,           INTENT(IN)     :: DEBU
      DOUBLE PRECISION, INTENT(INOUT)   :: ES(NPOIN,NOMBLAY)
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: K,ICLA,IPOIN,IERR
      INTEGER            :: ILAYER,IMUD,ISAND,ISTRAT
      DOUBLE PRECISION   :: CHECK_RS,CHECK_RM
      DOUBLE PRECISION   :: TERM,DISCR,TOT1,TOT2
      DOUBLE PRECISION   :: MASS_TOT,TOTSAND,TOTMUD
      DOUBLE PRECISION   :: XMVS_LAY
      DOUBLE PRECISION, ALLOCATABLE :: ESTRATUM(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: RATIO_INIT(:,:,:)
!
!======================================================================!
!

      IF(DEBU) THEN                     ! This is checked ONLY
        IF(NSICLA.GT.1) HIRANO=.TRUE.   ! for pure a bedload case
      ENDIF
!
      IF(.NOT.DEBU) THEN
!       CASE NOT COMPUTATION CONTINUED
!
!       ALLOCATE ESTRATUM AND RATIO_INIT
        ALLOCATE(ESTRATUM(NUMSTRAT,NPOIN), STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'ESTRATUM')
        ALLOCATE(RATIO_INIT(NSICLA,NUMSTRAT,NPOIN), STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'RATIO_SAND')
!
!       BED COMPOSITION:
!
!       DEFAULT CASE : NO STRATIFICATION = ONLY ONE STRATUM 100 M DEEP
!
!       USER CAN CHANGE THE THICKNESS OF SEDIMENT HERE
!       (REPLACES SUBROUTINE NOEROD)
!       GRADED SEDIMENT: USER CAN DEFINE AN INITIAL STRATIFICATION
!       DEFINED BY LAYER THICKNESS AND COMPOSITION FOR EACH STRATUM
!
!       POROSITY IS DEFINED (KEYWORD) FOR STRATUMS.
!       THE VALUE FOR THE FIRST STRATUM IS COPIED IN
!       THE FIRST TWO NUMERICAL LAYERS (ACTIVE LAYER + FIRST STRATUM)
        DO IPOIN=1,NPOIN
          DO ISTRAT=1,NUMSTRAT
!           DEFAULT CASE: ALL STRATUMS HAVE SAME THICKNESS.
!           THIS CAN BE CHANGED BY USER IN USER_BED_INIT
            ESTRATUM(ISTRAT,IPOIN) = SED_THICK(ISTRAT)
!           DEFAULT CASE: ALL STRATUMS HAVE SAME COMPOSITION.
!           THIS CAN BE CHANGED BY USER IN USER_BED_INIT
            DO ICLA=1,NSICLA
              RATIO_INIT(ICLA,ISTRAT,IPOIN) = AVA0(ICLA)
            ENDDO
          ENDDO
        ENDDO
!
!       USER CAN DO ADVANCED SETUP HERE
        CALL USER_BED_INIT(NUMSTRAT,NPOIN,NSICLA,ESTRATUM,RATIO_INIT)
!
!       INITIALISATION OF NUMERICAL THICKNESS (ES):
        DO IPOIN=1,NPOIN
          DO ILAYER=1,NOMBLAY
            ES(IPOIN,ILAYER)=0.D0
          ENDDO
        ENDDO
!
!       INITIALISATION OF FLUX FOR ACTIVE LAYER IN CASE OF CONSOLIDATION
!       IF NOT HIRANO, FLUX HAVE TO STAY AT 0 BECAUSE THERE AS USE IN THE GENERAL CASE.
        DO IPOIN = 1,NPOIN
          DO IMUD = 1,NMUD
            FLUX_NEG_MUD_ACTIV_LAYER(IMUD,IPOIN)= 0.D0
            DO ILAYER=1,NOMBLAY
              FLUX_POS_MUD_ACTIV_LAYER(IMUD,ILAYER,IPOIN)=0.D0
            ENDDO
          ENDDO
        ENDDO
        DO IPOIN = 1,NPOIN
          DO ISAND = 1,NSAND
            FLUX_NEG_SAND_ACTIV_LAYER(ISAND,IPOIN)=0.D0
              DO ILAYER=1,NOMBLAY
                FLUX_POS_SAND_ACTIV_LAYER(ISAND,ILAYER,IPOIN)=0.D0
              ENDDO
          ENDDO
        ENDDO
!
        IF(NSICLA.EQ.1) THEN
!
          HIRANO=.FALSE.
!
          IF(NOMBLAY.GT.1.AND.NMUD.EQ.0) THEN
              WRITE(LU,*) 'ERROR: YOU HAVE ONLY 1 NON-COHESIVE CLASS '
              WRITE(LU,*) 'AND SEVERAL LAYERS. THIS IS NOT ALLOWED!'
              CALL PLANTE(1)
              STOP
          ENDIF
!
!
          DO IPOIN = 1,NPOIN
            DO ILAYER=1,NOMBLAY
              ES(IPOIN,ILAYER) = ESTRATUM(ILAYER,IPOIN)
              CONC_MUD(ILAYER,IPOIN) = CONC_MUD0(ILAYER)
              TRANS_MASS(ILAYER,IPOIN) = TRANS_MASS0(ILAYER)
              TOCE_MUD(ILAYER,IPOIN) = TOCE_MUD0(ILAYER)
              PARTHENIADES(ILAYER,IPOIN)=PARTHENIADES0(ILAYER)
              ENDDO
          ENDDO
!
        ELSE
!
!         MULTICLASS CASE
!
          HIRANO=.TRUE.
!
          DO IPOIN=1,NPOIN
            ELAY%R(IPOIN) = ELAY0 
!           ELAY WILL BE CALCULATED IN BED1_UPDATE_ACTIVELAYER_HIRANO (AGAIN)
!
!           THE ACTIVE LAYER CORRESPONDS TO THE FIRST LAYER
!           ITS THICKNESS IS AUTOMATICALLY COMPUTED IN THE FIRST CALL TO
!           BED_UPDATE_HIRANO AND VALUE FOR MUD ARE UPDATE FOR THIS LAYER
!           BUT TRANS_MAS MUST BE EQUAL TO 0 FOR THIS LAYER
!
            ES(IPOIN,1) = 0.D0 !!ES OF LAYER 1 COMPUTED AFTER IN HIRANO
            CONC_MUD(1,IPOIN) = CONC_MUD0(1) !!CONC_MUD OF LAYER 1 IS COMPUTED AFTER IN HIRANO
            TRANS_MASS(1,IPOIN) = TRANS_MASS0(1)!!TRANS_MASS OF LAYER 1 IS COMPUTED AFTER IN HIRANO
            TOCE_MUD(1,IPOIN)= TOCE_MUD0(1) !!TOCE_MUD OF LAYER 1 COMPUTED AFTER IN HIRANO
            PARTHENIADES(1,IPOIN)=PARTHENIADES0(1)!!TO CHANGE IF NECESSARY

            DO ILAYER = 2,NOMBLAY
              ISTRAT=ILAYER-1
              ES(IPOIN,ILAYER) = ESTRATUM(ISTRAT,IPOIN)
              CONC_MUD(ILAYER,IPOIN) = CONC_MUD0(ISTRAT)
              TRANS_MASS(ILAYER,IPOIN) = TRANS_MASS0(ISTRAT)
              TOCE_MUD(ILAYER,IPOIN) = TOCE_MUD0(ISTRAT)
              PARTHENIADES(ILAYER,IPOIN)=PARTHENIADES0(ISTRAT)
            ENDDO
          ENDDO
!
        ENDIF
!       CHECK PARAMETER VALUES FOR CONSOLIDATION
        IF(BED_MODEL.EQ.2)THEN
          DO IPOIN=1,NPOIN
            DO ILAYER = 2,NOMBLAY
              IF(CONC_MUD(ILAYER,IPOIN).LT.
     &        CONC_MUD(ILAYER-1,IPOIN)) THEN
                WRITE(LU,*)'MUD CONCENTRATION OF ILAYER MUST BE',
     &          ' < TO MUD CONCNTRATION OF ILAYER +1 '
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDDO
            IF(TRANS_MASS(NOMBLAY,IPOIN).LT.0.D0.OR.
     &        TRANS_MASS(NOMBLAY,IPOIN).GT.1.D-8) THEN
              WRITE(LU,*)'MASS TRANSFERT FOR LAST LAYER ',
     &            'OF CONSOLIDATIONMUST BE EQUAL TO 0'
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDDO
        ENDIF
!
!       HERE THE NON ERODABLE BED IS FIXED
!
        DO IPOIN=1,NPOIN
          ZR%R(IPOIN)=ZF%R(IPOIN)
          DO ILAYER=1,NOMBLAY
            ZR%R(IPOIN)=ZR%R(IPOIN)-ES(IPOIN,ILAYER)
          ENDDO
        ENDDO
!
!       RATIO_SAND, RATIO_MUD AND RATIO_MUD_SAND ARE COMPUTED FROM RATIO_INIT(NCLA,NUMSTRAT,NPOIN)
!
!       1: RATIO_SAND
!
        RATIO_SAND = 0.D0
        IF(NSAND.GT.0) THEN
          DO IPOIN=1,NPOIN
            DO ISTRAT=1,NUMSTRAT
              TOT1=0.D0
              TOT2=0.D0
              DO ISAND=1,NSAND
                TOT1=TOT1+RATIO_INIT(NUM_ISAND_ICLA(ISAND),ISTRAT,IPOIN)
              ENDDO
              DO ISAND=1,NSAND
                IF(NSICLA.GT.1)THEN
                  ILAYER = ISTRAT+1
!                 ACTIVE LAYER IS FILLED IN BED_UPDATE_HIRANO BUT ITS
!                 RATIO IS INIITIALISED HERE
                  IF(ISAND.LT.NSAND)THEN
                    RATIO_SAND(ISAND,1,IPOIN)=0.D0
                  ELSE
                    RATIO_SAND(NSAND,1,IPOIN)=1.D0
                  ENDIF
                ELSE
                  ILAYER=ISTRAT
                ENDIF
                IF(ISAND.LT.NSAND)THEN
                  IF(TOT1.GT.0.D0)THEN
                    RATIO_SAND(ISAND,ILAYER,IPOIN)=
     &              RATIO_INIT(NUM_ISAND_ICLA(ISAND),ISTRAT,IPOIN)/TOT1
                  ELSE
                    RATIO_SAND(NSAND,ILAYER,IPOIN)=0.D0
                  ENDIF
                  TOT2=TOT2+RATIO_SAND(ISAND,ILAYER,IPOIN)
                ELSE
                  RATIO_SAND(NSAND,ILAYER,IPOIN)=1.D0-TOT2
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDIF
!
!       2: RATIO_MUD
!
        RATIO_MUD = 0.D0
        IF(NMUD.GT.0) THEN
          DO IPOIN=1,NPOIN
            DO ISTRAT=1,NUMSTRAT
              TOT1=0.D0
              TOT2=0.D0
              DO IMUD=1,NMUD
              TOT1= TOT1 + RATIO_INIT(NUM_IMUD_ICLA(IMUD),ISTRAT,IPOIN)
              ENDDO
              DO IMUD=1,NMUD
                IF(NSICLA.GT.1)THEN
                  ILAYER = ISTRAT+1
!                 ACTIVE LAYER IS FILLED IN BED_UPDATE_HIRANO BUT ITS
!                 RATIO IS INIITIALISED HERE
                  IF(IMUD.LT.NMUD)THEN
                    RATIO_MUD(IMUD,1,IPOIN)=0.D0
                  ELSE
                    RATIO_MUD(NMUD,1,IPOIN)=1.D0
                  ENDIF
                ELSE
                  ILAYER=ISTRAT
                ENDIF
                IF(IMUD.LT.NMUD)THEN
                  IF(TOT1.GT.0.D0)THEN
                      RATIO_MUD(IMUD,ILAYER,IPOIN)=
     &            RATIO_INIT(NUM_IMUD_ICLA(IMUD),ISTRAT,IPOIN)/TOT1
                  ELSE
                      RATIO_MUD(NMUD,ILAYER,IPOIN)=0.D0
                  ENDIF
                  TOT2=TOT2+RATIO_MUD(IMUD,ILAYER,IPOIN)
                ELSE
                  RATIO_MUD(NMUD,ILAYER,IPOIN)=1.D0-TOT2
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDIF
!
!       3: RATIO_MUD_SAND
!
        RATIO_MUD_SAND = 0.D0
        DO IPOIN=1,NPOIN
          DO ISTRAT=1,NUMSTRAT
            TOTMUD=0.D0
            TOTSAND=0.D0
            DO IMUD=1,NMUD
              TOTMUD= TOTMUD +
     &        RATIO_INIT(NUM_IMUD_ICLA(IMUD),ISTRAT,IPOIN)
            ENDDO
            DO ISAND=1,NSAND
              TOTSAND= TOTSAND +
     &        RATIO_INIT(NUM_ISAND_ICLA(ISAND),ISTRAT,IPOIN)
            ENDDO
            IF(NSICLA.GT.1)THEN
              ILAYER = ISTRAT+1
!           ACTIVE LAYER IS FILLED IN THE FIRST CALL TO BED_UPDATE_HIRANO
            ELSE
              ILAYER=ISTRAT
            ENDIF
            IF(TOTMUD+TOTSAND.GT.0.D0)THEN
              RATIO_MUD_SAND(ILAYER,IPOIN)=TOTMUD/(TOTMUD+TOTSAND)
            ELSE
              RATIO_MUD_SAND(ILAYER,IPOIN)=1.D0
            ENDIF
          ENDDO
        ENDDO
!
      ELSE
!
!       DEBU.EQ.TRUE
!       IN THIS PART WE COMPLETE MISSING DATA FROM PREV SED FILE
!       USING DATA READ IN THE STEERING FILE
        IF(NSICLA.EQ.1) THEN
          DO IPOIN=1,NPOIN
            DO ILAYER=1,NOMBLAY
              DO ISAND=1,NSAND
                RATIO_SAND(ISAND,ILAYER,IPOIN) =
     &          AVA0(NUM_ISAND_ICLA(ISAND))
              ENDDO
              DO IMUD=1,NMUD
                RATIO_MUD(IMUD,ILAYER,IPOIN) =
     &          AVA0(NUM_IMUD_ICLA(IMUD))
              ENDDO
            ENDDO
          ENDDO
!         INITIALISE THE MUD ARRAYS IF THEY WERE NOT FOUND IN THE
!         PREVIOUS FILE
          DO IPOIN = 1,NPOIN
            DO ILAYER=1,NOMBLAY
              IF (.NOT.CONC_MUD_FOUND) THEN
                CONC_MUD(ILAYER,IPOIN) = CONC_MUD0(ILAYER)
                IF(IPOIN==1.AND.ILAYER==1)
     &          WRITE(LU,*) 'WARNING: MUD CONCENTRATION NOT FOUND IN ',
     &                      'THE PREVIOUS COMPUTATION FILE, IT IS SET ',
     &                      'FROM THE STEERING FILE'
              ENDIF
              IF (.NOT.MTRANS_FOUND) THEN
                TRANS_MASS(ILAYER,IPOIN) = TRANS_MASS0(ILAYER)
                IF(IPOIN==1.AND.ILAYER==1)
     &           WRITE(LU,*) 'WARNING: MASS TRANSFER NOT FOUND IN ',
     &                      'THE PREVIOUS COMPUTATION FILE, IT IS SET ',
     &                      'FROM THE STEERING FILE'
              ENDIF
              IF (.NOT.TOCE_MUD_FOUND) THEN
                TOCE_MUD(ILAYER,IPOIN) = TOCE_MUD0(ILAYER)
                IF(IPOIN==1.AND.ILAYER==1)
     &           WRITE(LU,*) 'WARNING: MUD TOCE NOT FOUND IN ',
     &                      'THE PREVIOUS COMPUTATION FILE, IT IS SET ',
     &                      'FROM THE STEERING FILE'
              ENDIF
              IF (.NOT.PARTHENIADES_FOUND) THEN
                PARTHENIADES(ILAYER,IPOIN) = PARTHENIADES0(ILAYER)
                IF(IPOIN==1.AND.ILAYER==1)
     &           WRITE(LU,*) 'WARNING: PARTHENIADES NOT FOUND IN ',
     &                      'THE PREVIOUS COMPUTATION FILE, IT IS SET ',
     &                      'FROM THE STEERING FILE'
              ENDIF
            ENDDO
          ENDDO
!
        ELSE
!
          DO IPOIN=1,NPOIN
!           THE ACTIVE LAYER THICKNESS IS ALWAYS READ FROM THE
!           STEERING FILE, AND NOT THE PREVIOUS COMPUTATION RESULTS
!           IT WILL BE CALCULATED IN BED1_UPDATE_ACTIVELAYER_HIRANO
            ELAY%R(IPOIN)= ELAY0
            IF (.NOT.CONC_MUD_FOUND) THEN
              CONC_MUD(1,IPOIN) = CONC_MUD0(1)
              IF(IPOIN==1)
     &          WRITE(LU,*) 'WARNING: MUD CONCENTRATION NOT FOUND IN ',
     &                    'THE PREVIOUS COMPUTATION FILE, IT IS SET ',
     &                    'FROM THE STEERING FILE'
            ENDIF
            IF (.NOT.MTRANS_FOUND) THEN
              TRANS_MASS(1,IPOIN) = TRANS_MASS0(1)
              IF(IPOIN==1)
     &         WRITE(LU,*) 'WARNING: MASS TRANSFER NOT FOUND IN ',
     &                    'THE PREVIOUS COMPUTATION FILE, IT IS SET ',
     &                    'FROM THE STEERING FILE'
            ENDIF
            IF (.NOT.TOCE_MUD_FOUND) THEN
              TOCE_MUD(1,IPOIN) = TOCE_MUD0(1)
              IF(IPOIN==1)
     &         WRITE(LU,*) 'WARNING: MUD TOCE NOT FOUND IN ',
     &                    'THE PREVIOUS COMPUTATION FILE, IT IS SET ',
     &                    'FROM THE STEERING FILE'
            ENDIF
            IF (.NOT.PARTHENIADES_FOUND) THEN
              PARTHENIADES(1,IPOIN) = PARTHENIADES0(1)
              IF(IPOIN==1)
     &         WRITE(LU,*) 'WARNING: PARTHENIADES NOT FOUND IN ',
     &                    'THE PREVIOUS COMPUTATION FILE, IT IS SET ',
     &                    'FROM THE STEERING FILE'
            ENDIF
            DO ILAYER = 2,NOMBLAY
              ISTRAT=ILAYER-1
              IF (.NOT.CONC_MUD_FOUND) THEN
                CONC_MUD(ILAYER,IPOIN) = CONC_MUD0(ISTRAT)
              ENDIF
              IF (.NOT.MTRANS_FOUND) THEN
                TRANS_MASS(ILAYER,IPOIN) = TRANS_MASS0(ISTRAT)
              ENDIF
              IF (.NOT.TOCE_MUD_FOUND) THEN
                TOCE_MUD(ILAYER,IPOIN) = TOCE_MUD0(ISTRAT)
              ENDIF
              IF (.NOT.PARTHENIADES_FOUND) THEN
                PARTHENIADES(ILAYER,IPOIN)=PARTHENIADES0(ISTRAT)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
        IF(.NOT.DEBU_MASS) THEN
!       COMPUTE RATIO_MUD_SAND FROM RATIO_SAND AND RATIO_MUD ISSUES FROM SEDIMENTOLOGICAL FILE
          DO IPOIN=1,NPOIN
            DO ILAYER=1,NOMBLAY
              TOTMUD=0.D0
              TOTSAND=0.D0
              DO IMUD=1,NMUD
                TOTMUD= TOTMUD + RATIO_MUD(IMUD,ILAYER,IPOIN)
              ENDDO
              DO ISAND=1,NSAND
                TOTSAND= TOTSAND + RATIO_SAND(ISAND,ILAYER,IPOIN)
              ENDDO
              IF(TOTMUD+TOTSAND.GT.0.D0)THEN
                RATIO_MUD_SAND(ILAYER,IPOIN)=TOTMUD/(TOTMUD+TOTSAND)
              ELSE
                RATIO_MUD_SAND(ILAYER,IPOIN)=1.D0
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
      IF(.NOT.DEBU.OR..NOT.DEBU_MASS) THEN
!
!       CHECK THE RATIOS
!
        DO IPOIN=1,NPOIN
          DO ILAYER=1,NOMBLAY
            CHECK_RS=0.D0
            CHECK_RM=0.D0
            IF(NSAND.GT.0) THEN
              DO ISAND=1,NSAND
                CHECK_RS=CHECK_RS+RATIO_SAND(ISAND,ILAYER,IPOIN)
              ENDDO
              IF(ABS(CHECK_RS-1.D0).GE.1.D-7) THEN
                WRITE(LU,*)'SUM OF SAND RATE COEFF MUST BE EQUAL TO 1!'
                WRITE(LU,*)'VERIFY YOUR MASS RATE OF SAND'
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDIF
            IF(NMUD.GT.0) THEN
              DO IMUD=1,NMUD
                CHECK_RM=CHECK_RM+RATIO_MUD(IMUD,ILAYER,IPOIN)
              ENDDO
              IF(ABS(CHECK_RM-1.D0).GE.1.D-7) THEN
                WRITE(LU,*)'SUM OF MUD RATE COEFF MUST BE EQUAL TO 1!'
                WRITE(LU,*)'VERIFY YOUR MASS RATE OF MUD'
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDIF
          ENDDO
        ENDDO
!
!       MASS COMPUTATION : MASS_MUD;MASS_SAND;MASS_MUD_TOT;MASS_SAND_TOT
!       ALL MASSES IN [kg/m2]
!
!       FOR MASS COMPUTATION IS MANDATORY RATIO_MUD_SAND,
!       XMVS,CONC_MUD,ES(ILAYER)
!
        IF(NSAND.EQ.0)THEN
          DO IPOIN = 1,NPOIN
            DO ILAYER = 1,NOMBLAY
              MASS_MUD_TOT(ILAYER,IPOIN) =
     &        ES(IPOIN,ILAYER)*CONC_MUD(ILAYER,IPOIN)
!           COMPUTES MASS FOR EVERY MUD
              DO IMUD = 1,NMUD
                MASS_MUD(IMUD,ILAYER,IPOIN)=MASS_MUD_TOT(ILAYER,IPOIN)
     &          *RATIO_MUD(IMUD,ILAYER,IPOIN)
              ENDDO
            ENDDO
          ENDDO
        ELSE
          DO IPOIN = 1,NPOIN
            DO ILAYER = 1,NOMBLAY
              XMVS_LAY=0.D0
              DO ISAND = 1,NSAND ! AVERAGE DENSITY OF SAND FOR THE LAYER
                XMVS_LAY=XMVS_LAY+RATIO_SAND(ISAND,ILAYER,IPOIN)
     &          *XMVS0(NUM_ISAND_ICLA(ISAND))
              ENDDO
              TERM=0.D0
              IF(NMUD.GT.0) THEN
                TERM =
     &            RATIO_MUD_SAND(ILAYER,IPOIN)/CONC_MUD(ILAYER,IPOIN)-
     &            (XKV0(ILAYER)*(1.D0-RATIO_MUD_SAND(ILAYER,IPOIN)))/
     &            (XMVS_LAY*(1.D0-XKV0(ILAYER)))
!             TERM REPRESENTS THE DIFFERENCE BETWEEN MUD VOLUME AND VOID
!             VOLUME
              ENDIF
!             DISCR IS POSITIVE WHEN MUD FILLS ALL THE SAND POROSITY
!             OTHERWISE IS ZERO
              DISCR=MAX(0.D0,TERM)
              MASS_TOT = ES(IPOIN,ILAYER)/
     &        ((1.D0-RATIO_MUD_SAND(ILAYER,IPOIN))/
     &        (XMVS_LAY*(1.D0-XKV0(ILAYER)))+DISCR)
!
              MASS_MUD_TOT(ILAYER,IPOIN) = RATIO_MUD_SAND(ILAYER,IPOIN)
     &        *MASS_TOT
              MASS_SAND_TOT(ILAYER,IPOIN) =
     &        (1.D0-RATIO_MUD_SAND(ILAYER,IPOIN))*MASS_TOT
!             COMPUTES MASS FOR EVERY MUD
              DO IMUD = 1,NMUD
                MASS_MUD(IMUD,ILAYER,IPOIN) = MASS_MUD_TOT(ILAYER,IPOIN)
     &          *RATIO_MUD(IMUD,ILAYER,IPOIN)
              ENDDO
!             COMPUTES MASS FOR EVERY SAND
              DO ISAND = 1,NSAND
                MASS_SAND(ISAND,ILAYER,IPOIN) =
     &            MASS_SAND_TOT(ILAYER,IPOIN)*
     &            RATIO_SAND(ISAND,ILAYER,IPOIN)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
!
!
      ENDIF !(.NOT.DEBU.OR..NOT.DEBU_MASS)
!
!     REAL MASS COMPUTATION: FROM [kg/m2] to [kg]
!     USEFUL FOR FIRST LISTING AND MASS BALANCE
!
      DO ICLA=1,NSICLA
        MASSTOT(ICLA)=0.D0
      ENDDO
!
      IF(NSAND.GT.0) THEN
        DO IPOIN=1,NPOIN
          DO ILAYER=1,NOMBLAY
            DO ISAND=1,NSAND
              K=NUM_ISAND_ICLA(ISAND)
              MASSTOT(K)=MASSTOT(K)+MASS_SAND(ISAND,ILAYER,IPOIN)
     &                   *VOLU2D%R(IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
      IF(NMUD.GT.0) THEN
        DO IPOIN=1,NPOIN
          DO ILAYER=1,NOMBLAY
            DO IMUD=1,NMUD
              K=NUM_IMUD_ICLA(IMUD)
              MASSTOT(K)=MASSTOT(K)+MASS_MUD(IMUD,ILAYER,IPOIN)
     &                   *VOLU2D%R(IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        DO ICLA=1,NSICLA
          MASSTOT(ICLA) = P_SUM(MASSTOT(ICLA))
        ENDDO
      ENDIF
!
      DO ICLA=1,NSICLA
        MASS0TOT(ICLA)=MASSTOT(ICLA)
      ENDDO
!
      WRITE(LU,*)
      WRITE(LU,100)
      DO ICLA=1,NSICLA
        WRITE(LU,111) ICLA,MASSTOT(ICLA)
      ENDDO
100   FORMAT(20X,'GAIA INITIAL MASS OF SEDIMENTS: ')
111   FORMAT(1X,'TOTAL MASS OF CLASS ',I2,' =',G16.7,'( KG )')
!
!-----------------------------------------------------------------------
      IF(.NOT.DEBU) THEN
        DEALLOCATE(ESTRATUM)
        DEALLOCATE(RATIO_INIT)
      ENDIF
!-----------------------------------------------------------------------
!
      RETURN
      END
