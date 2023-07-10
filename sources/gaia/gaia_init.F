!                   ********************
                    SUBROUTINE GAIA_INIT
!                   ********************
!
     &(GRAFCOUNT,LISTCOUNT,TELNIT,U_TEL,V_TEL,H_TEL,ZF_TEL,UETCAR,
     & DELTAR,CF_TEL,KS_TEL,CODE,U3D,V3D,T_TEL,DT_TEL,CHARR_TEL,
     & SUSP_TEL,THETAW_TEL,HW_TEL,TW_TEL,UW_TEL,YAGOUT,XMVE_TEL,
     & GRAV_TEL)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Initialization of gaia (LT=0)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     GRAFCOUNT Period of graphical outputs
!>@param[in]     LISTCOUNT Period of listing outputs
!>@param[in]     TELNIT    Number of telemac iterations
!>@param[in]     U_TEL     U velocity from telemac
!>@param[in]     V_TEL     V velocity from telemac
!>@param[in]     H_TEL     Water depth from tel h (n+1)
!>@param[in,out] ZF_TEL    Bottom elevation of the calling telemac
!>@param[in,out] UETCAR    Square of the friction velocity (coupling with tel 3d)
!>@param[in]     DELTAR    Relative density = (rho-rho0)/rho0 sent by telemac-3d
!>@param[in,out] CF_TEL    Quadratic friction coefficient from telemac
!>@param[in,out] KS_TEL    Bed roughness sent to telemac
!>@param[in]     CODE      Name of calling programme (telemac2d or 3d)
!>@param[in]     U3D       3D velocity sent by telemac 3d (u)
!>@param[in]     V3D       3D velocity sent by telemac 3d (v)
!>@param[in]     T_TEL     Current time in calling programme
!>@param[in]     DT_TEL    Time step from telemac
!>@param[in,out] CHARR_TEL Logical, bed load or not: sent to telemac-2d
!>@param[in,out] SUSP_TEL  Logical, suspended load or not: sent to telemac-2d
!>@param[in]     THETAW_TEL Mean direction of wave (from tomawac)
!>@param[in]     HW_TEL    Significant wave height (from tomawac)
!>@param[in]     TW_TEL    Peak period order 5th (from tomawac)
!>@param[in]     UW_TEL    Orbital velocity
!>@param[in]     YAGOUT    Logical: if yes graphic output (steered by t2d)
!>@param[in]     XMVE_TEL  Water density from telemac
!>@param[in]     GRAV_TEL  Gravity acceleration from telemac
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC, ONLY :TRIGO
      USE DECLARATIONS_GAIA
      USE INTERFACE_GAIA, EX_GAIA_INIT => GAIA_INIT
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_SUM
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN)    :: GRAFCOUNT,LISTCOUNT
      INTEGER,           INTENT(IN)    :: TELNIT
      CHARACTER(LEN=24),  INTENT(IN)   :: CODE
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U_TEL,V_TEL,H_TEL
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: ZF_TEL,UETCAR,KS_TEL
      TYPE(BIEF_OBJ),    INTENT(IN)    :: DELTAR
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: CF_TEL
      DOUBLE PRECISION,  INTENT(IN)    :: T_TEL,XMVE_TEL,GRAV_TEL
      LOGICAL,           INTENT(INOUT) :: CHARR_TEL,SUSP_TEL
      DOUBLE PRECISION,  INTENT(IN)    :: DT_TEL
      TYPE(BIEF_OBJ),    INTENT(IN)    :: THETAW_TEL,HW_TEL,TW_TEL
      TYPE(BIEF_OBJ),    INTENT(IN)    :: UW_TEL
      LOGICAL,           INTENT(IN)    :: YAGOUT
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER                       :: I,K,IELEB,KP1,IVAR
      INTEGER                       :: NUMENX,IERR
      INTEGER                       :: TROUVE(MAXVAR+10)
      INTEGER                       :: ISAND,IPOIN,IMUD,ICLA
      DOUBLE PRECISION              :: AT !only used for wave
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER, ALLOCATABLE          :: NULLT(:)
      DOUBLE PRECISION, ALLOCATABLE :: NULLD(:)
!======================================================================!
!
#if defined COMPAD
      CALL AD_GAIA_BEGIN
      CALL AD_GAIA_INITIALISATION_BEGIN
#endif
      ! Set to 1 for Telemac3d
      HIND_TYPE = 1
!     INITIALISES THE CONSTANTS
!
      CALL INIT_CONSTANT_GAIA(KARIM_HOLLY_YANG,KARMAN,PI)
!
!     READS THE WAVE DATA IN THE HYDRODYNAMIC FILE
!
      IF(HOULE.AND.GAI_FILES(GAICOU)%NAME(1:1).EQ.' ') THEN
        ALIRE(12)=1
        ALIRE(13)=1
        ALIRE(14)=1
        ALIRE(22)=1
      ENDIF
!
!     READS THE SEDIMENTOLOGICAL DATA IN THE CONTINUATION FILE
!     IN CASE OF COMPUTATION CONTINUED
!
      IF(DEBU) THEN
        ALIRE(5)=1 ! bottom
        ALIRE(9)=1 ! rigid bed
        ALIRE(15)=1
        ALIRE(16)=1
        ALIRE(17)=1
        ALIRE(18)=1
        ALIRE(19)=1
        ALIRE(20)=1
        ALIRE(21)=1
!       READS REFERENCE LEVEL FOR NESTOR
        IF(NESTOR) ALIRE(23)=1           ! Nestor
!       READS RATIO_SAND FROM THE PREVIOUS COMPUTATION FILE
        DO I=1,NSAND*NOMBLAY
          ALIRE(NVAR_RATIOS+I)=1
        ENDDO
!       READS RATIO_MUD FROM THE PREVIOUS COMPUTATION FILE
        DO I=1,NMUD*NOMBLAY
          ALIRE(NVAR_RATIOM+I)=1
        ENDDO
!       READS THE LAYER THICKNESSES
        DO I=1,NOMBLAY
          ALIRE(NVAR_LAYTHI+I)=1
        ENDDO
!       READS MASS_S FROM THE PREVIOUS COMPUTATION FILE
        DO I=1,NSAND*NOMBLAY
          ALIRE(NVAR_MASS_S+I)=1
        ENDDO
!       READS MASS_M FROM THE PREVIOUS COMPUTATION FILE
        DO I=1,NMUD*NOMBLAY
          ALIRE(NVAR_MASS_M+I)=1
        ENDDO
!       READS LAYER CONCENTRATION, CRITICAL SHEAR STRESS,
!       PARTHENIADES CONSTANT EROSION RATE, MASS TRANSFER RATE FROM
!       THE PREVIOUS COMPUTATION FILE
        DO I=1,NOMBLAY
          ALIRE(NVAR_LAYCONC+I)=1
          ALIRE(NVAR_TOCEMUD+I)=1
          ALIRE(NVAR_PARTHE+I)=1
          ALIRE(NVAR_MTRANS+I)=1
        ENDDO
!       DIFFERENTIATED VARIABLES
!       FOR READING GRADIENTS IN SELAFIN FILES
!
        IF(NADVAR.GT.0) THEN
          DO IVAR=1,NADVAR
!           SEE NOMVAR_GAIA
            ALIRE(NVAR_ADVAR+IVAR) = 1
          ENDDO
        ENDIF
!
      ENDIF
!
!     INITIALISES (SETS TO 0) THE ARRAYS
!
      CALL INIT_ZERO_GAIA

!       -- INITIALISES THE REFERENCE LEVEL ZRL FOR NESTOR
!          IT MUST BE EXACLY 123456789.0D0
        ZRL%R(:) = 123456789.0D0
!
!
!     DISCRETISATION : LINEAR FOR THE TIME BEING
!
!     IELMT HARD-CODED IN LECDON
!
      IELMX  = MAX(IELMT,IELMH_GAI,IELMU_GAI)
      NELMAX = NELEM
!
!=======================================================================
!
!     READS, PREPARES AND CONTROLS THE DATA
!
!=======================================================================
!
!     READS THE BOUNDARY CONDITIONS AND INDICES FOR THE BOUNDARY NODES
!     EBOR IS READ HERE FOR THE FIRST CLASS ONLY
!     SEE CONLIT_GAIA FOR OTHER CLASSES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECLIM'
      ALLOCATE(NULLT(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'NULLT')
      ALLOCATE(NULLD(NPTFR*2),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'NULLD')
      CALL LECLIM(LIHBOR%I,LIQBOR%I,NULLT,LIEBOR%I,Q2BOR%R,NULLD,
     &            NULLD,EBOR%ADR(1)%P%R,NULLD,AFBOR%R,BFBOR%R,
     &            MESH%NPTFR,'GAI',.TRUE.,
     &            GAI_FILES(GAIGEO)%FMT,GAI_FILES(GAIGEO)%LU,
     &            KENT,KENT,-1,-1,-1,-1,
     &            NUMLIQ%I,MESH,BOUNDARY_COLOUR=BOUNDARY_COLOUR%I)
      DEALLOCATE(NULLT)
      DEALLOCATE(NULLD)
      IF(DEBUG.GT.0) WRITE(LU,*) 'END_LECLIM'
!
!-----------------------------------------------------------------------
!
!     COMPLEMENTS THE DATA STRUCTURE FOR BIEF
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'INBIEF'
      CALL INBIEF(IT1%I,KLOG,IT2,IT3,IT4,LVMAC,IELMX,
     &               0.D0,SPHERI,MESH,T1,T2,OPTASS,PRODUC,EQUA)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'END_INBIEF'
!
!-----------------------------------------------------------------------
!
!     LOCATES THE BOUNDARIES
!
      IF(NCSIZE.GT.1) THEN
        NFRLIQ=0
        DO I=1,NPTFR
          NFRLIQ=MAX(NFRLIQ,NUMLIQ%I(I))
        ENDDO
        NFRLIQ=P_MAX(NFRLIQ)
        WRITE(LU,*) ' '
        WRITE(LU,*) 'LIQUID BOUNDARIES:',NFRLIQ
!       NFRLIQ CHECKED (NFRSOL NOT USED IN PARALLEL)
        IF(NFRLIQ.GT.MAXFRO) THEN
          WRITE(LU,*) 'INCREASE THE MAXIMUM NUMBER OF BOUNDARIES'
          WRITE(LU,*) 'CURRENTLY AT ',MAXFRO
          WRITE(LU,*) 'TO THE VALUE ',NFRLIQ
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FRONT2'
        CALL FRONT2(NFRLIQ,
     &              LIEBOR%I,LIEBOR%I,
     &              MESH%X%R,MESH%Y%R,MESH%NBOR%I,MESH%KP1BOR%I,
     &              IT1%I,NPOIN,NPTFR,KLOG,.TRUE.,NUMLIQ%I,MAXFRO)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FRONT2'
      ENDIF
      IF(NFRLIQ.GT.MAXFRO) THEN
        WRITE(LU,*) 'FRONT2: SIZE OF ARRAYS EXCEEDED'
        WRITE(LU,*) '        INCREASE THE KEYWORD'
        WRITE(LU,*) '        MAXIMUM NUMBER OF BOUNDARIES'
        WRITE(LU,*) '        IN THE CALLING PROGRAM'
        WRITE(LU,*) '        THE CURRENT VALUE IS ',MAXFRO
        WRITE(LU,*) '        THE VALUE SHOULD BE ',NFRLIQ
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!     LOOKS FOR RADSEC FOR SECONDARY CURRENTS IN THE GEOMETRY FILE :
!-----------------------------------------------------------------------
!
!
!     RADSEC IS LANGUAGE INDEPENDENT
!     CALL FIND_VARIABLE(RADSEC,'RADSEC          ',GAI_FILES(GAIGEO)%LU,
!    &                   GAI_FILES(GAIGEO)%FMT,W,OKRADSEC,TIME=BID)
! Initialising
      RADSEC%R=0.D0
      CALL FIND_VARIABLE(GAI_FILES(GAIGEO)%FMT,GAI_FILES(GAIGEO)%LU,
     & 'RADSEC          ', RADSEC%R,NPOIN,IERR)
!
!-----------------------------------------------------------------------
!       LOOKS FOR RFERENCE LEVEL FOR NESTOR IN THE GEOMETRY FILE :
!-----------------------------------------------------------------------
!       REFERENCE LEVEL IS LANGUAGE INDEPENDENT
!     Initialising
      CALL FIND_VARIABLE(GAI_FILES(GAIGEO)%FMT,GAI_FILES(GAIGEO)%LU,
     &  'REFERENCE LEVEL ', ZRL%R,NPOIN,IERR)
      IF( IERR == 0 ) THEN
        WRITE(6,*)'-------------------------------------------'
        WRITE(6,*)
     &  'Found REFERENCE LEVEL (ZRL) for Nestor in Sis Geometry file'
        WRITE(6,*)'          max value of ZRL = ',MAXVAL(ZRL%R)
        WRITE(6,*)'          min value of ZRL = ',MINVAL(ZRL%R)
        WRITE(6,*)'-------------------------------------------'
      ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     PREPARES THE RESULTS FILE (OPTIONAL)
!
!     STANDARD SELAFIN FORMAT
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'WRITE_HEADER'
!     CREATES DATA FILE USING A GIVEN FILE FORMAT : FORMAT_RES
!     THE DATA ARE CREATED IN THE FILE: GAIRES, AND ARE
!     CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
!     CONTAINED IN THE FILE.
      CALL WRITE_HEADER(GAI_FILES(GAIRES)%FMT, ! RESULTS FILE FORMAT
     &                  GAI_FILES(GAIRES)%LU,  ! LU FOR RESULTS FILE
     &                  TITCA,      ! TITLE
     &                  MAXVAR,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                  TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                  SORLEO)     ! PRINT TO FILE OR NOT
      IF(DEBUG.GT.0) WRITE(LU,*) 'END_WRITE_HEADER'
      IF(DEBUG.GT.0) WRITE(LU,*) 'WRITE_MESH'
!     WRITES THE MESH IN THE OUTPUT FILE :
!     IN PARALLEL, REQUIRES NCSIZE AND NPTIR.
!     THE REST OF THE INFORMATION IS IN MESH.
!     ALSO WRITES : START DATE/TIME AND COORDINATES OF THE
!     ORIGIN.
      CALL WRITE_MESH(GAI_FILES(GAIRES)%FMT, ! RESULTS FILE FORMAT
     &                GAI_FILES(GAIRES)%LU,  ! LU FOR RESULTS FILE
     &                MESH,
     &                1,             ! NUMBER OF PLANES /NA/
     &                MARDAT,        ! START DATE
     &                MARTIM,        ! START TIME
     &                T1,T2,
     &                NCSIZE.GT.1,NPTIR,
     &                NGEO=GAI_FILES(GAIGEO)%LU,
     &                GEOFORMAT=GAI_FILES(GAIGEO)%FMT)
      IF(DEBUG.GT.0) WRITE(LU,*) 'END_WRITE_MESH'
!
!     FILLS IN MASKEL BY DEFAULT
!
      IF(MSK) CALL OS ('X=C     ', X=MASKEL, C=1.D0)
!
!     BUILDING THE MASK FOR LIQUID BOUNDARIES
!     A SEGMENT IS LIQUID IF BOTH ENDS ARE NOT SOLID
!
      DO IELEB = 1, MESH%NELEB
        K=MESH%IKLBOR%I(IELEB)
        KP1=MESH%IKLBOR%I(IELEB+MESH%NELEBX)
        IF(LIEBOR%I(K).NE.2.AND.LIEBOR%I(KP1).NE.2) THEN
          MASK%R(IELEB) = 1.D0
        ELSE
          MASK%R(IELEB) = 0.D0
        ENDIF
      ENDDO
!
!=======================================================================
!
!     INITIALISES
!
!=======================================================================
!
      PASS      = .TRUE.
!
      SECOND_SUSP_STEP = .FALSE.
      MASSNESTOR = 0.D0
      IF(BILMA) THEN
        SUMMCUMUCL= 0.D0
        SUMBEDLOAD_B = 0.D0
        SUMRMASCL = 0.D0
        SUM_DEPOSITION = 0.D0
        SUM_EROSION = 0.D0
        SUMMASSNESTOR = 0.D0
      ENDIF
      XMVE      = XMVE_TEL
      GRAV      = GRAV_TEL
!
!     DETERMINES THE TOTAL NUMBER OF TIMESTEPS         : NIT
!                    INITIAL TIME                      : AT0
!                    TIMESTEP                          : DT
      AT0 = T_TEL
      DT = DT_TEL
      NIT = TELNIT
!     VALIDATES AGAINST THE LAST TIMESTEP
!PTRK -- VALNIT has to be checked if the computation is efficient! -> CHECK THIS PART!!
      VALNIT = NIT
!     VALNIT WILL BE USED FOR CALLING VALIDA
!     JMH ON 21/06/2016: IT SEEMS THAT THE FORMULA ON NEXT LINE IS OUTDATED...
!     VALNIT = (TELNIT/PERICOU)*PERICOU-PERICOU+1
      VALNIT = TELNIT
!     MODIFICATION JMH + CV: TO AVOID 2 SUCCESSIVE CALLS TO VALIDA
!     WHEN BEDLOAD AND SUSPENSION
      IF(GRAFCOUNT.GT.TELNIT) VALNIT=NIT+1
      LEOPR = GRAFCOUNT
      LISPR = LISTCOUNT
!     CHARR, SUSP AND TIME STEP MONITORED BY CALLING PROGRAM
!     AT0=T_TEL
      DT=MOFAC*DT_TEL
!
!     RESUMES GAIA COMPUTATION
!
      IF(GAI_FILES(GAIPRE)%NAME(1:1).NE.' ')  THEN
!
!       READS THE HYDRO AND SEDIMENTOLOGICAL VARIABLES
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'READ_DATASET'
        CALL READ_DATASET(GAI_FILES(GAIPRE)%FMT,GAI_FILES(GAIPRE)%LU,
     &                    VARSOR,NPOIN,NUMENX,AT0,TEXTPR,TROUVE,ALIRE,
     &                    .TRUE.,.TRUE.,MAXVAR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_READ_DATASET'
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'RESCUE_GAIA'
        CALL RESCUE_GAIA(HN%R,Z%R,ZF%R,ZR%R,ES,HW%R,TW%R,
     &       THETAW%R,NPOIN,NOMBLAY,TROUVE,ALIRE,PASS,ICF,.TRUE.,MAXVAR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE RESCUE_GAIA'
!
      ENDIF
!
!     READS THE LAST RECORD : WAVE FILE
!
!     NOTE : GAI_FILES(GAICOU)%NAME SET TO ' ' IF HOULE=NO
!
      IF (GAI_FILES(GAICOU)%NAME(1:1).NE.' ')  THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'SUITE_HOULE'
        WRITE(LU,*) ' LECTURE HOULE :',GAI_FILES(GAICOU)%NAME
        CALL READ_DATASET(GAI_FILES(GAICOU)%FMT,GAI_FILES(GAICOU)%LU,
     &                    VARSOR,NPOIN,NUMENX,AT,TEXTPR,TROUVE,ALIRH,
     &                    .TRUE.,.TRUE.,MAXVAR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_SUITE_HOULE'
!       TRACES IF WAVE DATA HAVE BEEN FOUND
        IF(TROUVE(11).EQ.1) HW%TYPR='Q'
        IF(TROUVE(12).EQ.1) TW%TYPR='Q'
        IF(TROUVE(13).EQ.1) THETAW%TYPR='Q'
!
!       THETAW in GAICOU is in ° but not necessary in trigo
!       Transformation in trigo for GAIA
        IF(.NOT.CONV_WAVES)THEN
          DO I=1,NPOIN
            THETAW%R(I)= 90.D0-THETAW%R(I)
            THETAW%R(I)= MODULO(THETAW%R(I),360.D0)
          ENDDO
        ENDIF
      ENDIF
!
      AT0=T_TEL
!
!     INFORMATION ON SUSPENSION SENT BACK
      CHARR_TEL = CHARR
      SUSP_TEL = SUSP
!
!     OV INSTEAD OF OS IN ORDER TO AVOID PROBLEMS WITH QUASI-BUBBLE ELEMENTS
!     OPERATES ONLY ON THE (1:NPOIN) RANGE OF THE TELEMAC FIELDS
!     IT IS A HIDDEN DISCRETISATION CHANGE
!
      CALL OV('X=Y     ', X=U2D%R, Y=U_TEL%R, DIM1=NPOIN)
      CALL OV('X=Y     ', X=V2D%R, Y=V_TEL%R, DIM1=NPOIN)
      CALL OV('X=Y     ', X=HN%R, Y=H_TEL%R, DIM1=NPOIN)
!     BOTTOM GIVEN BY CALLING PROGRAMME
      CALL OS('X=Y     ', X=ZF, Y=ZF_TEL)
!
!     CLIPS NEGATIVE DEPTHS
!
      IF(OPTBAN.GT.0) THEN
        DO I = 1,NPOIN
          IF(HN%R(I).LT.HMIN) THEN
            U2D%R(I)=0.D0
            V2D%R(I)=0.D0
            HN%R(I) = HMIN
          ENDIF
        ENDDO
      ENDIF
!
!     CASE OF TRIPLE COUPLING
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
!       incident wave direction
        CALL OS( 'X=Y     ',THETAW,THETAW_TEL)
!       THETAW_TEL is in ° but not necessary in trigo
!       Transformation in trigo for GAIA
        IF(.NOT.TRIGO)THEN
          DO I=1,NPOIN
            THETAW%R(I)= 90.D0-THETAW%R(I)
            THETAW%R(I)= MODULO(THETAW%R(I),360.D0)
          ENDDO
        ENDIF
!       Wave period
        CALL OS( 'X=Y     ', TW, TW_TEL)
!       significant wave height
        CALL OS( 'X=Y     ', HW , HW_TEL)
!       Bottom orbital velocity
        CALL OS( 'X=Y     ', UW , UW_TEL)
        HW%TYPR='Q'
        TW%TYPR='Q'
        THETAW%TYPR='Q'
        UW%TYPR='Q'
      ENDIF
!
!     END COUPLING
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'USER_FORCING_GAIA'
      IF(.NOT.DEBU) THEN
        CALL USER_FORCING_GAIA
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'END_USER_FORCING_GAIA'
!
!     AT THIS LEVEL U2D,V2D,H AND ZF MUST HAVE BEEN DEFINED
!     EITHER BY READ_DATASET, USER_FORCING_GAIA OR CALLING PROGRAM
!
!     NOW COMPUTES FUNCTIONS OF U2D,V2D,HN AND ZF
!
!     FREE SURFACE
      IF(DEBUG.GT.0) WRITE(LU,*) 'FREE SURFACE'
      CALL OS('X=Y+Z   ', X=Z, Y=ZF, Z=HN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'END FREE SURFACE'
!
!     CHECKS THE WAVE DATA
!
      IF(HOULE) THEN
        IF(UW%TYPR.EQ.'Q') THEN
!         IF(HW%TYPR    .NE.'Q'.OR.
        ELSEIF(HW%TYPR    .NE.'Q'.OR.
     &    TW%TYPR    .NE.'Q'.OR.
     &    THETAW%TYPR.NE.'Q') THEN
          WRITE(LU,*) ' '
          WRITE(LU,*) ' '
          WRITE(LU,*) 'MISSING WAVE DATA'
          IF(HW%TYPR.NE.'Q') WRITE(LU,*) 'WAVE HEIGHT HM0'
          IF(TW%TYPR.NE.'Q') WRITE(LU,*) 'PEAK PERIOD TPR5'
          IF(THETAW%TYPR.NE.'Q') WRITE(LU,*) 'MEAN DIRECTION'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!     END OF HYDRODYNAMIC INITIALISATION
!
!     COMPUTES AREAS (WITHOUT MASKING)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'VECTOR FOR VOLU2D'
      CALL VECTOR(VOLU2D,'=','MASBAS          ',
     &            IELMH_GAI,1.D0,
     &            T1,T1,T1,T1,T1,T1,MESH,.FALSE.,MASKEL)
      IF(DEBUG.GT.0) WRITE(LU,*) 'END VECTOR FOR VOLU2D'
!     V2DPAR : LIKE VOLU2D BUT IN PARALLEL VALUES COMPLETED AT
!              INTERFACES BETWEEN SUBDOMAINS
      CALL OS('X=Y     ',X=V2DPAR,Y=VOLU2D)
      IF(NCSIZE.GT.1) CALL PARCOM(V2DPAR,2,MESH)
!     INVERSE OF VOLUMES (DONE WITHOUT MASKING)
      CALL OS('X=1/Y   ',X=UNSV2D,Y=V2DPAR,
     &        IOPT=2,INFINI=0.D0,ZERO=1.D-12)
!
!     INITIALIZATION FOR SETTLING VELOCITIES
!
      CALL SETTLING_VEL
!
!
!     INITIALIZATION FOR SHIELDS PARAMETER
!
      CALL SHIELDS
!
!     INITIALISATION FOR SEDIMENT IN BED MODEL
!
      IF (BED_MODEL.EQ.1.OR.BED_MODEL.EQ.2)THEN
!       model multilayer or multilayer with consolidation
        IF(DEBUG.GT.0) WRITE(LU,*) 'BED1_INIT_SEDIMENT_GAIA'
        CALL BED1_INIT_SEDIMENT_GAIA(NSICLA,ELAY,ZF,ZR,NPOIN,
     &                   XMVS0,ES,NOMBLAY,DEBU,VOLU2D,NUMSTRAT,
     &                   MAXVAR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END BED1_INIT_SEDIMENT_GAIA'


!    CONVERSION MASS TO THICKNESS

      IF(NESTOR.OR.VSMTYPE==1) THEN

!     |   faktor = 1 / (  density * area * ( 1 - porosity )  )         |
!     |   =>  mass * faktor = thickness                                |
!     | but VARIOUS MASSES ARE STILL IN [kg/m**2]  ==>
!
        DO ICLA = 1,NSICLA
          DO I = 1,NPOIN
            M2T(ICLA,I) = 1.D0/(XMVS0(ICLA)*VOLU2D%R(I)*(1.D0-XKV0(1)))
          ENDDO
            MPA2T(ICLA) = 1.D0/(  XMVS0(ICLA) * (1.D0-XKV0(1))  )
        ENDDO
      ENDIF
!
        IF(HIRANO) THEN
          IF(DEBUG.GT.0) WRITE(LU,*)'BED1_UPDATE 1'
            CALL BED1_UPDATE(ZR,ZF,VOLU2D)
          IF(DEBUG.GT.0) WRITE(LU,*)'END BED1_UPDATE 1'
          IF(DEBUG.GT.0) WRITE(LU,*)'BED1_UPDATE_ACTIVELAYER_HIRANO'
            CALL BED1_UPDATE_ACTIVELAYER_HIRANO
          IF(DEBUG.GT.0) THEN
            WRITE(LU,*)'END BED1_UPDATE_ACTIVELAYER_HIRANO'
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*)'BED1_UPDATE 2'
          CALL BED1_UPDATE(ZR,ZF,VOLU2D)
        IF(DEBUG.GT.0) WRITE(LU,*)'END BED1_UPDATE 2'
!
!     INITIALIZATION OF CVSM MODEL
        IF(VSMTYPE.EQ.1) THEN
          IF(.NOT.HIRANO) THEN
            WRITE(LU,*)'WITH CVSM MODEL HIRANO MUST BE CHOSEN!'
            CALL PLANTE(1)
          ENDIF
          IF(NMUD.GT.0) THEN
            WRITE(LU,*)'CVSM MODEL ONLY WITH SAND/GRAVEL CLASSES'
            CALL PLANTE(1)
          ENDIF

!  ----     PRINTOUTS TO C-VSP
!
          IF(CVSMPPERIOD.EQ.0) CVSMPPERIOD = LEOPR
          CVSM_OUT  = .FALSE.
          IF(CVSMPPERIOD*(LT/CVSMPPERIOD).EQ.LT) CVSM_OUT = .TRUE.
          WRITE(LU,*) ' '
          WRITE(LU,*) '------------------------------------------------'
          WRITE(LU,*) 'C-VSM MODEL'
          WRITE(LU,*) 'CONTINUOUS VERTICAL GRAIN SORTING STRATIGRAPHY'
          WRITE(LU,*) ' '
          WRITE(LU,*) 'ACTIVE LAYER THICKNESS MODEL:', ALT_MODEL
!
          IF (PRO_MAX_MAX .GT. 250) THEN
            WRITE(LU,*) 'HIGH NUMBER OF SECTIONS IS EXPENSIVE',
     &PRO_MAX_MAX
            WRITE(LU,*) 'BETTER < 250 and > 4 + 4 x NUMBER OF FRACTIONS'
          ENDIF
!
          IF ((CVSMPPERIOD / NIT) .GT. 5) THEN
            WRITE(LU,*) 'HIGH NUMBER OF FULL CVSM PRINTOUTS'
            WRITE(LU,*) 'ATTENTION: DISK SPACE AND SIMULATION TIME'
            WRITE(LU,*) 'ADAPT C-VSM FULL PRINTOUT PERIOD'
          ENDIF
!
          WRITE(LU,*) ' '
          CALL CVSP_INIT_GAIA()
          WRITE(LU,*)'CVSM INITIALISED!'
          WRITE(LU,*) '------------------------------------------------'
          ENDIF
        ENDIF ! HIRANO.AND.VSMTYPE=1
      ELSE ! BED_MODEL NE 1 OR 2
        WRITE(LU,*)'ONLY BED_MODEL 1 OR 2 FOR TIS TIME'
        STOP
      ENDIF

!     INITIALIZATION OF ACLADM AND UNLADM
      IF(NSAND.GT.0) THEN
        CALL MEAN_GRAIN_SIZE_GAIA
      ENDIF!
!
!     CALUCLATION INITIAL MASS IN ACTIVE LAYER
!     AT THE MOMENT MASS0ACT IS ONLY COMPUTED FOR MASS
!     BALANCE
!
      IF(BILMA) THEN
        MASS0ACT = 0.D0
        IF(NSAND.GT.0) THEN
          DO IPOIN=1,NPOIN
            DO ISAND=1,NSAND
              K = NUM_ISAND_ICLA(ISAND)
              MASS0ACT(K) = MASS0ACT(K) + MASS_SAND(ISAND,1,IPOIN)
     &                      *VOLU2D%R(IPOIN)
            ENDDO
          ENDDO
        ENDIF
        IF(NMUD.GT.0) THEN
          DO IPOIN=1,NPOIN
            DO IMUD=1,NMUD
              K = NUM_IMUD_ICLA(IMUD)
              MASS0ACT(K) = MASS0ACT(K) + MASS_MUD(IMUD,1,IPOIN)
     &                      *VOLU2D%R(IPOIN)
            ENDDO
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          DO ICLA=1,NSICLA
            MASS0ACT(ICLA) = P_SUM(MASS0ACT(ICLA))
          ENDDO
        ENDIF
      ENDIF
!
!     MEAN VELOCITY
!
      CALL OS('X=N(Y,Z)',X=UNORM,Y=U2D,Z=V2D)
!
!     DIRECTION OF CURRENT THETAC (° and trigo )
!                                 (same convention as THETAW in GAIA)
      CALL BEDLOAD_DIRECTION_GAIA(U2D,V2D,NPOIN,PI,THETAC)
!     radians to  degres
      DO I=1,NPOIN
        THETAC%R(I)=THETAC%R(I)*180/PI
        THETAC%R(I)=MODULO(THETAC%R(I),360.D0)
      ENDDO
!
!     WAVE ORBITAL VELOCITY
!
      IF(HOULE) THEN
!       directly if found in hydro file; otherwise compute with CALCUW_GAIA
        IF(UW%TYPR.NE.'Q') THEN
          CALL CALCUW_GAIA(UW%R,HN%R,HW%R,TW%R,GRAV,NPOIN, TYPE_HOULE)
        ELSE
            IF(TYPE_HOULE==1)THEN
!             UW is OK!
              CONTINUE
            ELSEIF(TYPE_HOULE==2)THEN
!             Case of tomawac (irregular waves)
!             UW calculated from sprectum is Urms
!             transformation for a Jonswap spectrum (Soulsby 1993)
              DO I=1,NPOIN
                UW%R(I)=SQRT(2.D0)*UW%R(I)
              ENDDO
            ELSE
              WRITE(LU,*)'VALUE OF TYPE OF WAVES IS NOT OK'
              CALL PLANTE(1)
              STOP
            ENDIF
        ENDIF
      ENDIF
!
!     TOTAL STRESS AT THE BOTTOM
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'TOB_GAIA'
      CALL TOB_GAIA(TOB,TOBW,TOBCW_MEAN,TOBCW_MAX,THETAC, THETAW,MU,
     &              KS,KSP,KSR,CF,FW,
     &              UETCAR,CF_TEL,KS_TEL,CODE,ICR,KSPRATIO,HOULE,
     &              GRAV,XMVE,XMVS0(1),VCE,KARMAN,ZERO,
     &              HMIN,HN,ACLADM,UNORM,UW,TW,NPOIN,KSCALC,IKS,
     &              DELTAR, H_TEL)
      IF(DEBUG.GT.0) WRITE(LU,*) 'END TOB_GAIA'
!
!     INITIALISATION FOR TRANSPORT
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'INIT_TRANSPORT_GAIA'
      CALL INIT_TRANSPORT_GAIA(HIDING,NSICLA,NPOIN,
     &   T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,
     &   CHARR,QS_C,QSXC,QSYC,CALFA_CL,SALFA_CL,COEFPN,COEFCR,
     &   SLOPEFF,SUSP,QS,QSCL,QSCL_C,QSCL_S,
     &   UNORM,U2D,V2D,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,HOULE,
     &   ACLADM,UNLADM,KSP,KSR,
     &   ICF,HIDFAC,XMVS0,XMVE,GRAV,VCE,HMIN,KARMAN,
     &   ZERO,PI,AC,CSTAEQ,SECCURRENT,BIJK,IELMT,MESH,DCLA,XWC,SEDCO,
     &   U3D,V3D,CODE,H_TEL,
     &   HW,THETAC,TOBCW_MEAN,TOBCW_MAX)
      IF(DEBUG.GT.0) WRITE(LU,*) 'END INIT_TRANSPORT_GAIA'
!
!     PRINT OF INITIAL CONDITION
!
      CALL ENTETE_GAIA(1,AT0,0)
!     PREPARES RESULTS
      CALL PREDES_GAIA(0,AT0,.TRUE.,CODE,LISTCOUNT)
!
!     PRINTS OUT THE RESULTS
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'BIEF_DESIMP'
      CALL BIEF_DESIMP(GAI_FILES(GAIRES)%FMT,VARSOR,
     &                 NPOIN,GAI_FILES(GAIRES)%LU,
     &                 AT0,0,LISTCOUNT,GRAFCOUNT,SORLEO,SORIMP,MAXVAR,
     &                 TEXTE,PTINIG,PTINIL,
     &                 ILEO=YAGOUT)
      IF(DEBUG.GT.0) WRITE(LU,*) 'END BIEF_DESIMP'
!

!     COUPLING
!
!     INITIALISING NESTOR
      IF(NESTOR) THEN
        CALL NESTOR_INTERFACE_GAIA(1,GRAFCOUNT,XMVS0,XKV0(1),VOLU2D)
        IF(LNG.EQ.1) WRITE(LU,*) 'GAIA COUPLE AVEC NESTOR'
        IF(LNG.EQ.2) WRITE(LU,*) 'GAIA COUPLED WITH NESTOR'
      ENDIF
!
      WRITE(LU,*) 'GAIA COUPLED WITH: ',CODE
!
!------------------------------------------------------------------
!
! ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
#if defined COMPAD
      CALL AD_GAIA_INITIALISATION_END
#endif
! ADJOINTWARE
!------------------------------------------------------------------
      RETURN
      END
