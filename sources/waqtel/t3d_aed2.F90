!                             ***************
                              MODULE T3D_AED2
!                             ***************
!
!***********************************************************************
! WAQTEL      V8P1
!***********************************************************************
!
!brief    Interface for TELEMAC3D to AED2 modules (libaed2)
!+        This is the main interface module that manages the connection
!+        with the host hydrodynamic model;
!+        done through the 4 PUBLIC functions listed below.
!
!history
!+        08/2013
!+        Interface for FV (Finite Volume) Model to AED2 modules (libaed2)
!+        Designed for TUFLOW-FV, released by BMT-WBM:
!+        http://www.tuflow.com/Tuflow%20FV.aspx
!+        Originally fv_aed2.F90
!+
!+        Developed by:
!+        AquaticEcoDynamics (AED) Group
!+        School of Earth & Environment
!+        (C) The University of Western Australia
!+        Copyright by the AED-team @ UWA under the GNU Public License
!+        www.gnu.org
!
!history
!+        03/2016
!+        Add new env variables and feedback links
!
!history  M. Jodeau (EDF)
!+        04/2016
!+        Modifications from fv_aed2.F90 and adaptation to TELEMAC3D
!
!history  C.-T. PHAM (LNHE)
!+        15/11/2018
!+        V8P0
!+   Variation of salinity, temperature, density with the order expected
!+   by AED2 (from top to bottom, then 2D number) + real wind velocity
!
#if defined HAVE_AED2
#include "aed2.h"
#endif

!#define FV_AED_VERS "0.9.21"

!#ifndef DEBUG
!#define DEBUG      0
!#endif
!#define _NO_ODE_   1
#if defined HAVE_AED2
      USE AED2_COMMON
      USE FV_ZONES
      USE FV_AED2_CSV_READER
      USE DECLARATIONS_SPECIAL
      USE BIEF

      IMPLICIT NONE

      PUBLIC INIT_AED2_MODELS, &
             NAMES_VAR_AED2, &
             INIT_VAR_AED2_MODELS, &
             SET_ENV_AED2_MODELS, &
             DO_AED2_MODELS, &
             CLEAN_AED2_MODELS

!#--------------------------------------------------------------------------#
!# MODULE DATA

      AED_REAL :: KW, KSED
      INTEGER  :: SOLUTION_METHOD

!# MAIN ARRAYS STORING/POINTING TO THE STATE AND DIAGNOSTIC VARIABLES
      AED_REAL,DIMENSION(:,:),POINTER :: CC, CC_DIAG

!# ARRAY POINTING TO THE LAGRANGIAN PARTICLE MASSES AND DIAGNOSTIC PROPERTIES
      AED_REAL,DIMENSION(:,:),POINTER :: PP

!# NAME OF FILE BEING USED TO LOAD INITIAL VALUES FOR BENTHIC OR BENTHIC_DIAG VARS
      CHARACTER(LEN=128) :: INIT_VALUES_FILE = ''

!# MAPS OF SURFACE, BOTTOM AND WET/DRY (ACTIVE) CELLS
      LOGICAL,DIMENSION(:),POINTER :: ACTIVE
      INTEGER,DIMENSION(:),POINTER :: SURF_MAP, BENTH_MAP

!# ARRAYS FOR WORK, VERTICAL MOVEMENT, AND CROSS-BOUNDARY FLUXES
      AED_REAL,ALLOCATABLE,DIMENSION(:,:) :: FLUX
      AED_REAL,ALLOCATABLE,DIMENSION(:)   :: WS, TOTAL
      AED_REAL,ALLOCATABLE,DIMENSION(:)   :: MIN_, MAX_

!# ARRAYS FOR ENVIRONMENTAL VARIABLES (USED IF THEY ARE NOT SUPPLIED EXTERNALLY)
      AED_REAL,ALLOCATABLE,DIMENSION(:),TARGET :: NIR
      AED_REAL,ALLOCATABLE,DIMENSION(:),TARGET :: PAR
      AED_REAL,ALLOCATABLE,DIMENSION(:),TARGET :: UVA
      AED_REAL,ALLOCATABLE,DIMENSION(:),TARGET :: UVB
      AED_REAL,DIMENSION(:),POINTER :: LPAR

      AED_REAL,TARGET :: COL_TAUB  ! A TEMP VAR FOR THE TAUB FOR COLUMN (COMPUTED FROM USTAR_BED)

!# EXTERNAL VARIABLES
      AED_REAL :: DTAED2 ! MODIF MAG !DTAED2
      AED_REAL :: EPS_AED2=1.D-9 ! MODIF MAG !DTAED2
      AED_REAL,DIMENSION(:),  POINTER :: TEMP_AED2, SALT, RHO_AED2
      AED_REAL,DIMENSION(:),  POINTER :: H_AED2, Z_AED2
      AED_REAL,DIMENSION(:),  POINTER :: EXTCOEFF, TSS, BIO_DRAG
      AED_REAL,DIMENSION(:),  POINTER :: I_0, WND, AIR_TEMP, RAINAED2 ! MODIF MAG !RAINAED2
      AED_REAL,DIMENSION(:),  POINTER :: AREA, BATHY, SHADEFRAC
      AED_REAL,DIMENSION(:),  POINTER :: RAINLOSS
      AED_REAL,DIMENSION(:),  POINTER :: FSED_SETL, USTAR_BED
      INTEGER, DIMENSION(:,:),POINTER :: MAT

!# SWITCHES FOR CONFIGURING MODEL OPERATION AND ACTIVE LINKS WITH THE HOST MODEL
      LOGICAL :: LINK_WATER_CLARITY, LINK_WATER_DENSITY,               &
                 LINK_BOTTOM_DRAG, LINK_SURFACE_DRAG,                  &
                 LINK_EXT_PAR, EXT_TSS_EXTINCTION,                     &
                 LINK_SOLAR_SHADE, LINK_RAIN_LOSS,                     &
                 DO_LIMITER, DO_2D_ATM_FLUX, DO_PARTICLE_BGC

      LOGICAL :: OLD_ZONES = .TRUE.
      LOGICAL :: DO_ZONE_AVERAGING = .FALSE.

!# INTEGERS STORING NUMBER OF VARIABLES BEING SIMULATED
      INTEGER :: N_AED2_VARS, N_VARS, N_VARS_BEN, N_VARS_DIAG
      INTEGER :: N_VARS_DIAG_SHEET

      CONTAINS

!                       ***************************
                        SUBROUTINE INIT_AED2_MODELS &
!                       ***************************
!
      (NAMLST,DNAME,NWQ_VAR,NBEN_VAR,NDIAG_VAR)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    This subroutine is called by TELEMAC-3D to define numbers of
!+        variables. TELEMAC-3D will allocate the variables after return
!+        from this subroutine
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMLST         |-->| UNIT FOR AED2 STEERING FILE
!| NBEN_VAR       |<--| NUMBER OF BENTHIC VARIABLES
!| NDIAG_VAR      |-->| NUMBER OF DIAGNOSIS VARIABLES
!| NWQ_VAR        |<--| NUMBER OF WATER QUALITY VARIABLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)  :: NAMLST
      INTEGER,          INTENT(OUT) :: NWQ_VAR,NBEN_VAR,NDIAG_VAR
      CHARACTER(LEN=*), INTENT(IN)  :: DNAME
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=30), ALLOCATABLE :: NAMES(:)
      CHARACTER(LEN=30), ALLOCATABLE :: BENNAMES(:)
      CHARACTER(LEN=30), ALLOCATABLE :: DIAGNAMES(:)
      CHARACTER(LEN=128) :: AED2_NML_FILE
      CHARACTER(LEN=200) :: TNAME
      AED_REAL           :: BASE_PAR_EXTINCTION, TSS_PAR_EXTINCTION
      INTEGER            :: STATUS
      INTEGER            :: N_SD, I, J
      TYPE(AED2_VARIABLE_T),POINTER :: TVAR

      CHARACTER(LEN=64) :: MODELS(64)
      NAMELIST /AED2_MODELS/ MODELS

      NAMELIST /AED2_BIO/ SOLUTION_METHOD, LINK_BOTTOM_DRAG,           &
                         LINK_SURFACE_DRAG, LINK_WATER_DENSITY,        &
                         LINK_WATER_CLARITY, AED2_NML_FILE,            &
                         LINK_EXT_PAR, BASE_PAR_EXTINCTION,            &
                         EXT_TSS_EXTINCTION, TSS_PAR_EXTINCTION,       &
                         DO_LIMITER, DO_2D_ATM_FLUX, DO_ZONE_AVERAGING,&
                         LINK_SOLAR_SHADE, LINK_RAIN_LOSS,             &
                         INIT_VALUES_FILE, DO_PARTICLE_BGC
!
!-----------------------------------------------------------------------
!
!BEGIN
      WRITE(LU,*) "*** USING AED2 COUPLING ***"

      OLD_ZONES = .TRUE.
      DO_ZONE_AVERAGING = .FALSE.
      AED2_NML_FILE = 'aed2.nml'
      SOLUTION_METHOD = 1
      LINK_BOTTOM_DRAG = .FALSE.
      LINK_SURFACE_DRAG = .FALSE.
      LINK_WATER_DENSITY = .FALSE.
      LINK_WATER_CLARITY = .FALSE.
      LINK_SOLAR_SHADE = .TRUE.
      LINK_RAIN_LOSS = .FALSE.
      LINK_EXT_PAR = .FALSE.
      BASE_PAR_EXTINCTION = 0.1D0
      EXT_TSS_EXTINCTION = .FALSE.
      TSS_PAR_EXTINCTION = 0.02D0
      DO_2D_ATM_FLUX = .TRUE.
      DO_LIMITER = .FALSE.
      DO_PARTICLE_BGC = .FALSE.
      DTAED2 = 0.D0

      IF ( AED2_INIT_CORE(DNAME).NE.0 ) THEN
        STOP "INITIALISATION OF AED2_CORE FAILED"
      ENDIF
      TNAME = TRIM(DNAME)//'aed2.nml'
!      TNAME = TRIM(DNAME)

      WRITE(LU,*)"READING AED2_MODELS CONFIG FROM ",TRIM(TNAME)
      WRITE(LU,*) NAMLST

      OPEN(NAMLST,FILE=TNAME,ACTION='READ',STATUS='OLD',IOSTAT=STATUS)
      IF ( STATUS.NE.0 ) CALL STOPIT("CANNOT OPEN FILE " // TRIM(TNAME))
      READ(NAMLST,NML=AED2_BIO,IOSTAT=STATUS)
      IF ( STATUS.NE.0 ) STOP "CANNOT READ NAMELIST ENTRY AED2_BIO"
      KW = BASE_PAR_EXTINCTION
      KSED = TSS_PAR_EXTINCTION

      MODELS = ''
      READ(NAMLST, NML=AED2_MODELS, IOSTAT=STATUS)
      IF ( STATUS.NE.0 ) STOP "CANNOT READ NAMELIST ENTRY AED2_MODELS"

      IF ( DO_ZONE_AVERAGING ) OLD_ZONES = .FALSE.

      DO I=1,SIZE(MODELS)
        IF (MODELS(I).EQ.'') EXIT
        CALL AED2_DEFINE_MODEL(MODELS(I), NAMLST)
      ENDDO

      N_AED2_VARS = AED2_CORE_STATUS(NWQ_VAR, NBEN_VAR, NDIAG_VAR, N_SD)

!#IF DEBUG
      DO I=1,N_AED2_VARS
        IF ( AED2_GET_VAR(I, TVAR) ) THEN
!         WRITE(LU,*)"AED2 VAR ", I, TVAR%SHEET, TVAR%DIAG, TVAR%EXTERN, TVAR%FOUND, TRIM(TVAR%NAME),'  ', TRIM(TVAR%MODEL_NAME)
        ELSE
          WRITE(LU,*)"AED2 VAR ", I, " IS EMPTY"
        ENDIF
      ENDDO
!#endif

      NDIAG_VAR = NDIAG_VAR + N_SD
      N_VARS = NWQ_VAR
      N_VARS_BEN = NBEN_VAR
      N_VARS_DIAG = NDIAG_VAR
      N_VARS_DIAG_SHEET = N_SD

!#IF DEBUG
!      WRITE(LU,*)'AED2 INIT_AED2_MODELS : N_AED2_VARS = ',N_AED2_VARS,' NWQ_VAR = ',NWQ_VAR,' NBEN_VAR ',NBEN_VAR
!#endif

      CALL CHECK_DATA

!# NAMES = GRAB THE NAMES FROM INFO
      ALLOCATE(NAMES(1:NWQ_VAR),STAT=STATUS)
      IF (STATUS.NE.0) THEN
        STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (NAMES)'
      ENDIF
      ALLOCATE(BENNAMES(1:NBEN_VAR),STAT=STATUS)
      IF (STATUS.NE.0) THEN
        STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (BENNAMES)'
      ENDIF
      IF ( .NOT. ALLOCATED(DIAGNAMES) ) ALLOCATE(DIAGNAMES(NDIAG_VAR))
      IF (STATUS.NE.0) THEN
        STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (DIAGNAMES)'
      ENDIF
      ALLOCATE(MIN_(1:NWQ_VAR+NBEN_VAR))
      ALLOCATE(MAX_(1:NWQ_VAR+NBEN_VAR))

      J = 0
      DO I=1,N_AED2_VARS
        IF ( AED2_GET_VAR(I, TVAR) ) THEN
          IF ( .NOT. (TVAR%SHEET .OR. TVAR%DIAG .OR. TVAR%EXTERN) ) THEN
            J = J + 1
            NAMES(J) = TRIM(TVAR%NAME)
            MIN_(J) = TVAR%MINIMUM
            MAX_(J) = TVAR%MAXIMUM
!            WRITE(LU,*)"AED2 VAR NAME(",J,") : ", TRIM(NAMES(J))
          ENDIF
        ENDIF
      ENDDO

      J = 0
      DO I=1,N_AED2_VARS
        IF ( AED2_GET_VAR(I, TVAR) ) THEN
          IF ( TVAR%SHEET .AND. .NOT.(TVAR%DIAG.OR.TVAR%EXTERN) ) THEN
            J = J + 1
            BENNAMES(J) = TRIM(TVAR%NAME)
            MIN_(NWQ_VAR+J) = TVAR%MINIMUM
            MAX_(NWQ_VAR+J) = TVAR%MAXIMUM
!            WRITE(LU,*)"AED2 VAR_BEN NAME(",J,") : ", TRIM(BENNAMES(J))
          ENDIF
        ENDIF
      ENDDO

      J = 0
      DO I=1,N_AED2_VARS
        IF ( AED2_GET_VAR(I, TVAR) ) THEN
          IF ( TVAR%DIAG ) THEN
            J = J + 1
            DIAGNAMES(J) = TRIM(TVAR%NAME)
!            WRITE(LU,*)"AED2 DIAG NAME(",J,") : ", TRIM(DIAGNAMES(J))
          ENDIF
        ENDIF
      ENDDO

      CLOSE(NAMLST)
      ! Intialising all pointers to null
      CC => null()
      CC_DIAG => null()
      PP => null()
      ACTIVE => null()
      SURF_MAP => null()
      benTH_map => null()
      lpar => null()
      temp_aed2 => null()
      salt => null()
      rho_aed2 => null()
      h_aed2 => null()
      Z_aed2 => null()
      extcoeff => null()
      tss => null()
      bio_drag => null()
      i_0 => null()
      wnd => null()
      air_temp => null()
      rainaed2 => null()
      area => null()
      bathy => null()
      shadefrac => null()
      rainloss => null()
      fsed_setl => null()
      ustar_bed => null()
      mat => null()
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE INIT_AED2_MODELS

!                        *************************
                         SUBROUTINE NAMES_VAR_AED2 &
!                        *************************
!
      (NV_AED2,NAMESAED2)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Assigns the names of AED2 variables
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMESAED2      |<--| NAMES OF AED2 VARIABLES
!| NAMLST         |-->| UNIT FOR AED2 STEERING FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: NV_AED2
      CHARACTER(LEN=16),ALLOCATABLE,INTENT(OUT) :: NAMESAED2(:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, J, STATUS
      TYPE(AED2_VARIABLE_T),POINTER :: TVAR
!
!-----------------------------------------------------------------------
!
      ALLOCATE(NAMESAED2(1:NV_AED2),STAT=STATUS)
      IF(STATUS.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (NAMES)'

      J = 0
      DO I=1,N_AED2_VARS
        IF ( AED2_GET_VAR(I, TVAR) ) THEN
          IF ( .NOT. (TVAR%SHEET .OR. TVAR%DIAG .OR. TVAR%EXTERN) ) THEN
            J = J + 1
            NAMESAED2(J) = TRIM(TVAR%NAME)
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE NAMES_VAR_AED2

!                     *******************************
                      SUBROUTINE INIT_VAR_AED2_MODELS &
!                     *******************************
!
      (NCELLS, CC_, CC_DIAG_, NWQ, NWQBEN, SM, BM)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Allocate memory for AED2 WQ variables
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BM             |-->| NUMBER OF BENTHIC NODES
!| CC_            |-->| VALUES OF WQ VARIABLES
!| CC_DIAG        |-->| VALUES OF WQ DIAG VARIABLES
!| NCELLS         |-->| NUMBER OF 3d NODES
!| NWQ            |<->| NUMBER OF WQ VARIABLES
!| NWQBEN         |<->| NUMER OF BENTHIC WQ VARIABLES
!| SM             |-->| NUMBER OF SURFACE NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,INTENT(IN) :: NCELLS
      AED_REAL,POINTER,DIMENSION(:,:),INTENT(IN) :: CC_, CC_DIAG_
      INTEGER,INTENT(INOUT)                   :: NWQ, NWQBEN
      INTEGER,POINTER,DIMENSION(:),INTENT(IN) :: SM, BM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER RC, AV, V, SV, D, SD
      TYPE(AED2_VARIABLE_T),POINTER :: TV
!
!-----------------------------------------------------------------------
!
!BEGIN
      NWQ = N_VARS
      NWQBEN = N_VARS_BEN

!   WRITE(LU,*)'INIT_VAR_AED2_MODELS : NWQ = ',NWQ,' NWQBEN = ',NWQBEN

      CC => CC_
      CC_DIAG => CC_DIAG_
      SURF_MAP => SM
      BENTH_MAP => BM

!  ALLOCATE STATE VARIABLE ARRAY
      IF ( .NOT.ASSOCIATED(CC) ) STOP ' ERROR : NO ASSOCIATION FOR (CC)'
      CC = 0.D0

      IF (.NOT. ASSOCIATED(CC_DIAG) ) THEN
        STOP ' ERROR : NO ASSOCIATION FOR (CC_DIAG)'
      ENDIF
      CC_DIAG = 0.D0
! ALLOCATE ARRAY WITH VERTICAL MOVEMENT RATES (M/S, POSITIVE FOR UPWARDS),
! AND SET THESE TO THE VALUES PROVIDED BY THE MODEL.

      ALLOCATE(WS(1:NCELLS),STAT=RC)
      IF (RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (WS)'
      WS = 0.D0

!!# PLACE HOLDER FOR LAGRANIGAN PARTICLES
!IF(DO_PARTICLE_BGC) THEN
!  PP => PP_
!END IF

! ALLOCATE ARRAY FOR PHOTOSYNTHETICALLY ACTIVE RADIATION (PAR).
! THIS WILL BE CALCULATED INTERNALLY DURING EACH TIME STEP.
      ALLOCATE(PAR(1:NCELLS),STAT=RC)
      IF (RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (PAR)'
      PAR = 0.D0
      ALLOCATE(NIR(1:NCELLS),STAT=RC)
      IF (RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (NIR)'
      NIR = 0.D0
      ALLOCATE(UVA(1:NCELLS),STAT=RC)
      IF (RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (UVA)'
      UVA = 0.D0
      ALLOCATE(UVB(1:NCELLS),STAT=RC)
      IF (RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (UVB)'
      UVB = 0.D0

!# ALLOCATE ARRAY FOR SEDIMENTATION FLUXES AND INITIALIZE THESE TO ZERO (NO FLUX).
      ALLOCATE(FSED_SETL(1:NCELLS),STAT=RC)
      IF(RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (FSED_SETL)'
      FSED_SETL = 0.D0

!# NOW SET INITIAL VALUES
      V = 0
      SV = 0
      DO AV=1,N_AED2_VARS
        IF ( .NOT.  AED2_GET_VAR(AV, TV) ) THEN
          STOP "ERROR GETTING VARIABLE INFO"
        ENDIF
        IF ( .NOT. ( TV%EXTERN .OR. TV%DIAG) ) THEN  !# NEITHER GLOBAL NOR DIAGNOSTIC VARIABLE
          IF ( TV%SHEET ) THEN
            SV = SV + 1
            CC(N_VARS+SV, :) = TV%INITIAL
          ELSE
            V = V + 1
            CC(V,:) = TV%INITIAL
          ENDIF
        ENDIF
      ENDDO

      IF ( INIT_VALUES_FILE.NE.'' ) CALL SET_INITIAL_FROM_FILE

!      CALL FV_INITIALIZE()


      ALLOCATE(FLUX(N_VARS, NCELLS),STAT=RC)
      IF (RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (FLUX)'
!#if !_NO_ODE_
!      ALLOCATE(FLUX2(N_VARS, NCELLS),STAT=RC) ; IF (RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (FLUX2)'
!      ALLOCATE(FLUX3(N_VARS, NCELLS),STAT=RC) ; IF (RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (FLUX3)'
!      ALLOCATE(FLUX4(N_VARS, NCELLS),STAT=RC) ; IF (RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (FLUX4)'
!      ALLOCATE(CC1(N_VARS, NCELLS),STAT=RC)   ; IF (RC.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (CC1)'
!#endif
!
!-------------------------------------------------------------------------------
      CONTAINS

!                       **************************
                        CHARACTER FUNCTION TOLOWER &
!                       **************************
!
      (C)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER, INTENT(IN) :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC
!
!-----------------------------------------------------------------------
!
!BEGIN
!----------------------------------------------------------------------------
      IC = ICHAR(C)
      IF (IC.GE.65 .AND. IC.LT.90) IC = (IC+32)
      TOLOWER = CHAR(IC)
      END FUNCTION TOLOWER

!                       *****************************************
                        FUNCTION SAME_STR_ICASE(A, B) RESULT(RES)
!                       *****************************************
!
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief  Tests if A and B are the same string
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=*), INTENT(IN) :: A,B
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LEN, I
      LOGICAL RES
!
!-----------------------------------------------------------------------
!
!BEGIN
      RES = .FALSE.
      LEN = LEN_TRIM(A)
      IF ( LEN.NE.LEN_TRIM(B) ) RETURN
      DO I=1, LEN
        IF (TOLOWER(A(I:I)).NE.TOLOWER(B(I:I)) ) RETURN
      ENDDO
      RES = .TRUE.

      END FUNCTION SAME_STR_ICASE

!                     ********************************
                      SUBROUTINE SET_INITIAL_FROM_FILE
!                     ********************************
!
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief  Set initial values of AED2 variables from files
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER UNIT, NCCOLS, CCOL
      CHARACTER(LEN=32),POINTER,DIMENSION(:) :: CSVNAMES
      AED_REAL,DIMENSION(:),ALLOCATABLE :: VALUES
      INTEGER :: IDX_COL = 0, NUMV = 0, NUMD = 0, T
      INTEGER,DIMENSION(:),ALLOCATABLE :: VARS, VMAP
      INTEGER,DIMENSION(:),ALLOCATABLE :: DVAR, DMAP
      LOGICAL,DIMENSION(:),ALLOCATABLE :: VSHEET, DSHEET
      LOGICAL MEH
!
!-----------------------------------------------------------------------
!
!BEGIN
!----------------------------------------------------------------------------
      UNIT = AED_CSV_READ_HEADER(INIT_VALUES_FILE, CSVNAMES, NCCOLS)
      WRITE(LU,*)'BENTHIC VARS INITIALISED FROM FILE : ', CSVNAMES
      IF (UNIT.LE.0) RETURN !# NO FILE FOUND
      DO CCOL=1,NCCOLS
        IF ( CSVNAMES(CCOL).EQ."ID" ) THEN
          IDX_COL = CCOL
          EXIT
        ENDIF
      ENDDO

      ALLOCATE(VARS(NCCOLS))   ; ALLOCATE(VMAP(NCCOLS))
      ALLOCATE(DVAR(NCCOLS))   ; ALLOCATE(DMAP(NCCOLS))
      ALLOCATE(VSHEET(NCCOLS)) ; ALLOCATE(DSHEET(NCCOLS))
      ALLOCATE(VALUES(NCCOLS))

      IF ( IDX_COL.GT.0 ) THEN
        V = 0 ; SV = 0; D = 0; SD = 0
        DO AV=1,N_AED2_VARS
          IF ( .NOT. AED2_GET_VAR(AV, TV) ) THEN
            STOP "ERROR GETTING VARIABLE INFO"
          ENDIF
          IF ( .NOT. ( TV%EXTERN ) ) THEN  !#  DONT DO ENVIRONMENT VARS
            DO CCOL=1,NCCOLS
              IF ( SAME_STR_ICASE(TV%NAME, CSVNAMES(CCOL)) ) THEN
                IF (TV%DIAG) THEN
                  NUMD = NUMD + 1
                  DMAP(NUMD) = CCOL
                  DVAR(NUMD) = NUMD
                  DSHEET(NUMD) = TV%SHEET
                ELSE
                  NUMV = NUMV + 1
                  VMAP(NUMV) = CCOL
                  IF ( TV%SHEET ) THEN
                    SV = SV + 1
                    VARS(NUMV) = N_VARS + SV
                  ELSE
                    V = V + 1
                    VARS(NUMV) = V
                  ENDIF
                  VSHEET(NUMD) = TV%SHEET
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO

        DO WHILE ( AED_CSV_READ_ROW(UNIT, VALUES) )
          T = INT(VALUES(IDX_COL))
          DO V=1,NUMV
            IF ( VSHEET(V) ) THEN
              CC(VARS(V), BM(T)) = VALUES(VMAP(V))
            ELSE
              CC(VARS(V), SM(T):BM(T)) = VALUES(VMAP(V))
            ENDIF
          ENDDO
          DO V=1,NUMD
            IF ( VSHEET(V) ) THEN
              CC_DIAG(DVAR(V), BM(T)) = VALUES(DMAP(V))
            ELSE
              CC_DIAG(DVAR(V), SM(T):BM(T)) = VALUES(DMAP(V))
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      MEH = AED_CSV_CLOSE(UNIT)
!# DO NOT CARE IF CLOSE FAILS

      IF (ASSOCIATED(CSVNAMES)) DEALLOCATE(CSVNAMES)
      IF (ALLOCATED(VALUES))    DEALLOCATE(VALUES)
      IF (ALLOCATED(VARS))      DEALLOCATE(VARS)
      IF (ALLOCATED(VMAP))      DEALLOCATE(VMAP)
      IF (ALLOCATED(DVAR))      DEALLOCATE(DVAR)
      IF (ALLOCATED(DMAP))      DEALLOCATE(DMAP)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SET_INITIAL_FROM_FILE

      END SUBROUTINE INIT_VAR_AED2_MODELS

!                    ******************************
                     SUBROUTINE SET_ENV_AED2_MODELS &
!                    ******************************
!
                              (DT_,              &   !
! 3D ENV VARIABLES
                               TEMP_,            &
                               SALT_,            &
                               RHO_,             &
                               H_,               &
                               TSS_,             &
                               RAD_,             &
! 3D FEEDBACK ARRAYS
                               EXTCOEFF_,        &
! 2D ENV VARIABLES
                               AREA_,            &
                               I_0_,             &
                               WND_,             &
                               RAIN_,            &
                               AIR_TEMP_,        &
                               USTAR_BED_,       &
                               USTAR_SURF_,      &
                               Z_,               &
                               BATHY_,           &
                               MAT_ID_,          &
                               ACTIVE_,          &
                               BENTH_MAP_,       &
! 2D FEEDBACK ARRAYS
                               BIODRAG_,         &
                               SOLARSHADE_,      &
                               RAINLOSS_         &
                               )
!
!***********************************************************************
! WAQTEL      V8P0
!***********************************************************************
!
!brief    Provide information about TELEMAC-3D data not declared by
!+        AED2_BIO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACTIVE_        |-->| COLUMN ACTIVE STATUS
!| AIR_TEMP_      |-->| AIR TEMPERATURE
!| AREA_          |-->| CELL AREA
!| BATHY_         |-->| HEIGHT OF COLUMN BOTTOM
!| BENTH_MAP_     |-->| NUMBER OF BENTHIC NODES
!| BIODRAG_       |-->| BIODRAG
!| DT_            |-->| TIME STEP
!| EXTCOEFF_      |-->| EXTINCTION COEFFICIENT
!| H_             |-->| CELL THICKNESS
!| I_0_           |-->| NET SURFACE IRRADIANCE
!| MAT_ID_        |-->| MATERIAL ID
!| RAD_           |-->| NET SHORT WAVE RADIATION
!| RAIN_          |-->| RAIN
!| RAINLOSS_      |-->| RAIN LOSS
!| RHO_           |-->| DENSITY
!| SALT_          |-->| SALINITY
!| SOLARSHADE_    |-->| SOLAR SHADE
!| TEMP_          |-->| TEMPERATURE
!| TSS_           |-->| TSS
!| USTAR_BED_     |-->| BED FRICTION VELOCITY
!| USTAR_SURF_    |-->| SURFACE FRICTION VELOCITY
!| WND_           |-->| 10M WIND SPEED
!| Z_             |-->| DEPTH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLETYPE, INTENT(IN) :: DT_
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: TEMP_,SALT_,RHO_
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: H_,AREA_,TSS_
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: EXTCOEFF_, Z_
      AED_REAL, INTENT(IN), DIMENSION(:,:), POINTER :: RAD_
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: I_0_,WND_
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: USTAR_BED_
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: RAIN_, BATHY_ ! JC    BATHY,
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: BIODRAG_
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: USTAR_SURF_
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: SOLARSHADE_
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: RAINLOSS_ ! JC    BATHY,
      AED_REAL, INTENT(IN), DIMENSION(:),   POINTER :: AIR_TEMP_
      INTEGER,  INTENT(IN), DIMENSION(:,:), POINTER :: MAT_ID_
      INTEGER,  INTENT(IN), DIMENSION(:),   POINTER :: BENTH_MAP_
      LOGICAL,  INTENT(IN), DIMENSION(:),   POINTER :: ACTIVE_
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!BEGIN
!# PROVIDE POINTERS TO ARRAYS WITH ENVIRONMENTAL VARIABLES TO AED2.

!# 2D (SHEET) VARIABLES BEING POINTED TO
      AREA => AREA_
      I_0 => I_0_
      WND => WND_
      USTAR_BED => USTAR_BED_
      MAT => MAT_ID_
      BATHY => BATHY_
      RAINAED2  => RAIN_
      SHADEFRAC => SOLARSHADE_
      RAINLOSS => RAINLOSS_
      BIO_DRAG => BIODRAG_
      AIR_TEMP => AIR_TEMP_

!# 3D VARIABLES BEING POINTED TO
      H_AED2    => H_            !# LAYER HEIGHTS [1D ARRAY] NEEDED FOR ADVECTION, DIFFUSION
      Z_AED2    => Z_            !# DEPTH [1D ARRAY], USED TO CALCULATE LOCAL PRESSURE
      EXTCOEFF => EXTCOEFF_ !# BIOGEOCHEMICAL LIGHT ATTENUATION COEFFICIENTS [1D ARRAY],
                            !# OUTPUT OF BIOGEOCHEMISTRY, INPUT FOR PHYSICS
      SALT => SALT_
      TEMP_AED2 => TEMP_

      RHO_AED2 => RHO_
      TSS => TSS_
      ACTIVE => ACTIVE_

      BENTH_MAP => BENTH_MAP_

!IF ( .NOT. ASSOCIATED(AREA) ) WRITE(LU,*) " NO ASSOCIATION FOR AREA"
!IF ( .NOT. ASSOCIATED(I_0) ) WRITE(LU,*) " NO ASSOCIATION FOR I_0"
!IF ( .NOT. ASSOCIATED(WND) ) WRITE(LU,*) " NO ASSOCIATION FOR WND"
!IF ( .NOT. ASSOCIATED(USTAR_BED) ) WRITE(LU,*) " NO ASSOCIATION FOR USTAR_BED"
!IF ( .NOT. ASSOCIATED(MAT) ) WRITE(LU,*) " NO ASSOCIATION FOR MAT"
!IF ( .NOT. ASSOCIATED(BATHY) ) WRITE(LU,*) " NO ASSOCIATION FOR BATHY"
!IF ( .NOT. ASSOCIATED(RAINAED2) ) WRITE(LU,*) " NO ASSOCIATION FOR RAINAED2"
!IF ( .NOT. ASSOCIATED(SHADEFRAC) ) WRITE(LU,*) " NO ASSOCIATION FOR SHADEFRAC"
!IF ( .NOT. ASSOCIATED(RAINLOSS) ) WRITE(LU,*) " NO ASSOCIATION FOR RAINLOSS"
!IF ( .NOT. ASSOCIATED(BIO_DRAG) ) WRITE(LU,*) " NO ASSOCIATION FOR BIO_DRAG"
!IF ( .NOT. ASSOCIATED(AIR_TEMP) ) WRITE(LU,*) " NO ASSOCIATION FOR AIR_TEMP"
!IF ( .NOT. ASSOCIATED(H) ) WRITE(LU,*) " NO ASSOCIATION FOR H"
!IF ( .NOT. ASSOCIATED(Z) ) WRITE(LU,*) " NO ASSOCIATION FOR Z"
!IF ( .NOT. ASSOCIATED(EXTCOEFF) ) WRITE(LU,*) " NO ASSOCIATION FOR EXTCOEFF"
!IF ( .NOT. ASSOCIATED(SALT) ) WRITE(LU,*) " NO ASSOCIATION FOR SALT"
!IF ( .NOT. ASSOCIATED(TEMP) ) WRITE(LU,*) " NO ASSOCIATION FOR TEMP"
!IF ( .NOT. ASSOCIATED(RHO) ) WRITE(LU,*) " NO ASSOCIATION FOR RHO"
!IF ( .NOT. ASSOCIATED(TSS) ) WRITE(LU,*) " NO ASSOCIATION FOR TSS"
!IF ( .NOT. ASSOCIATED(ACTIVE) ) WRITE(LU,*) " NO ASSOCIATION FOR ACTIVE"

      DTAED2 = DT_

      IF (LINK_EXT_PAR) THEN
        LPAR => RAD_(1,:)
      ENDIF

      IF (OLD_ZONES) THEN
!# WE ALLOCATE THE FULL SIZE ARRAY BECAUSE THAT'S HOW THE INDICES ARE PRESENTED
        ALLOCATE(ZONE(UBOUND(MAT_ID_,2))) ! JC
        ZONE = 1.
        DO I=1, UBOUND(MAT_ID_,2) ! JC
!# USE THE BOTTOM INDEX TO FILL THE ARRAY
          ZONE(I) = MAT_ID_(1,I)! JC
        ENDDO
      ELSE
        CALL INIT_ZONES(UBOUND(MAT_ID_, 2),MAT_ID_,N_AED2_VARS,N_VARS, &
                        N_VARS_BEN)
      ENDIF

!   WRITE(LU,*)'AED2 RE_INITIALIZE SKIPED STARTING ... '
!CALL RE_INITIALIZE() !
!   WRITE(LU,*)'AED2 RE_INITIALIZE SKIPED COMPLETED'

      CONTAINS

!!                          ************************
!                           SUBROUTINE RE_INITIALIZE
!!                          ************************
!!
!!***********************************************************************
!! WAQTEL      V8P0
!!***********************************************************************
!!
!!brief    Re initialize variables
!!
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!
!!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!!
!!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!!     AED_REAL :: FLUX_BEN(N_VARS+N_VARS_BEN)
!!     AED_REAL :: FLUX_ATM(N_VARS+N_VARS_BEN)
!!     TYPE (AED2_COLUMN_T) :: COLUMN(N_AED2_VARS)
!
!      INTEGER COL, LEV, TOP, BOT, COUNT, NCOLS
!!
!!-----------------------------------------------------------------------
!!
!!BEGIN
!      NCOLS = UBOUND(ACTIVE, 1)
!      DO COL=1, NCOLS
!        TOP = SURF_MAP(COL)
!        BOT = BENTH_MAP(COL)
!        COUNT = TOP-BOT+1
!!!$        CALL DEFINE_COLUMN(COLUMN,COL,CC,CC_DIAG,FLUX,FLUX_ATM,FLUX_BEN)
!!! CTP TEMPORARY COMMENTED BECAUSE RE_INITIALIZE NOT CALLED
!        DO LEV=1, COUNT
!!         CALL AED2_INITIALIZE(COLUMN, LEV) !!MJ
!        ENDDO
!      ENDDO
!!
!!-----------------------------------------------------------------------
!!
!      RETURN
!      END SUBROUTINE RE_INITIALIZE
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SET_ENV_AED2_MODELS

!                          *********************
                           SUBROUTINE CHECK_DATA
!                          *********************
!
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Check that all variable dependencies have been met
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-------------------------------------------------------------------------------
!  CHECK THAT ALL VARIABLE DEPENDENCIES HAVE BEEN MET
!-------------------------------------------------------------------------------
!  ARGUMENTS
!
!  LOCALS
      INTEGER AV, V, D, SV, SD, EV, ERR_COUNT
      TYPE(AED2_VARIABLE_T),POINTER :: TVAR
!-------------------------------------------------------------------------------
!BEGIN
      V = 0 ; D = 0; SV = 0; SD = 0 ; EV = 0
      ERR_COUNT = 0

      DO AV=1,N_AED2_VARS
        IF ( .NOT. AED2_GET_VAR(AV, TVAR) ) THEN
          STOP "ERROR GETTING VARIABLE INFO"
        ENDIF

        IF ( TVAR%EXTERN ) THEN !# GLOBAL VARIABLE
          EV = EV + 1
          SELECT CASE (TVAR%NAME)
            CASE ( 'temperature' ) ; TVAR%FOUND = .TRUE.
            CASE ( 'salinity' )    ; TVAR%FOUND = .TRUE.
            CASE ( 'density' )     ; TVAR%FOUND = .TRUE.
            CASE ( 'layer_ht' )    ; TVAR%FOUND = .TRUE.
            CASE ( 'layer_area' )  ; TVAR%FOUND = .TRUE.
            CASE ( 'rainaed2' )    ; TVAR%FOUND = .TRUE.
            CASE ( 'rainloss' )    ; TVAR%FOUND = .TRUE.
            CASE ( 'material' )    ; TVAR%FOUND = .TRUE.
            CASE ( 'bathy' )       ; TVAR%FOUND = .TRUE.
            CASE ( 'extc_coef' )   ; TVAR%FOUND = .TRUE.
            CASE ( 'tss' )         ; TVAR%FOUND = .TRUE.
            CASE ( 'par' )         ; TVAR%FOUND = .TRUE.
            CASE ( 'nir' )         ; TVAR%FOUND = .TRUE.
            CASE ( 'uva' )         ; TVAR%FOUND = .TRUE.
            CASE ( 'uvb' )         ; TVAR%FOUND = .TRUE.
            CASE ( 'sed_zone' )    ; TVAR%FOUND = .TRUE.
            CASE ( 'wind_speed' )  ; TVAR%FOUND = .TRUE.
            CASE ( 'par_sf' )      ; TVAR%FOUND = .TRUE.
            CASE ( 'taub' )        ; TVAR%FOUND = .TRUE.
            CASE ( 'air_temp' )    ; TVAR%FOUND = .TRUE.
!            CASE DEFAULT ; CALL STOPIT("ERROR: EXTERNAL VARIABLE "//TRIM(TVAR%NAME)//" NOT FOUND.")
            END SELECT
          ELSEIF ( TVAR%DIAG ) THEN  !# DIAGNOSTIC VARIABLE
            IF ( TVAR%SHEET ) THEN
              SD = SD + 1
            ELSE
              D = D + 1
            ENDIF
          ELSE    !# STATE VARIABLE
            IF ( TVAR%SHEET ) THEN
              SV = SV + 1
            ELSE
              V = V + 1
            ENDIF
          ENDIF
        IF ( .NOT. TVAR%FOUND ) THEN
          WRITE(LU,*) 'AV & VAR NAME   ', AV, TVAR%NAME, TVAR%FOUND
          WRITE(LU,*) "ERROR: UNDEFINED VARIABLE ", TVAR%NAME
          ERR_COUNT = ERR_COUNT + 1
        ENDIF
      ENDDO

      IF ( N_VARS.LT.V ) WRITE(LU,*)"MORE VARS THAN EXPECTED"
      IF ( N_VARS_BEN.LT.SV ) WRITE(LU,*)"MORE SHEET VARS THAN EXPECTED"
      IF(N_VARS_DIAG.LT.SD+D) WRITE(LU,*)"MORE DIAG VARS THAN EXPECTED"
      IF ( N_VARS_DIAG_SHEET.LT.SD ) THEN
        WRITE(LU,*) "MORE SHEET DIAG VARS THAN EXPECTED"
      ENDIF

      IF ( ERR_COUNT.GT.0 ) CALL STOPIT("*** ERRORS IN CONFIGURATION")
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CHECK_DATA

!                          ************************
                           SUBROUTINE DEFINE_COLUMN &
!                          ************************
!
      (COLUMN, COL, CC, CC_DIAG, FLUX_PEL, FLUX_ATM, FLUX_BEN, NPLAN)
!
!***********************************************************************
! WAQTEL      V8P0
!***********************************************************************
!
!brief Set column data structure from global arrays
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CC                |-->| WQ VARIABLES
!| CC_DIAG           |-->| WQ DIAG VARIABLES
!| COL               |-->| NODE
!| COLUMN            |<->| COLUMN DATA STRUCTURE
!| FLUX_ATM          |<->| ATMOSPHERIC FLUXES
!| FLUX_BEN          |<->| BENTHIC FLUXES
!| FLUX_PEL          |<->| PELAGIC FLUXES
!| NPLAN             |-->| NUMBER OF HORIZONTAL PLANES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (AED2_COLUMN_T), INTENT(INOUT) :: COLUMN(:)
      INTEGER, INTENT(IN) :: COL,NPLAN
      AED_REAL, TARGET, INTENT(IN) :: CC(:,:)       !# (N_VARS, N_LAYERS)
      AED_REAL, TARGET, INTENT(IN) :: CC_DIAG(:,:)  !# (N_VARS, N_LAYERS)
      AED_REAL, TARGET, INTENT(INOUT) :: FLUX_PEL(:,:) !# (N_VARS, N_LAYERS)
      AED_REAL, TARGET, INTENT(INOUT) :: FLUX_ATM(:)   !# (N_VARS)
      AED_REAL, TARGET, INTENT(INOUT) :: FLUX_BEN(:)   !# (N_VARS)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER AV, TOP, BOT, V, D, SV, SD, EV
      TYPE(AED2_VARIABLE_T),POINTER :: TVAR
!
!-----------------------------------------------------------------------
!
!BEGIN
      TOP = SURF_MAP(COL)
      BOT = BENTH_MAP(COL)

      V = 0 ; D = 0; SV = 0; SD = 0 ; EV = 0
      DO AV=1,N_AED2_VARS

        IF (.NOT.AED2_GET_VAR(AV,TVAR)) THEN
          STOP "ERROR GETTING VARIABLE INFO"
        ENDIF

        IF ( TVAR%EXTERN ) THEN !# GLOBAL VARIABLE
          EV = EV + 1
          SELECT CASE (TVAR%NAME)
            CASE ( 'temperature' )
              COLUMN(AV)%CELL => TEMP_AED2(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN)
            CASE ( 'salinity' )
              COLUMN(AV)%CELL => SALT(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN)
            CASE ( 'density' )
              COLUMN(AV)%CELL => RHO_AED2(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN)
            CASE ( 'layer_ht' )
              COLUMN(AV)%CELL => H_AED2(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN)
!!$            CASE ( 'temperature' )
!!$              COLUMN(AV)%CELL => TEMP_AED2(TOP:BOT)
!!$            CASE ( 'salinity' )    ; COLUMN(AV)%CELL => SALT(TOP:BOT)
!!$            CASE ( 'densigy' )     ; COLUMN(AV)%CELL => RHO_AED2(TOP:BOT)
!!$            CASE ( 'layer_ht' )    ; COLUMN(AV)%CELL => H_AED2(TOP:BOT)
            CASE ( 'layer_area' )  ; COLUMN(AV)%CELL_SHEET => AREA(COL)
            CASE ( 'rainaed2' )
              COLUMN(AV)%CELL_SHEET => RAINAED2(COL)! DOES IT MEAN THIS VAR IS ALREADY POINTED TO THE CORRECT VARIABLE OF TFFV? JC
            CASE ( 'rainloss' )
              COLUMN(AV)%CELL_SHEET => RAINLOSS(COL)! JC
            CASE ( 'material' )    ; COLUMN(AV)%CELL_SHEET => ZONE(COL)
            CASE ( 'bathy' )       ; COLUMN(AV)%CELL_SHEET => BATHY(COL)! DOES IT MEAN THIS VAR IS ALREADY POINTED TO THE CORRECT VARIABLE OF TFFV? JC
            CASE ( 'extc_coef' )
              COLUMN(AV)%CELL => EXTCOEFF(TOP:BOT)
            CASE ( 'tss' )         ; COLUMN(AV)%CELL => TSS(TOP:BOT)
            CASE ( 'par' )         ; IF (LINK_EXT_PAR) THEN
                                        COLUMN(AV)%CELL => LPAR(TOP:BOT)
                                     ELSE
              COLUMN(AV)%CELL => PAR(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN)
!!$                                        COLUMN(AV)%CELL => PAR(TOP:BOT)
                                     ENDIF
            CASE ( 'nir' )
              COLUMN(AV)%CELL => NIR(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN)
            CASE ( 'uva' )
              COLUMN(AV)%CELL => UVA(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN)
            CASE ( 'uvb' )
              COLUMN(AV)%CELL => UVB(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN)
!!$            CASE ( 'nir' )         ; COLUMN(AV)%CELL => NIR(TOP:BOT)
!!$            CASE ( 'uva' )         ; COLUMN(AV)%CELL => UVA(TOP:BOT)
!!$            CASE ( 'uvb' )         ; COLUMN(AV)%CELL => UVB(TOP:BOT)
            CASE ( 'sed_zone' )    ; COLUMN(AV)%CELL_SHEET => ZONE(COL)
            CASE ( 'wind_speed' )  ; COLUMN(AV)%CELL_SHEET => WND(COL)
            CASE ( 'par_sf' )      ; COLUMN(AV)%CELL_SHEET => I_0(COL)
            CASE ( 'taub' )        ; COLUMN(AV)%CELL_SHEET => COL_TAUB
            CASE ( 'air_temp' )
              COLUMN(AV)%CELL_SHEET => AIR_TEMP(COL)
            CASE DEFAULT
              CALL STOPIT("ERROR: EXTERNAL VARIABLE "                  &
                          //TRIM(TVAR%NAME)//" NOT FOUND.")
          END SELECT
        ELSEIF ( TVAR%DIAG ) THEN  !# DIAGNOSTIC VARIABLE
          D = D + 1
          IF ( TVAR%SHEET ) THEN
            COLUMN(AV)%CELL_SHEET => CC_DIAG(D, BOT)
          ELSE
!           ABR TO FILL THE WHOLE ARRAY, SO THAT IT CAN BE USED IN OUTPUT
            COLUMN(AV)%CELL => CC_DIAG(D,TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN)
          ENDIF
        ELSE    !# STATE VARIABLE
          IF ( TVAR%SHEET ) THEN
            SV = SV + 1
            IF ( TVAR%BOT ) THEN
              COLUMN(AV)%CELL_SHEET => CC(N_VARS+SV, BOT)
            ELSEIF ( TVAR%TOP ) THEN
              COLUMN(AV)%CELL_SHEET => CC(N_VARS+SV, TOP)
            ENDIF
            COLUMN(AV)%FLUX_BEN => FLUX_BEN(N_VARS+SV)
            COLUMN(AV)%FLUX_ATM => FLUX_ATM(N_VARS+SV)
          ELSE
            V = V + 1
            COLUMN(AV)%CELL => CC(V,TOP:BOT)
            COLUMN(AV)%FLUX_PEL => FLUX_PEL(V,TOP:BOT)
            COLUMN(AV)%FLUX_BEN => FLUX_BEN(V)
            COLUMN(AV)%FLUX_ATM => FLUX_ATM(V)
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DEFINE_COLUMN

!                    ****************************
                     SUBROUTINE  CALCULATE_FLUXES &
!                    ****************************
!
      (COLUMN, COUNT, FLUX_PEL, FLUX_ATM, FLUX_BEN, H_AED2)
!
!***********************************************************************
! WAQTEL      V8P0
!***********************************************************************
!
!brief    Checks the current values of all state variables and repairs
!+        these
!
!history  M. JODEAU + J. VIDAL HURTADO (LNHE)
!+        20/02/2018
!+        V8P0
!+   Limitation of fluxes to prevent from negative concentrations
!+   Code similar to glm_aed2.F90
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COLUMN            |<->| COLUMN DATA STRUCTURE
!| COUNT             |-->| NUMBER OF LAYERS
!| FLUX_ATM          |<->| ATMOSPHERIC FLUXES
!| FLUX_BEN          |<->| BENTHIC FLUXES
!| FLUX_PEL          |<->| PELAGIC FLUXES
!| H_AED2            |-->| CELL THICKNESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (AED2_COLUMN_T), INTENT(INOUT) :: COLUMN(:)
      INTEGER, INTENT(IN) :: COUNT
      AED_REAL, INTENT(INOUT) :: FLUX_PEL(:,:) !# (N_VARS, N_LAYERS)
      AED_REAL, INTENT(INOUT) :: FLUX_ATM(:)   !# (N_VARS)
      AED_REAL, INTENT(INOUT) :: FLUX_BEN(:)   !# (N_VARS)
      AED_REAL, INTENT(INOUT) :: H_AED2(:)     !# (N_LAYERS)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!BEGIN
      FLUX_PEL = ZERO_
      FLUX_ATM = ZERO_
      FLUX_BEN = ZERO_

!# CALCULATE TEMPORAL DERIVATIVES DUE TO AIR-WATER EXCHANGE.
      CALL AED2_CALCULATE_SURFACE(COLUMN, 1)

!# DISTRIBUTE THE FLUXES INTO PELAGIC SURFACE LAYER
      IF ( DO_2D_ATM_FLUX .OR. COUNT.GT.1 ) THEN
        IF (H_AED2(1).GT.EPS_AED2) THEN   !!ajout mj
          FLUX_PEL(:,1) = FLUX_PEL(:,1) + FLUX_ATM(:)/H_AED2(1)
        ENDIF
      ENDIF

      IF ( .NOT. DO_ZONE_AVERAGING ) THEN
!# CALCULATE TEMPORAL DERIVATIVES DUE TO BENTHIC EXCHANGE PROCESSES.
        CALL AED2_CALCULATE_BENTHIC(COLUMN, COUNT)
!# DISTRIBUTE BOTTOM FLUX INTO PELAGIC OVER BOTTOM BOX (I.E., DIVIDE BY LAYER HEIGHT).
        IF (H_AED2(COUNT).GT.EPS_AED2) THEN   !!ajout mj
!          FLUX_PEL(:,COUNT) = FLUX_PEL(:,COUNT)/H_AED2(COUNT)
! LIMIT FLUXES DEPENDING ON THE CONCENTRATION, CODE SIMILAR TO glm_aed2.F90
          FLUX_PEL(:,COUNT) = MAX ( -1.0*CC(:,COUNT) , FLUX_PEL(:,COUNT)/H_AED2(COUNT) )
        ENDIF
      ENDIF

! LIMIT FLUXES DEPENDING ON THE CONCENTRATION, CODE SIMILAR TO glm_aed2.F90
! DONE ABOVE
!      DO I=2,COUNT
!        IF (H_AED2(I).GT.0.D0) THEN
!          FLUX_PEL(:,I) = MAX ( -1.0*CC(:,I) , FLUX_PEL(:,I)/H_AED2(I) ) ! JVH -->
!          FLUX_PEL(:,I) = MAX ( -1.0*CC(:,I) , FLUX_PEL(:,I) )
!        ENDIF
!      ENDDO

!# ADD PELAGIC SINK AND SOURCE TERMS FOR ALL DEPTH LEVELS.
      DO I=1,COUNT
        CALL AED2_CALCULATE(COLUMN, I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CALCULATE_FLUXES

!                          ***********************
                           SUBROUTINE CHECK_STATES &
!                          ***********************
!
      (COLUMN, TOP, BOT)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Checks states
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BOT               |-->| NUMBER OF THE BOTTOM LAYER
!| COLUMN            |<->| COLUMN DATA STRUCTURE
!| TOP               |-->| NUMBER OF THE SURFACE LAYER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  USES
!   USE IEEE_ARITHMETIC ! MODIF MJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (AED2_COLUMN_T),INTENT(INOUT) :: COLUMN(:)
      INTEGER,INTENT(IN) :: TOP, BOT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(AED2_VARIABLE_T),POINTER :: TV
      INTEGER I,V,LEV
!
!-----------------------------------------------------------------------
!
!BEGIN
      DO LEV=TOP, BOT
!CALL AED2_EQUILIBRATE(COLUMN, LEV)
        V = 0
        DO I=1,N_AED2_VARS
          IF ( AED2_GET_VAR(I, TV) ) THEN
            IF ( .NOT. (TV%DIAG .OR. TV%EXTERN) ) THEN
              V = V + 1
              IF ( DO_LIMITER ) THEN
                IF ( .NOT. ISNAN(MIN_(V)) ) THEN
                  IF ( CC(V, LEV).LT.MIN_(V) ) CC(V, LEV) = MIN_(V)
                ENDIF
                IF ( .NOT. ISNAN(MAX_(V)) ) THEN
                  IF ( CC(V, LEV).GT.MAX_(V) ) CC(V, LEV) = MAX_(V)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CHECK_STATES

!                          *************************
                           SUBROUTINE DO_AED2_MODELS &
!                          *************************
!
      (NCELLS, NCOLS, NPLAN, FLUXAED2, EXTCAED2, TA)
!
!***********************************************************************
! WAQTEL      V8P0
!***********************************************************************
!
!brief    Calculates source terms
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EXTCAED2          |<--| EXTINCTION COEFFICIENT
!| FLUXAED2          |<--| SOURCES TERMS
!| NCELLS            |-->| NUMBER OF 3D NODES
!| NCOLS             |-->| NUMBER OF 2D NODES
!| NPLAN             |-->| NUMBER OF HORIZONTAL PLANES
!| TA                |-->| TRACERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NCELLS, NCOLS, NPLAN
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION , INTENT(INOUT) :: FLUXAED2(:,:,:)
      DOUBLE PRECISION , INTENT(OUT  ) :: EXTCAED2(:,:)
      TYPE(BIEF_OBJ), INTENT(IN) :: TA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(AED2_VARIABLE_T),POINTER :: TV

      AED_REAL :: FLUX_BEN(N_VARS+N_VARS_BEN)
      AED_REAL :: FLUX_ATM(N_VARS+N_VARS_BEN)
      TYPE (AED2_COLUMN_T) :: COLUMN(N_AED2_VARS)

      INTEGER  I, J, COL, LEV, TOP, BOT, V
!
!-----------------------------------------------------------------------
      FLUXAED2(:,:,:)= 0.D0
      EXTCAED2(:,:)  = KW
!
!BEGIN

!     DO_ZONE_AVERAGING = .FALSE.
      IF ( DO_ZONE_AVERAGING ) THEN
        IF (LINK_EXT_PAR) THEN
          CALL CALC_ZONE_AREAS(NCOLS,TEMP_AED2,SALT,H_AED2,AREA,WND,   &
                               RHO_AED2,EXTCOEFF,I_0,PAR,TSS,ACTIVE,   &
                               RAINAED2)
        ELSE
          CALL CALC_ZONE_AREAS(NCOLS,TEMP_AED2,SALT,H_AED2,AREA,WND,   &
                               RHO_AED2,EXTCOEFF,I_0,LPAR,TSS,ACTIVE,  &
                               RAINAED2)
        ENDIF
      ENDIF

!OMP DO
!#--------------------------------------------------------------------
!# LOOP THROUGH COLUMNS DOING JOBS PRIOR TO THE KINETICS BEING SOLVED

      DO COL=1, NCOLS
        IF (.NOT. ACTIVE(COL)) CYCLE

        TOP = SURF_MAP(COL)
        BOT = BENTH_MAP(COL)

        CALL DEFINE_COLUMN(COLUMN,COL,CC,CC_DIAG,FLUX,FLUX_ATM,        &
                           FLUX_BEN,NPLAN)

!# FIRSTLY RUN THROUGH ALL VARS AND SELECT STATE VARS
        V = 0
        DO I=1,N_AED2_VARS
          IF ( AED2_GET_VAR(I, TV) ) THEN
            IF ( .NOT. (TV%SHEET .OR. TV%DIAG .OR. TV%EXTERN) ) THEN
              V = V + 1
!# ONLY FOR STATE_VARS THAT ARE NOT SHEET
              IF ( .NOT. ISNAN(TV%MOBILITY) ) THEN
                WS(TOP:BOT) = TV%MOBILITY
                CALL SETTLING(BOT-TOP+1,DTAED2,                        &
                              H_AED2(TOP+(COL-1)*NPLAN:                &
                                     BOT+(COL-1)*NPLAN),               &
                              WS(TOP:BOT),FSED_SETL(COL),COLUMN(I)%CELL)
!                             WS TO BE DEPENDENT ON COL!
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        CALL CHECK_STATES(COLUMN, TOP, BOT)

!# POPULATE LOCAL LIGHT/EXTC ARRAYS ONE COLUMN AT A TIME
        IF (.NOT. LINK_EXT_PAR) &  !#MH CHECK LINK_EXT_PAR LOGIC
          CALL LIGHT(COLUMN,BOT-TOP+1,I_0(COL),EXTCOEFF(TOP:BOT),      &
                     PAR(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN),         &
                     H_AED2(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN))

!# NON-PAR LIGHT FUDGE
        DO I=TOP,BOT
          NIR(I+(COL-1)*NPLAN) = (PAR(I+(COL-1)*NPLAN)/0.45D0)*0.510D0
          UVA(I+(COL-1)*NPLAN) = (PAR(I+(COL-1)*NPLAN)/0.45D0)*0.035D0
          UVB(I+(COL-1)*NPLAN) = (PAR(I+(COL-1)*NPLAN)/0.45D0)*0.005D0
        ENDDO
!!$        NIR(TOP:BOT) = (PAR(TOP:BOT)/0.45D0) * 0.510D0
!!$        UVA(TOP:BOT) = (PAR(TOP:BOT)/0.45D0) * 0.035D0
!!$        UVB(TOP:BOT) = (PAR(TOP:BOT)/0.45D0) * 0.005D0
      ENDDO
!OMP END DO

      IF ( DO_ZONE_AVERAGING ) THEN
        CALL COPY_TO_ZONE(NCOLS, CC, AREA, ACTIVE, BENTH_MAP)
        CALL COMPUTE_ZONE_BENTHIC_FLUXES(N_AED2_VARS, DTAED2)
      ENDIF

!OMP DO

!#--------------------------------------------------------------------
!# THIS IS THE MAIN WQ SOLUTION LOOP
      DO COL=1, NCOLS

!# FIND TOP AND BOTTOM CELL INDICES BASED ON MAPS PROVIDED BY THE HOST
        TOP = SURF_MAP(COL)
        BOT = BENTH_MAP(COL)

!# COMPUTE BOTTOM SHEAR STRESS FOR THIS COLUMN BASED ON USTAR FROM HOST
        COL_TAUB = RHO_AED2(BOT)*(USTAR_BED(COL)*USTAR_BED(COL))

        DO J=1,N_VARS
          DO LEV=TOP,BOT
!# IF BOT .NE. NPLAN, IT IS NOT THE TOP FOR TA%ADR()%P%R
!!$            CC(J,LEV) = TA%ADR(J+2)%P%R(COL+(BOT-LEV)*NCOLS)
            CC(J,LEV) = TA%ADR(J+2)%P%R(COL+(NPLAN-LEV)*NCOLS)
          ENDDO
        ENDDO

!# SET COLUMN DATA STRUCTURE FROM GLOBAL ARRAYS
        CALL DEFINE_COLUMN(COLUMN,COL,CC,CC_DIAG,FLUX,FLUX_ATM,        &
                           FLUX_BEN,NPLAN)

!        IF (.NOT. ACTIVE(COL)) THEN
!          CALL AED2_CALCULATE_RIPARIAN(COLUMN, BOT-TOP+1, ZERO_);
!          CALL AED2_CALCULATE_DRY(COLUMN, BOT-TOP+1);
!          CALL AED2_RAIN_LOSS(COLUMN, BOT-TOP+1, RAIN_LOSS);! JC
!          CYCLE
!        ELSE
!          CALL AED2_CALCULATE_RIPARIAN(COLUMN, BOT-TOP+1, 1.D0);
!        ENDIF

!#IF _NO_ODE_
!# FOR THIS COLUMN, DO THE MAIN KINETIC/BGC FLUX CALCULATION
!# (THIS INCLUDES WATER COLUMN, SURFACE AND BENTHIC INTERFACES)
        CALL CALCULATE_FLUXES(COLUMN,BOT-TOP+1,FLUX(:,TOP:BOT),        &
                              FLUX_ATM,FLUX_BEN,                       &
                              H_AED2(TOP+(COL-1)*NPLAN:                &
                                     BOT+(COL-1)*NPLAN))

!# FIND THE PARTICLES IN THIS COLUMN AND UPDATE PARTICLE BGC
        IF(DO_PARTICLE_BGC) THEN
           CALL PARTICLES(COLUMN,BOT-TOP+1,                            &
                          H_AED2(TOP+(COL-1)*NPLAN:BOT+(COL-1)*NPLAN))
        ENDIF

!# DO RIPARIAN INTERFACES FOR THIS COLUMN AND UPDATE FLUXES
! IF ( .NOT.  RIPARIAN(COLUMN, ACTIVE(COL), SHADEFRAC(COL), RAINLOSS(COL)) ) &
!    CYCLE !!! COMMENTAIRE MJ

!# NOW GO FORTH AND SOLVE THE ODE (EULER! - LINK TO FV_ODE WOULD BE NICE)
        DO LEV = TOP, BOT
          DO I = 1, N_VARS
!           EXPLICIT IF FLUX IS POSITIVE
!           IMPLICIT (USING PATANKAR, 1980) IF THE FLUX IS NEGATIVE
            IF(FLUX(I,LEV).GE.0.D0) THEN
              CC(I,LEV)=CC(I,LEV)+DTAED2*FLUX(I,LEV)
            ELSE
              CC(I,LEV)=CC(I,LEV)/(1.D0-DTAED2*FLUX(I,LEV)/MAX(CC(I,LEV),1.D-15))
            ENDIF
!#IF DEBUG>1
!# CHECK FOR NANS
            IF ( ISNAN(CC(I,LEV)) ) THEN
              WRITE(LU,*)'NAN AT I = ', I, ' LEV = ', LEV
              WRITE(LU,*)'H_AED2(LEV) = ', H_AED2(LEV+(COL-1)*NPLAN),  &
                         ' FLUX(I,LEV) = ', FLUX(I,LEV)
              WRITE(LU,*)'TOP OF COLUMN @ ',TOP,' BOTTOM OF COLUMN @ ',&
                         BOT
              CALL STOPIT('NAN VALUE')
            ENDIF

!#endif _NO_ODE

          ENDDO ! VARS
        ENDDO  ! LEVELS

        IF ( DO_ZONE_AVERAGING ) THEN ! UNTESTED
          DO I = N_VARS+1, N_VARS+N_VARS_BEN
            CC(I,BOT)=CC(I,BOT)+DTAED2*FLUX(I,BOT)

!#IF DEBUG>1
!# CHECK FOR NANS
            IF ( ISNAN(CC(I,BOT)) ) THEN
              WRITE(LU,*)'NAN AT I = ', I, ' BOT = ', BOT
              WRITE(LU,*)'H_AED2(BOT) = ',H_AED2(BOT+(COL-1)*NPLAN),   &
                         ' FLUX(I,BOT) = ', FLUX(I,BOT)
              CALL STOPIT('NAN VALUE')
            ENDIF
!#endif
          ENDDO ! BEN VARS
        ENDIF
!#endif

!# DO NON-KINETIC UPDATES TO BGC VARIABLES (EQ EQUILIBRATION)
        CALL UPDATE(COLUMN, BOT-TOP+1)

!# NOW THE BGC UPDATES ARE COMPLETE, UPDATE LINKS TO HOST MODEL
        CALL BIODRAG(COLUMN, BOT-TOP+1, BIO_DRAG(COL))
        CALL BIOEXTINCTION(COLUMN, BOT-TOP+1, EXTCOEFF(TOP:BOT))
!CALL BIODENSITY()

        CALL CHECK_STATES(COLUMN, TOP, BOT)

!  EXPORT OF SOURCE TERMS
        DO LEV = TOP, BOT
          DO I = 1, N_VARS
!           EXPLICIT IF FLUX IS POSITIVE
!           IMPLICIT (USING PATANKAR, 1980) IF THE FLUX IS NEGATIVE
            IF(FLUX(I,LEV).GE.0.D0) THEN
              FLUXAED2(COL,I,NPLAN-LEV+1) = FLUX(I,LEV)
            ELSE
              FLUXAED2(COL,I,NPLAN-LEV+1) = FLUX(I,LEV)/MAX(CC(I,LEV),1.D-15)
            ENDIF
          ENDDO !N_VARS
          EXTCAED2(COL,NPLAN-LEV+1) = KW + EXTCOEFF(LEV)
        ENDDO !LEV
      ENDDO ! COLS
!OMP END DO

      IF ( DO_ZONE_AVERAGING ) THEN
        CALL COPY_TO_ZONE(NCOLS, CC, AREA, ACTIVE, BENTH_MAP)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DO_AED2_MODELS

!                          ****************************
                           SUBROUTINE CLEAN_AED2_MODELS
!                          ****************************
!
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Cleans AED2 models
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!BEGIN
! DEALLOCATE INTERNAL ARRAYS
      IF (ALLOCATED(WS))    DEALLOCATE(WS)
      IF (ALLOCATED(TOTAL)) DEALLOCATE(TOTAL)
      IF (ALLOCATED(NIR))   DEALLOCATE(NIR)
      IF (ALLOCATED(PAR))   DEALLOCATE(PAR)
      IF (ALLOCATED(UVA))   DEALLOCATE(UVA)
      IF (ALLOCATED(UVB))   DEALLOCATE(UVB)
      IF (ALLOCATED(MIN_))   DEALLOCATE(MIN_)
      IF (ALLOCATED(MAX_))   DEALLOCATE(MAX_)
      IF (ALLOCATED(FLUX))   DEALLOCATE(FLUX)
      IF (ALLOCATED(ZONE))   DEALLOCATE(ZONE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CLEAN_AED2_MODELS

!                          ****************
                           SUBROUTINE LIGHT &
!                          ****************
      (COLUMN, COUNT, IO, EXTC, PAR_, H_)
!-------------------------------------------------------------------------------
!
!brief    Calculate photosynthetically active radiation over the entire
!+        domain based on surface radiation, and background and biotic
!+        extention.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COLUMN            |<->| COLUMN DATA STRUCTURE
!| COUNT             |-->| NUMBER OF PLANES
!| EXTC              |<->| EXTINCTION COEFFICIENT
!| H_                |<->| LAYER THICKNESS
!| I0                |-->| NET SURFACE IRRADIANCE
!| PAR_              |<->| NET SHORT WAVE RADIATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (AED2_COLUMN_T), INTENT(INOUT) :: COLUMN(:)
      INTEGER,  INTENT(IN)    :: COUNT
      AED_REAL, INTENT(IN)    :: IO
      AED_REAL, INTENT(INOUT) :: EXTC(:)
      AED_REAL, INTENT(INOUT) :: PAR_(:)
      AED_REAL, INTENT(INOUT) :: H_(:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      AED_REAL ZZ, LOCALEXT
!
!-----------------------------------------------------------------------
!
!BEGIN
      ZZ = ZERO_
      LOCALEXT = ZERO_

      CALL BIOEXTINCTION(COLUMN,COUNT,EXTC)

      LOCALEXT = EXTC(1)
      ZZ = 0.001D0 !0.5*H_(1)    !MH: ASSUME TOP OF LAYER
      PAR_(1) = 0.45D0 * IO * EXP( -(KW+LOCALEXT) * ZZ )

      IF (COUNT.LE.1) RETURN

      DO I = 2, COUNT
        LOCALEXT = EXTC(I)

!ZZ = ZZ + 0.5D0*H_(I)
        ZZ = H_(I)
        PAR_(I) = PAR_(I-1) * EXP( -(KW+LOCALEXT) * ZZ )
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE LIGHT

!                          *******************
                           SUBROUTINE SETTLING &
!                          *******************
      (N,DTAED2,H_AED2,WW,FSED,Y)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Update settling of AED2 state variables in a given column
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DTAED2         |-->| TIME STEP
!| FSED           |<->| VALUE OF SEDIMENT FLUX DUE TO SETTLING
!| H_AED2         |-->| LAYER THICKNESS
!| N              |-->| NUMBER OF VERTICAL LAYERS
!| WW             |-->| VERTICAL ADVECTION SPEED
!| Y              |<->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,INTENT(IN)     :: N         !# NUMBER OF VERTICAL LAYERS
      AED_REAL,INTENT(IN)    :: DTAED2    !# TIME STEP (S)
      AED_REAL,INTENT(IN)    :: H_AED2(:) !# LAYER THICKNESS (M)
      AED_REAL,INTENT(IN)    :: WW(:)     !# VERTICAL ADVECTION SPEED
      AED_REAL,INTENT(INOUT) :: FSED      !# VALUE OF SEDIMENT FLUX DUE TO SETTLING
      AED_REAL,INTENT(INOUT) :: Y(:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!CONSTANTS
      INTEGER,PARAMETER :: ITMAX=100
!
      INTEGER  I,K,IT
      AED_REAL STEP_DT,YC,C,CMAX
      AED_REAL CU(0:N)
!
!-----------------------------------------------------------------------
!
!BEGIN
      FSED = 0.D0 !# INITIALIZE SEDIMENT SETTLING FLUXES WITH ZERO
      CU   = 0.D0 !# INITIALIZE INTERFACE FLUXES WITH ZERO
      CMAX = 0.D0 !# INITIALIZE MAXIMUM COURANT NUMBER

!# COMPUTE MAXIMUM COURANT NUMBER
!# CALCULATED AS NUMBER OF LAYERS THAT THE PARTICLES WILL TRAVEL BASED ON SETTLING OR
!# BUOYANCY VELOCITY
!# THIS NUMBER IS THEN USED TO SPLIT THE VERTICAL MOVEMENT CALCULATIONS TO LIMIT
!# MOVEMENT ACROSS A SINGLE LAYER
      DO K=1,N-1
!# SINKING PARTICLES
        IF (H_AED2(K+1)+H_AED2(K).GT.EPS_AED2) THEN   !!ajout mj
          C=ABS(WW(K+1))*DTAED2/(0.5D0*(H_AED2(K+1)+H_AED2(K)))
          IF (C.GT.CMAX) CMAX=C
!# RISING PARTICLES
          C=ABS(WW(K))*DTAED2/(0.5D0*(H_AED2(K+1)+H_AED2(K)))
          IF (C.GT.CMAX) CMAX=C
        ENDIF
      ENDDO

      IT=MIN(ITMAX,INT(CMAX)+1)
      STEP_DT = DTAED2 / FLOAT(IT);

!# SPLITTING LOOP
      DO I=1, IT
!# VERTICAL LOOP
        DO K=1,N-1
!# COMPUTE THE SLOPE RATION
          IF (WW(K).GT.0.D0) THEN !# PARTICLE IS RISING
            YC=Y(K  )     !# CENTRAL VALUE
          ELSE !# NEGATIVE SPEED PARTICLE IS SINKING
            YC=Y(K+1)     !# CENTRAL VALUE
          ENDIF
!# COMPUTE THE LIMITED FLUX
          CU(K)=WW(K) * YC
        ENDDO

!# DO THE UPPER BOUNDARY CONDITIONS
        CU(N) = ZERO_       !# FLUX INTO THE DOMAIN FROM ATMOSPHERE

!# DO THE LOWER BOUNDARY CONDITIONS
        IF (WW(1).GT.0.D0) THEN !# PARTICLE IS RISING
          CU(0) = 0.D0  !FLUX FROM BENTHOS IS ZERO
        ELSE  !# PARTICLE IS SETTLING
          CU(0) = WW(1)*Y(1)
          FSED = CU(0) * STEP_DT !# FLUX SETTLED INTO THE SEDIMENTS PER SUB TIME STEP
        ENDIF
!# DO THE VERTICAL ADVECTION STEP INCLUDING POSITIVE MIGRATION
!# AND SETTLING OF SUSPENDED MATTER.
        DO K=1,N
          IF (H_AED2(K).GT.EPS_AED2) THEN   !!ajout mj
            Y(K)=Y(K) - STEP_DT * ((CU(K) - CU(K-1)) / H_AED2(K))
          ENDIF
        ENDDO
      ENDDO !# END OF THE ITERATION LOOP
      FSED = FSED / DTAED2 !# AVERAGE FLUX RATE FOR FULL TIME STEP USED IN AED2
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SETTLING

!                          *************************
                           LOGICAL FUNCTION RIPARIAN &
!                          *************************
      (COLUMN, ACTV, SHADE_FRAC, RAIN_LOSS)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Do Riparian functionality, including operations in dry and
!+        fringing celles populate feedback arrays to the hoist model
!+        associatied with Riparian effects
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (AED2_COLUMN_T), INTENT(INOUT) :: COLUMN(:)
      LOGICAL,  INTENT(IN)    :: ACTV
      AED_REAL, INTENT(INOUT) :: SHADE_FRAC, RAIN_LOSS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      AED_REAL LOCALSHADE, LOCALRAINL
      AED_REAL :: ONE = 1.D0
!
!-----------------------------------------------------------------------
!
!BEGIN
!# COMPUTE THE METHODS RELEVANT TO EITHER DRY OR WET CELLS
      IF (.NOT. ACTV) THEN
        CALL AED2_CALCULATE_RIPARIAN(COLUMN, 1, ZERO_);
        CALL AED2_CALCULATE_DRY(COLUMN, 1);
        RIPARIAN = .FALSE.
      ELSE
        CALL AED2_CALCULATE_RIPARIAN(COLUMN, 1, ONE);
      ENDIF

!# UPDATE FEEDBACK ARRAYS TO HOST MODEL, TO REDUCE RAIN (OR IF -VE THEN ADD FLOW)
!CALL AED2_RAIN_LOSS(COLUMN, 1, LOCALRAINL); ! COMMENTAIRE MJ
      IF (LINK_RAIN_LOSS) RAIN_LOSS = LOCALRAINL

!# UPDATE FEEDBACK ARRAYS TO SHADE THE WATER (IE REDUCE INCOMING LIGHT, IO)
!CALL AED2_LIGHT_SHADING(COLUMN, 1, LOCALSHADE)
      IF (LINK_SOLAR_SHADE) SHADE_FRAC = LOCALSHADE

      RIPARIAN = .TRUE.
      END FUNCTION RIPARIAN

!                          *****************
                           SUBROUTINE UPDATE &
!                          *****************
      (COLUMN,COUNT)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Do non-kinetic (eg equilibrium) updates to state variables in
!+        AED2 modules
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (AED2_COLUMN_T), INTENT(INOUT) :: COLUMN(:)
      INTEGER,  INTENT(IN)    :: COUNT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LEV
!
!-----------------------------------------------------------------------
!BEGIN
      DO LEV=1,COUNT
        CALL AED2_EQUILIBRATE(COLUMN, LEV)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UPDATE

!                          ********************
                           SUBROUTINE PARTICLES &
!                          ********************
      (COLUMN, COUNT, H_)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Calculate biogeochemical transformations on particles
!+        !to be completed!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (AED2_COLUMN_T), INTENT(INOUT) :: COLUMN(:)
      INTEGER,  INTENT(IN)    :: COUNT
      AED_REAL, INTENT(INOUT) :: H_(:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      AED_REAL ZZ
!
!-----------------------------------------------------------------------
!
!BEGIN
      ZZ = ZERO_

!      CALL AED2_PARTICLE_BGC(COLUMN,1,PPID ...)
      IF (COUNT.LE.1) RETURN

      DO I = 2, COUNT
!      CALL AED2_PARTICLE_BGC(COLUMN,COUNT,PPID ...)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE PARTICLES

!                          ************************
                           SUBROUTINE BIOEXTINCTION &
!                          ************************
      (COLUMN,COUNT,EXTC)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Calculate the specific light attenuation additions due to AED2
!+        modules
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (AED2_COLUMN_T), INTENT(INOUT) :: COLUMN(:)
      INTEGER,  INTENT(IN)    :: COUNT
      AED_REAL, INTENT(INOUT) :: EXTC(:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      AED_REAL LOCALEXT
!
!-----------------------------------------------------------------------
!
!BEGIN
      LOCALEXT = ZERO_

      CALL AED2_LIGHT_EXTINCTION(COLUMN, 1, LOCALEXT)
      IF (LINK_WATER_CLARITY) EXTC(1) = LOCALEXT
      IF (COUNT.LE.1) RETURN

      DO I = 2, COUNT
        CALL AED2_LIGHT_EXTINCTION(COLUMN, I, LOCALEXT)
        IF (LINK_WATER_CLARITY) EXTC(I) = LOCALEXT
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BIOEXTINCTION

!                          ******************
                           SUBROUTINE BIODRAG &
!                          ******************
      (COLUMN,COUNT,BDRAG)
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    Calculate the drag addition to be returned to the host model
!+        due to vegetation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (AED2_COLUMN_T), INTENT(INOUT) :: COLUMN(:)
      INTEGER,  INTENT(IN)    :: COUNT
      AED_REAL, INTENT(INOUT) :: BDRAG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      AED_REAL LOCALDRAG
!
!-----------------------------------------------------------------------
!
!      BEGIN
!      CALL AED2_BIO_DRAG(COLUMN, COUNT, LOCALDRAG)  ! MODIF MJ DESACTIVE

      IF (LINK_BOTTOM_DRAG) BDRAG = LOCALDRAG
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BIODRAG

!                          *********************
                           SUBROUTINE BIODENSITY &
!                          *********************
!
      (COLUMN,COUNT,BIO_DENSITY)
!
!***********************************************************************
! WAQTEL      V8P0
!***********************************************************************
!
!brief    Calculate the density addition to be returned to the host
!+        model due to WQ
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (AED2_COLUMN_T), INTENT(INOUT) :: COLUMN(:)
      INTEGER,  INTENT(IN)    :: COUNT
      AED_REAL, INTENT(INOUT) :: BIO_DENSITY(:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!BEGIN
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BIODENSITY
#endif

      END MODULE T3D_AED2
