!                 *****************************
                  SUBROUTINE T3D_CONSTRUCT_AED2 &
!                 *****************************
!
      (DT,TA,RHO4AED2,V2DPAR,NPLAN,SALI4AED2,TEMP4AED2,WINDSPD,NPOIN2,&
       NPOIN3,THICK4AED2,BENTH4AED2,RAIN)
!
!***********************************************************************
! WAQTEL      V8P0
!***********************************************************************
!
!brief    Allows exchange of variables when coupling with AED2 for 3D case
!         modele tuflowfv_construct-extern_wq
!
!history  M. Jodeau
!+        10/08/2016
!+        V7P3
!+       CREATION
!
!history  C.-T. PHAM (LNHE)
!+        15/11/2018
!+        V8P0
!+   Variation of salinity, temperature, density with the order expected
!+   by AED2 (from top to bottom, then 2D number) + real wind velocity
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BENTH4AED2     |-->| NUMBER OF THE BENTHOS LAYER
!| DT             |-->| TIME STEP
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN THE 2D MESH
!| NPOIN3         |-->| NUMBER OF POINTS IN THE 3D MESH
!| RAIN           |-->| RAIN
!| RHO4AED2       |-->| WATER DENSITY
!| SALI4AED2      |-->| SALINITY
!| TA             |-->| TRACER
!| TEMP4AED2      |-->| TEMPERATURE
!| THICK4AED2     |-->| THICKNESS OF LAYERS
!| V2DPAR         |-->| INTEGRAL OF 2D TEST FUNCTIONS
!| WINDSPD        |-->| MAGNITUDE OF WIND SPEED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL, ONLY : NWQVARS,NWQDIAGS,RAYAED2
      USE METEO_TELEMAC, ONLY : TAIR
#if defined HAVE_AED2
      USE T3D_AED2
#endif
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  VARIABLES T3D
      INTEGER       , INTENT(IN)   :: NPLAN,NPOIN2,NPOIN3
      DOUBLE PRECISION, INTENT(IN) :: DT
      TYPE(BIEF_OBJ), INTENT(IN)   :: TA,V2DPAR
      TYPE(BIEF_OBJ), INTENT(IN)   :: RAIN,WINDSPD
      TYPE(BIEF_OBJ), INTENT(IN)   :: RHO4AED2,THICK4AED2,BENTH4AED2
      TYPE(BIEF_OBJ), INTENT(IN)   :: TEMP4AED2,SALI4AED2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_AED2
      INTEGER,PARAMETER :: REALKIND   = 8
!DEC$ END IF
      INTEGER,PARAMETER :: DOUBLEKIND = 8
!  WQ TYPE
      TYPE :: FVWQ
      LOGICAL :: INIT = .FALSE.                                 ! WQ INITIALISED
      LOGICAL :: DISABLE = .FALSE.                              ! WQ CALCULATION FLAG
      INTEGER :: TYP                                            ! WQ MODEL ID
      CHARACTER(LEN=30) :: MODEL                                ! WQ MODEL DESCRIPTION
      LOGICAL :: UPDATED                                        ! UPDATED STATUS
      REAL(DOUBLEKIND) :: DT_UPDATE                             ! UPDATE TIMESTEP
      REAL(DOUBLEKIND) :: T_UPDATE                              ! NEXT UPDATE TIME
      INTEGER :: NC2                                            ! NUMBER OF 2D CELLS
      INTEGER :: NC3                                            ! NUMBER OF 3D CELLS
      INTEGER :: NWQ                                            ! NUMBER OF WQ CONSTITUENTS
      INTEGER :: NBEN                                           ! NUMBER OF BENTHIC WQ CONSTITUENTS
      INTEGER :: NDIAG                                          ! NUMBER OF WQ DIAGNOSTIC VARIABLES
      CHARACTER(LEN=30),ALLOCATABLE,DIMENSION(:) :: NAMES       ! WQ PELAGIC CONSTITUENT NAMES
      CHARACTER(LEN=30),ALLOCATABLE,DIMENSION(:) :: BEN_NAMES   ! WQ BENTHIC CONSTITUENT NAMES
      CHARACTER(LEN=30),ALLOCATABLE,DIMENSION(:) :: DIAG_NAMES  ! WQ DIAGNOSTIC VARIABLE NAMES
      INTEGER,POINTER,DIMENSION(:) :: SURF_MAP                  ! SURFACE CELL MAP (NC2)
      INTEGER,POINTER,DIMENSION(:) :: BENTH_MAP                 ! BOTTOM/BENTHIC LAYER MAP (NC2)
      INTEGER,POINTER,DIMENSION(:) :: NL                        ! NUMBER OF LAYERS (NC2)
      INTEGER,POINTER,DIMENSION(:,:) :: MAT_ID                  ! MATERIAL ID (NMG,NC2)
      REAL(REALKIND),POINTER,DIMENSION(:) :: THICK              ! CELL THICKNESS (NC3)
      REAL(REALKIND),POINTER,DIMENSION(:) :: DEPTH              ! LOCAL MID-CELL DEPTH (NC3)
      REAL(REALKIND),POINTER,DIMENSION(:,:) :: DCDT             ! TEMPORAL DERIVATIVE OF WQ CONSTITUENTS (NWQ,NC3)
      REAL(REALKIND),POINTER,DIMENSION(:) :: SAL                ! SALINITY POINTER (NC3)
      REAL(REALKIND),POINTER,DIMENSION(:) :: TEMP               ! TEMPERATURE POINTER (NC3)
      REAL(REALKIND),POINTER,DIMENSION(:) :: TSS                ! TOTAL SUSPENDED SOLIDS POINTER (NC3)
      REAL(REALKIND),POINTER,DIMENSION(:) :: VVEL               ! VERTICAL VELOCITIES (NC3)
      REAL(REALKIND),POINTER,DIMENSION(:,:) :: PAR              ! NET SHORTWAVE RADIATION (NC3)
      REAL(REALKIND),POINTER,DIMENSION(:,:) :: CC               ! WQ CONSTITUENT CONCENTRATIONS (NWQ,NC3)
      REAL(REALKIND),POINTER,DIMENSION(:,:) :: DIAG             ! DIAGNOSTIC WQ VARIABLES (NDIAG,NC3)
      REAL(REALKIND),POINTER,DIMENSION(:) :: DENSITY            ! ABSOLUTE DENSITY (NC3)
      REAL(REALKIND),POINTER,DIMENSION(:) :: I_0                ! NET SURFACE IRRADIANCE (NC2)
      REAL(REALKIND),POINTER,DIMENSION(:) :: WIND               ! 10M WINDSPEED (NC2)
      REAL(REALKIND),POINTER,DIMENSION(:) :: PRECIP             ! RAIN (NC2)                                                         !#MH
      REAL(REALKIND),POINTER,DIMENSION(:) :: USTAR_BED          ! BED FRICTION VELOCITY (NC2)
      REAL(REALKIND),POINTER,DIMENSION(:) :: USTAR_SURF         ! SURFACE FRICTION VELOCITY (NC2)
      REAL(REALKIND),POINTER,DIMENSION(:) :: AIR_TEMP           ! AIR TEMPERATURE (NC2)
!  ARRAYS THAT CONTROL FEEDBACKS BETWEEN THE MODELS
      REAL(REALKIND),POINTER,DIMENSION(:) :: BIOSHADE           ! BIOGEOCHEMICAL LIGHT EXTINCTION COEFFICIENT RETURNED FROM WQ (NC3)
      REAL(REALKIND),POINTER,DIMENSION(:) :: BIODRAG            ! ADDITIONAL DRAG ON FLOW FROM BIOLOGY, RETURNED FROM WQ (NC3)       !#MH
      REAL(REALKIND),POINTER,DIMENSION(:) :: SOLARSHADE         ! REDUCTION OF SOLAR RADIATION DUE TO SHADING RETURNED FROM WQ (NC2) !#MH
      REAL(REALKIND),POINTER,DIMENSION(:) :: RAINLOSS           ! LOSS OF RAINFALL INTO EXPOSED SEDIMENT RETURNED FROM WQ (NC2)      !#MH
!  VARIABLES REQUIRED FOR AED2 DRY CELL MODELS
      LOGICAL,POINTER,DIMENSION(:) :: ACTIVE                    ! COLUMN ACTIVE STATUS (NC2)
      REAL(REALKIND),POINTER,DIMENSION(:) :: AREA               ! CELL AREA (NC2)
      REAL(REALKIND),POINTER,DIMENSION(:) :: BATHY              ! HEIGHT OF COLUMN BOTTOM (NC2)                                      !#MH
      END TYPE

      TYPE(FVWQ) :: WQ

!  SUBROUTINE ARGUMENTS
!  INTEGER,INTENT(IN) :: NLOG

!  LOCAL VARIABLES
      INTEGER I,J
      INTEGER RC1
!
!-----------------------------------------------------------------------
!
!  INITIALISE AED2 LINKAGE
      WRITE(LU,*) 'INITIALISING "AED2" EXTERNAL MODULE:'
!  CORRESPONDANCE T3D-AED2
      WQ%NC3 = NPOIN3

!  VARIABLE 3D
      ALLOCATE(WQ%CC(1:NWQVARS,1:NPOIN3),STAT=RC1)
      IF (RC1.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (WQ%CC)'

      ALLOCATE(WQ%DIAG(1:NWQDIAGS,1:NPOIN3),STAT=RC1)
      IF (RC1.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (WQ%DIAG)'

      ALLOCATE(WQ%TEMP(1:NPOIN3),STAT=RC1)
      IF (RC1.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (WQ%TEMP)'

      ALLOCATE(WQ%SAL(1:NPOIN3),STAT=RC1)
      IF (RC1.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (WQ%SAL)'

      ALLOCATE(WQ%DENSITY(1:NPOIN3),STAT=RC1)
      IF (RC1.NE.0) THEN
        STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (WQ%DENSITY)'
      ENDIF

      ALLOCATE(WQ%THICK(1:NPOIN3),STAT=RC1)
      IF(RC1.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (WQ%THICK)'

      ALLOCATE(WQ%TSS(1:NPOIN3),STAT=RC1)
      IF (RC1.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (WQ%TSS)'

!      ALLOCATE(WQ%PAR(1:NPOIN3,   ),STAT=RC1)
!      IF (RC1.NE.0) STOP 'ALLOCATE_MEMORY(): ERROR ALLOCATING (WQ%PAR)'

      ALLOCATE(WQ%BIOSHADE(1:NPOIN3),STAT=RC1)
      ALLOCATE(WQ%BIODRAG(1:NPOIN3),STAT=RC1)

!  VARIABLES 2D
      ALLOCATE(WQ%AREA(1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%I_0(1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%WIND(1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%PRECIP(1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%AIR_TEMP(1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%USTAR_BED(1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%USTAR_SURF(1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%DEPTH(1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%BATHY(1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%MAT_ID(1:2,1:NPOIN2),STAT=RC1) !NMG ??? NMG=2 A MODIFIER
      ALLOCATE(WQ%ACTIVE(1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%SURF_MAP (1:NPOIN2),STAT=RC1)
      ALLOCATE(WQ%BENTH_MAP (1:NPOIN2),STAT=RC1)

!  AJOUT INITIALISATION
      WQ%ACTIVE = .TRUE.
      WQ%MAT_ID = 0
!      WQ%I_0 = 500.D0
      WQ%I_0 => RAYAED2%R

      IF ( .NOT. ASSOCIATED(WQ%CC) .OR. .NOT. ASSOCIATED(WQ%DIAG) ) THEN
        WRITE(LU,*) 'WATER QUALITY VARIABLES NOT ASSOCIATED'
      ENDIF

!  ALLOCATE LOCAL MEMORY BLOCK FOR AED2 WQ VARIABLES
      CALL INIT_VAR_AED2_MODELS(WQ%NC3,WQ%CC,WQ%DIAG,WQ%NWQ,WQ%NBEN,&
                                WQ%SURF_MAP,WQ%BENTH_MAP)
      WRITE(LU,*) 'EXIT INIT_VAR_AED2_MODELS'

      WQ%DT_UPDATE = DT

!  ASSOCIATION DES POINTEURS AED2 A CEUX DE T3D
!  3D
      DO J=1,WQ%NWQ
        DO I=1,NPOIN3
          WQ%CC(J,I) = TA%ADR(J+2)%P%R(I)
        ENDDO
      ENDDO

!      WQ%TEMP => TA%ADR(IND_T)%P%R
!      WQ%SAL  => TA%ADR(IND_S)%P%R
!      WQ%WIND = 0.D0
!      WQ%DENSITY = RHO0

!      DO J=1,NPLAN-1
!        DO I=1,NPOIN2
!          WQ%THICK(I+(J-1)*NPOIN2) = Z(I+J*NPOIN2) - Z(I+(J-1)*NPOIN2)
!        ENDDO
!      ENDDO

      WQ%TEMP => TEMP4AED2%R
      WQ%SAL  => SALI4AED2%R
      WQ%WIND => WINDSPD%R
      WQ%PRECIP => RAIN%R
      WQ%DENSITY => RHO4AED2%R

      WQ%THICK => THICK4AED2%R

      WQ%AIR_TEMP => TAIR%R

!      WQ%DEPTH =
!      WQ%BATHY =
      WQ%TSS = 0.D0
!  2D
!     V2DPAR, NOT VOLU2D IN ORDER TO WORK IN PARALLEL
      WQ%AREA => V2DPAR%R
      WQ%SURF_MAP = 1
!      WQ%BENTH_MAP = NPLAN
      WQ%BENTH_MAP => BENTH4AED2%I

!  3D FEEDBACK ARRAYS
!      WQ%BIOSHADE =


!  2D FEEDBACK ARRAYS
!      WQ%BIODRAG =
!      WQ%SOLARSHADE =
!      WQ%RAINLOSS =
      WQ%USTAR_BED = 0.D0
      WQ%USTAR_SURF = 0.D0

      CALL SET_ENV_AED2_MODELS(WQ%DT_UPDATE,       &
!  3D ENV VARIABLES
                               WQ%TEMP,            &
                               WQ%SAL,             &
                               WQ%DENSITY,         &
                               WQ%THICK,           &
                               WQ%TSS,             &
                               WQ%PAR,             &
!  3D FEEDBACK ARRAYS
                               WQ%BIOSHADE,        &
!  2D ENV VARIABLES
                               WQ%AREA,            &
                               WQ%I_0,             &
                               WQ%WIND,            &
                               WQ%PRECIP,          &
                               WQ%AIR_TEMP,        &
                               WQ%USTAR_BED,       &
                               WQ%USTAR_SURF,      &
                               WQ%DEPTH,           &
                               WQ%BATHY,           &
                               WQ%MAT_ID,          &
                               WQ%ACTIVE,          &
                               WQ%BENTH_MAP,       &
!  2D FEEDBACK ARRAYS
                               WQ%BIODRAG,         &
                               WQ%SOLARSHADE,      &
                               WQ%RAINLOSS)

      WRITE(LU,*) 'SET_ENV_AED2_MODELS SUCCESSFUL'

      WQ%INIT = .TRUE.
!
#endif
      RETURN

      END SUBROUTINE
