
!     ********************************************
      MODULE AD_SISYPHE_CHECKPOINT
!     ********************************************
!
      USE BIEF_DEF
!      USE FRICTION_DEF
!
      IMPLICIT NONE

      PRIVATE

      PUBLIC :: CHECKPOINT_SIS_INIT
      PUBLIC :: CHECKPOINT_SIS_STORE, CHECKPOINT_SIS_RESTORE
      PUBLIC :: CHECKPOINT_SIS_COPY_TO_TMP_CP
      PUBLIC :: CHECKPOINT_SIS_COPY_FROM_TMP_CP
!!      PUBLIC :: CHECKPOINT_SIS_COMPARE

      PUBLIC :: SIS_ADJ_BUFFER,         SIS_TMP_CP

      TYPE CHECKPOINT_SIS_TYPE
!        SEDIMENT FRACTION FOR EACH CHECKPOINT,LAYER, CLASS, POINT
        DOUBLE PRECISION,DIMENSION(:,:,:), ALLOCATABLE       :: AVAIL
!     LAYER THICKNESSES AS DOUBLE PRECISION
        DOUBLE PRECISION,DIMENSION(:,:),   ALLOCATABLE       :: ES
!     SEDIMENT COMPOSITION
!!        TYPE(BIEF_OBJ)                                       :: AVAI
!        Active Layer Thickness
!!        TYPE(BIEF_OBJ)                                       :: ELAY
!        EVOLUTION
        TYPE(BIEF_OBJ)                                       :: E
!        Active Stratum Thickness
!!        TYPE(BIEF_OBJ)                                       :: ESTRAT
!        MEAN DIAMETER OF ACTIVE-LAYER
        TYPE(BIEF_OBJ)                                       :: ACLADM
!        BOTTOM FRICTION COEFFICIENT (CHEZY, NIKURADSE OR STICKLER)
        TYPE(BIEF_OBJ)                                       :: CHESTR
!        BOTTOM ELEVATION
        TYPE(BIEF_OBJ)                                       :: ZF
!        NON ERODABLE (RIGID) BOTTOM ELEVATION
        TYPE(BIEF_OBJ)                                       :: ZR
!        MAXIMUM EVOLUTION
        TYPE(BIEF_OBJ)                                       :: EMAX
!!
!$$$!       Simulation time
!$$$      DOUBLE PRECISION                                      :: AT

      END TYPE CHECKPOINT_SIS_TYPE

      TYPE(CHECKPOINT_SIS_TYPE), DIMENSION(:),
     &                    ALLOCATABLE, TARGET        :: CP_SIS


      TYPE(CHECKPOINT_SIS_TYPE), POINTER             :: SIS_ADJ_BUFFER
      TYPE(CHECKPOINT_SIS_TYPE), POINTER             :: SIS_TMP_CP

      ! Number of Check Points
      INTEGER                                         :: NUMCP_SIS = -1

!

      CONTAINS


!                       *****************
      SUBROUTINE CHECKPOINT_SIS_INIT
!                       *****************
     &( NUMCP )
!
!***********************************************************************
! SISYPHE VERSION 6.2          07/05/2013    J.Riehme STCE, RWTH Aachen
! SISYPHE VERSION 6.2          01/04/2012    U.Merkel
!***********************************************************************
!
!
!  FUNCTION  : Allocates / Inits all Checkpointing Parameters / Struktures
!              Use this only ones outside of the time loop
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |   NUMCP        | -->| NUMBER OF CHECKPOINTS TO ALLOCATE
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------

      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_SISYPHE

      INTEGER, INTENT(IN)  :: NUMCP

      INTEGER K

      PRINT *,'CHECKPOINT_SIS_INIT : NUMCP ', NUMCP

      IF ( NUMCP_SIS .NE. -1 )  THEN
        WRITE(LU,*)
     &       'CHECKPOINT_SIS_INIT :: CHECKPOINTS ALREADY INITIALISED'
        CALL PLANTE(1)
        STOP
      ENDIF

!-----------------------------------------------------------------------

      NUMCP_SIS = NUMCP

      ALLOCATE( CP_SIS(-2:NUMCP_SIS) )

      DO K = -2, NUMCP_SIS
!
!     COMPONENTS OF VELOCITY

        ALLOCATE(CP_SIS(K)%AVAIL(NPOIN,9,NSICLA)) ! FRACTION OF EACH CLASS FOR EACH LAYER
        ALLOCATE(CP_SIS(K)%ES(NPOIN,9))           ! THICKNESS OF EACH CLASS ???

!       SEDIMENT COMPOSITION
!         CALL ALLBLO(CP_SIS(K)%AVAI  , 'AVAI  ')       ! FRACTION OF EACH CLASS FOR THE TWO FIRST LAYERS
!       Active Layer Thickness
!         CALL BIEF_ALLVEC(1,CP_SIS(K)%ELAY, 'ELAY  ', IELMT, 1, 2,MESH) ! ACTIVE LAYER THICKNESS
!       EVOLUTION
        CALL BIEF_ALLVEC(1,CP_SIS(K)%E     ,'E     ', IELMT, 1, 2,MESH)
!       Active Stratum Thickness
!         CALL BIEF_ALLVEC(1,CP_SIS(K)%ESTRAT, 'ESTRAT', IELMT,1,2,MESH) ! 2ND LAYER THICKNESS
!       MEAN DIAMETER OF ACTIVE-LAYER
        CALL BIEF_ALLVEC(1,CP_SIS(K)%ACLADM,'ACLADM', IELMT,1,2,MESH) ! MEAN DIAMETER IN ACTIVE LAYER
!       BOTTOM FRICTION COEFFICIENT (CHEZY, NIKURADSE OR STICKLER)
        CALL BIEF_ALLVEC(1,CP_SIS(K)%CHESTR,'CHESTR', IELMT,1,2,MESH) ! FRICTION COEFFICIENT
!       BOTTOM ELEVATION
        CALL BIEF_ALLVEC(1,CP_SIS(K)%ZF    ,'ZF    ', IELMT,1,2,MESH) ! BED ELEVATIONS
!       NON ERODABLE (RIGID) BOTTOM ELEVATION
        CALL BIEF_ALLVEC(1,CP_SIS(K)%ZR    ,'ZR    ', IELMT,1,2,MESH) ! NON-ERODABLE BED ELEVATIONS
!       MAXIMUM EVOLUTION
        CALL BIEF_ALLVEC(1,CP_SIS(K)%EMAX  ,'EMAX  ', IELMT,1,2,MESH) ! VARIABLES E SUMMED U
!
      ENDDO

!     Set pointer for adjoint buffer
      SIS_ADJ_BUFFER => CP_SIS(-1)
      SIS_TMP_CP     => CP_SIS(-2)

      RETURN
      END SUBROUTINE CHECKPOINT_SIS_INIT


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                       *****************
      SUBROUTINE CHECKPOINT_SIS_STORE
!                       *****************
     &(CP_ID)
!
!***********************************************************************
! SISYPHE VERSION 6.2          07/05/2013    J.Riehme STCE, RWTH Aachen
! SISYPHE VERSION 6.2          01/04/2012    U.Merkel
!***********************************************************************
!
!
!  FUNCTION  : Stores a CHECKPOINT of the main SISYPHE PARAMETERS
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |   CP_ID        | -->| CHECKPOINT SLOT TO OVERWRITE.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------

      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_SISYPHE

      INTEGER,          INTENT(IN) :: CP_ID

      INTEGER I,J,K, N

      PRINT *,'CHECKPOINT_SIS_STORE: LT, CP_ID ',LT, CP_ID

      IF ( NUMCP_SIS .EQ. -1 )  THEN
        WRITE(LU,*)  'CHECKPOINT_SIS_STORE ::',
     &       ' CHECKPOINT SYSTEM NOT INTIALISED'
        CALL PLANTE(1)
        STOP
      ENDIF

      IF ( CP_ID < 0 .OR. CP_ID .GT. NUMCP_SIS )  THEN
        WRITE(LU,*)
     &       'CHECKPOINT_SIS_STORE :: WRONG CHECKPOINT ID ', CP_ID
        CALL PLANTE(1)
        STOP
      ENDIF

      CP_SIS(CP_ID)%AVAIL   = AVAIL   ! FRACTION OF EACH CLASS FOR EACH LAYER
      CP_SIS(CP_ID)%ES      = ES(I,J) ! THICKNESS OF EACH CLASS ???

!        SEDIMENT COMPOSITION
      !  CALL OS('X=Y     ',X=CP_SIS(CP_ID)%AVAI    ,Y=AVAI) ! FRACTION OF EACH CLASS FOR THE TWO FIRST LAYERS
!        Active Layer Thickness
      !  CALL OS('X=Y     ',X=CP_SIS(CP_ID)%ELAY    ,Y=ELAY) ! ACTIVE LAYER THICKNESS
!        EVOLUTION
      CALL OS('X=Y     ',X=CP_SIS(CP_ID)%E       ,Y=E)
!        Active Stratum Thickness
      !  CALL OS('X=Y     ',X=CP_SIS(CP_ID)%ESTRAT  ,Y=ESTRAT) ! 2ND LAYER THICKNESS
!        MEAN DIAMETER OF ACTIVE-LAYER
      CALL OS('X=Y     ',X=CP_SIS(CP_ID)%ACLADM  ,Y=ACLADM) ! MEAN DIAMETER IN ACTIVE LAYER
!        BOTTOM FRICTION COEFFICIENT (CHEZY, NIKURADSE OR STICKLER)
      CALL OS('X=Y     ',X=CP_SIS(CP_ID)%CHESTR  ,Y=CHESTR) ! FRICTION COEFFICIENT
!        BOTTOM ELEVATION
      CALL OS('X=Y     ',X=CP_SIS(CP_ID)%ZF      ,Y=ZF) ! BED ELEVATIONS
!        NON ERODABLE (RIGID) BOTTOM ELEVATION
      CALL OS('X=Y     ',X=CP_SIS(CP_ID)%ZR      ,Y=ZR) ! NON-ERODABLE BED ELEVATIONS
!        MAXIMUM EVOLUTION
      CALL OS('X=Y     ',X=CP_SIS(CP_ID)%EMAX    ,Y=EMAX) ! VARIABLES E SUMMED U

!-----------------------------------------------------------------------

!$$$!     Simulation time
!$$$      CP_SIS(CP_ID)%AT  = AT

      RETURN
      END SUBROUTINE CHECKPOINT_SIS_STORE


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                       *****************
                        SUBROUTINE CHECKPOINT_SIS_RESTORE
!                       *****************
     &(CP_ID)
!
!***********************************************************************
! SISYPHE VERSION 6.2          07/05/2013    J.Riehme STCE, RWTH Aachen
! SISYPHE VERSION 6.2          01/04/2012    U.Merkel
!***********************************************************************
!
!
!
!  FUNCTION  : RESTORES a CHECKPOINT of the main SISYPHE PARAMETERS
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |   CP_ID        | -->| CHECKPOINT SLOT TO READ
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_SISYPHE

      INTEGER,          INTENT(IN) :: CP_ID

      INTEGER                      :: I, J, K

      PRINT *,'CHECKPOINT_SIS_RESTORE : LT, CP_ID ',LT, CP_ID

      IF ( NUMCP_SIS .EQ. -1 )  THEN
        WRITE(LU,*)  'CHECKPOINT_SIS_RESTORE ::',
     &       ' CHECKPOINT SYSTEM NOT INTIALISED'
        CALL PLANTE(1)
        STOP
      ENDIF

      IF ( CP_ID < 0 .OR. CP_ID .GT. NUMCP_SIS )  THEN
        WRITE(LU,*)
     &       'CHECKPOINT_SIS_RESTORE :: WRONG CHECKPOINT ID ', CP_ID
        CALL PLANTE(1)
        STOP
      ENDIF

      AVAIL  = CP_SIS(CP_ID)%AVAIL   ! FRACTION OF EACH CLASS FOR EACH LAYER
      ES     = CP_SIS(CP_ID)%ES      ! THICKNESS OF EACH CLASS ???

!        SEDIMENT COMPOSITION
      !CALL OS('X=Y     ',Y=CP_SIS(CP_ID)%AVAI    ,X=AVAI) ! FRACTION OF EACH CLASS FOR THE TWO FIRST LAYERS
!        Active Layer Thickness
      !CALL OS('X=Y     ',Y=CP_SIS(CP_ID)%ELAY    ,X=ELAY) ! ACTIVE LAYER THICKNESS
!        EVOLUTION
      CALL OS('X=Y     ',Y=CP_SIS(CP_ID)%E       ,X=E)
!        Active Stratum Thickness
      !CALL OS('X=Y     ',Y=CP_SIS(CP_ID)%ESTRAT  ,X=ESTRAT) ! 2ND LAYER THICKNESS
!        MEAN DIAMETER OF ACTIVE-LAYER
      CALL OS('X=Y     ',Y=CP_SIS(CP_ID)%ACLADM  ,X=ACLADM) ! MEAN DIAMETER IN ACTIVE LAYER
!        BOTTOM FRICTION COEFFICIENT (CHEZY, NIKURADSE OR STICKLER)
      CALL OS('X=Y     ',Y=CP_SIS(CP_ID)%CHESTR  ,X=CHESTR) ! FRICTION COEFFICIENT
!        BOTTOM ELEVATION
      CALL OS('X=Y     ',Y=CP_SIS(CP_ID)%ZF      ,X=ZF) ! BED ELEVATIONS
!        NON ERODABLE (RIGID) BOTTOM ELEVATION
      CALL OS('X=Y     ',Y=CP_SIS(CP_ID)%ZR      ,X=ZR) ! NON-ERODABLE BED ELEVATIONS
!        MAXIMUM EVOLUTION
      CALL OS('X=Y     ',Y=CP_SIS(CP_ID)%EMAX    ,X=EMAX) ! VARIABLES E SUMMED U

!-----------------------------------------------------------------------

!$$$!     Simulation time
!$$$      AT = CP_SIS(CP_ID)%AT

      RETURN
      END SUBROUTINE CHECKPOINT_SIS_RESTORE


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                       *****************
      SUBROUTINE CHECKPOINT_SIS_COPY_TO_TMP_CP
!                       *****************
     &()
!
!***********************************************************************
! SISYPHE VERSION 6.2          07/05/2013    J.Riehme STCE, RWTH Aachen
! SISYPHE VERSION 6.2          01/04/2012    U.Merkel
!***********************************************************************
!
!
!  FUNCTION  : Stores a CHECKPOINT of the main SISYPHE PARAMETERS
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------

      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_SISYPHE

      INTEGER I,J,K, N

      PRINT *,'CHECKPOINT_SIS_COPY_TO_TMP_CP: LT ',LT

      IF ( NUMCP_SIS .EQ. -1 )  THEN
        WRITE(LU,*)  'CHECKPOINT_SIS_COPY_TO_TMP_CP ::',
     &       ' CHECKPOINT SYSTEM NOT INTIALISED'
        CALL PLANTE(1)
        STOP
      ENDIF


      CP_SIS(-2)%AVAIL   = AVAIL   ! FRACTION OF EACH CLASS FOR EACH LAYER
      CP_SIS(-2)%ES      = ES(I,J) ! THICKNESS OF EACH CLASS ???

!        SEDIMENT COMPOSITION
      !  CALL OS('X=Y     ',X=CP_SIS(-2)%AVAI    ,Y=AVAI) ! FRACTION OF EACH CLASS FOR THE TWO FIRST LAYERS
!        Active Layer Thickness
      !  CALL OS('X=Y     ',X=CP_SIS(-2)%ELAY    ,Y=ELAY) ! ACTIVE LAYER THICKNESS
!        EVOLUTION
      CALL OS('X=Y     ',X=CP_SIS(-2)%E       ,Y=E)
!        Active Stratum Thickness
      !  CALL OS('X=Y     ',X=CP_SIS(-2)%ESTRAT  ,Y=ESTRAT) ! 2ND LAYER THICKNESS
!        MEAN DIAMETER OF ACTIVE-LAYER
      CALL OS('X=Y     ',X=CP_SIS(-2)%ACLADM  ,Y=ACLADM) ! MEAN DIAMETER IN ACTIVE LAYER
!        BOTTOM FRICTION COEFFICIENT (CHEZY, NIKURADSE OR STICKLER)
      CALL OS('X=Y     ',X=CP_SIS(-2)%CHESTR  ,Y=CHESTR) ! FRICTION COEFFICIENT
!        BOTTOM ELEVATION
      CALL OS('X=Y     ',X=CP_SIS(-2)%ZF      ,Y=ZF) ! BED ELEVATIONS
!        NON ERODABLE (RIGID) BOTTOM ELEVATION
      CALL OS('X=Y     ',X=CP_SIS(-2)%ZR      ,Y=ZR) ! NON-ERODABLE BED ELEVATIONS
!        MAXIMUM EVOLUTION
      CALL OS('X=Y     ',X=CP_SIS(-2)%EMAX    ,Y=EMAX) ! VARIABLES E SUMMED U

!-----------------------------------------------------------------------

!$$$!     Simulation time
!$$$      CP_SIS(-2)%AT  = AT

      RETURN
      END SUBROUTINE CHECKPOINT_SIS_COPY_TO_TMP_CP


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                       *****************
                        SUBROUTINE CHECKPOINT_SIS_COPY_FROM_TMP_CP
!                       *****************
     &()
!
!***********************************************************************
! SISYPHE VERSION 6.2          07/05/2013    J.Riehme STCE, RWTH Aachen
! SISYPHE VERSION 6.2          01/04/2012    U.Merkel
!***********************************************************************
!
!
!
!  FUNCTION  : RESTORES a CHECKPOINT of the main SISYPHE PARAMETERS
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_SISYPHE

      INTEGER                      :: I, J, K

      PRINT *,'CHECKPOINT_SIS_COPY_FROM_TMP_CP : LT, -2 ',LT, -2

      IF ( NUMCP_SIS .EQ. -1 )  THEN
        WRITE(LU,*)  'CHECKPOINT_SIS_COPY_FROM_TMP_CP ::',
     &       ' CHECKPOINT SYSTEM NOT INTIALISED'
        CALL PLANTE(1)
        STOP
      ENDIF

      AVAIL  = CP_SIS(-2)%AVAIL   ! FRACTION OF EACH CLASS FOR EACH LAYER
      ES     = CP_SIS(-2)%ES      ! THICKNESS OF EACH CLASS ???

!        SEDIMENT COMPOSITION
      !CALL OS('X=Y     ',Y=CP_SIS(-2)%AVAI    ,X=AVAI) ! FRACTION OF EACH CLASS FOR THE TWO FIRST LAYERS
!        Active Layer Thickness
      !CALL OS('X=Y     ',Y=CP_SIS(-2)%ELAY    ,X=ELAY) ! ACTIVE LAYER THICKNESS
!        EVOLUTION
      CALL OS('X=Y     ',Y=CP_SIS(-2)%E       ,X=E)
!        Active Stratum Thickness
      !CALL OS('X=Y     ',Y=CP_SIS(-2)%ESTRAT  ,X=ESTRAT) ! 2ND LAYER THICKNESS
!        MEAN DIAMETER OF ACTIVE-LAYER
      CALL OS('X=Y     ',Y=CP_SIS(-2)%ACLADM  ,X=ACLADM) ! MEAN DIAMETER IN ACTIVE LAYER
!        BOTTOM FRICTION COEFFICIENT (CHEZY, NIKURADSE OR STICKLER)
      CALL OS('X=Y     ',Y=CP_SIS(-2)%CHESTR  ,X=CHESTR) ! FRICTION COEFFICIENT
!        BOTTOM ELEVATION
      CALL OS('X=Y     ',Y=CP_SIS(-2)%ZF      ,X=ZF) ! BED ELEVATIONS
!        NON ERODABLE (RIGID) BOTTOM ELEVATION
      CALL OS('X=Y     ',Y=CP_SIS(-2)%ZR      ,X=ZR) ! NON-ERODABLE BED ELEVATIONS
!        MAXIMUM EVOLUTION
      CALL OS('X=Y     ',Y=CP_SIS(-2)%EMAX    ,X=EMAX) ! VARIABLES E SUMMED U


!-----------------------------------------------------------------------

!$$$!     Simulation time
!$$$      AT = CP_SIS(-2)%AT

      RETURN
      END SUBROUTINE CHECKPOINT_SIS_COPY_FROM_TMP_CP


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      END MODULE AD_SISYPHE_CHECKPOINT

