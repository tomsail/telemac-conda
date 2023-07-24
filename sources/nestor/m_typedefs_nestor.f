!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      MODULE      m_TypeDefs_Nestor              !******       SUBROUTINE  ********************
!**                                               *********************************************
!**                                               *********************************************
!      ________________________________________________________________
!     |                                                                |
!     |     SEB @ HRW, JR @ RWTH                                       |
!     |     COMPATIBILITY WITH STANDARD FORTRAN                        |
!     |                                                                |
      INTEGER, PARAMETER :: K4  = SELECTED_INT_KIND(8)
      INTEGER, PARAMETER :: K8  = SELECTED_INT_KIND(16)
      INTEGER, PARAMETER :: R4 = SELECTED_REAL_KIND( 5, 20 )  ! SINGLE PRECISION SIZE
      INTEGER, PARAMETER :: R8 = SELECTED_REAL_KIND( 10, 60 ) ! DOUBLE PRECISION SIZE
                         !_____________________________________________|
!
!
!      ________________________________________________________________
!     |                                                                |
!     |                          DateTime                              |
      TYPE :: t_DateTime !_____________________________________________|
         INTEGER    :: year               ! The year
         INTEGER    :: month              ! The month
         INTEGER    :: day                ! The day of the month
         INTEGER    :: zone               ! Time difference with UTC in minutes
         INTEGER    :: hour               ! The hour of the day
         INTEGER    :: minutes            ! The minutes of the hour
         INTEGER    :: seconds            ! The seconds of the minute
         INTEGER    :: milliseconds       ! The milliseconds of the second
       END TYPE t_DateTime
!
!      ________________________________________________________________
!     |                                                                |
!     |                          String and his length                 |
      TYPE :: t_String_Length !________________________________________|
        CHARACTER                 (128) :: s    ! string
        INTEGER                         :: i    ! length of string
      END TYPE t_String_Length
!
!      ________________________________________________________________
!     |                                                                |
!     |                          2D point                              |
      TYPE :: t_Point_2D !_____________________________________________|
         REAL (KIND=R8):: y
         REAL (KIND=R8):: x
      END TYPE t_Point_2D
!
!      ________________________________________________________________
!     |                                                                |
!     |                          Polygon                               |
      TYPE :: t_Polygon !______________________________________________|
        CHARACTER                 (128) :: name = 'blabloblu'
        INTEGER                         :: nPoints  = -999
        TYPE(t_Point_2D), ALLOCATABLE
     &                  , DIMENSION (:) :: Pt
      END TYPE t_Polygon
!
!      ________________________________________________________________
!     |                                                                |
!     |                          Leg 3D                                |
      TYPE :: t_Leg_3D !_______________________________________________|
         REAL (KIND=R8):: x1  ! x coordinate point 1
         REAL (KIND=R8):: y1  ! y coordinate point 1
         REAL (KIND=R8):: z1  ! z coordinate point 1
         REAL (KIND=R8):: x2  ! x coordinate point 2
         REAL (KIND=R8):: y2  ! y coordinate point 2
         REAL (KIND=R8):: z2  ! z coordinate point 2
         REAL (KIND=R8):: km  ! measure of river length
      END TYPE t_Leg_3D
!
!      ________________________________________________________________
!     |                                                                |
!     |                          Interface node                        |
      TYPE :: t_iFaceNode !____________________________________________|
         INTEGER  :: Index
         INTEGER  :: nNeighbPart
      END TYPE t_iFaceNode
!
!      ________________________________________________________________
!     |                                                                |
!     |                          Field                                 |
      TYPE :: t_Field !________________________________________________|
        CHARACTER                   (128) ::   Name    = 'blabloblu'
        Integer                           ::   FieldID = -1
        INTEGER,ALLOCATABLE,DIMENSION (:) ::   Node           ! nodes inside the Polygon lokal index
        INTEGER                           ::  nNodes   = -999
!        INTEGER,ALLOCATABLE,DIMENSION (:) ::   NodeGlobal     ! nodes inside the Polygon global index
        TYPE(t_iFaceNode),ALLOCATABLE
     &                   ,DIMENSION   (:) ::   IntFacNode
        INTEGER                           ::  nIntFacNodes = -999
        REAL (KIND=R8)                    ::   Area
        REAL (KIND=R8),ALLOCATABLE
     &                ,DIMENSION   (:)    ::   NodeArea
        REAL (KIND=R8),ALLOCATABLE
     &                ,DIMENSION   (:)    ::   X
        REAL (KIND=R8),ALLOCATABLE
     &                ,DIMENSION   (:)    ::   Y
        REAL (KIND=R8),ALLOCATABLE
     &                ,DIMENSION   (:)    ::   Z
        REAL (KIND=R8),ALLOCATABLE
     &                ,DIMENSION   (:)    ::   Z0
        REAL (KIND=R8),ALLOCATABLE
     &                ,DIMENSION   (:)    ::   dZ
        REAL (KIND=R8),ALLOCATABLE
     &                ,DIMENSION   (:)    ::   dZ_Tot
        REAL (KIND=R8),ALLOCATABLE
     &                ,DIMENSION   (:)    ::   refZ
        REAL (KIND=R8),ALLOCATABLE
     &                ,DIMENSION   (:)    ::   km
        REAL (KIND=R8),ALLOCATABLE
     &                ,DIMENSION   (:)    ::   targZ
        LOGICAL       ,ALLOCATABLE
     &                ,DIMENSION   (:)    ::   NodeToDig
        INTEGER                           ::  nNodeToDig = -999
        LOGICAL       ,ALLOCATABLE
     &                ,DIMENSION   (:)    ::   NodeToDump
        INTEGER                           ::  nNodeToDump = -999
!
      END TYPE t_Field
!
!      ________________________________________________________________
!     |                                                                |
!     |                          Action                                |
      TYPE :: t_Action !_______________________________________________|
        INTEGER         ::  ActionType      = -11
        CHARACTER (128) ::  ActionTypeStr   = 'aaaaaaaaaaa'
        CHARACTER (128) ::  ActionDateStr   = 'aaaaaaaaaaa'
        CHARACTER (128) ::  FieldDig        = '000_aaaaaaaa'
        Integer         ::  FieldDigID      = -1
        CHARACTER (128) ::  ReferenceLevel  = '-1aaaa'
        REAL (KIND=R8)  ::  TimeStart       = -11.1D34    !> in practice here negative values may occure
        REAL (KIND=R8)  ::  TimeEnd         = -11.1D34    !  thus to initialise a value far bejond probability is used
        REAL (KIND=R8)  ::  TimeRepeat      = -11.1D0
!                                           
        REAL (KIND=R8)  ::  DigVolume       =  -0.1D0
        REAL (KIND=R8)  ::  DigRate         =  -0.1D0
        REAL (KIND=R8)  ::  DigDepth        = -11.1D34    !> in practice here negative values may occure
        LOGICAL         ::  DigPlanar       = .False.     !  thus to initialise a value far bejond probability is used
        REAL (KIND=R8)  ::  CritDepth       = -11.1D34    !> in practice here negative values may occure
        REAL (KIND=R8)  ::  MinVolume       =  -0.1D0     !  thus to initialise a value far bejond probability is used
        REAL (KIND=R8)  ::  MinVolumeRadius =  -0.1D0
        CHARACTER (128) ::  FieldDump       = '000_aaaaaaaa'
        INTEGER         ::  FieldDumpID     = -1
        REAL (KIND=R8)  ::  DumpVolume      = -0.1D0
        REAL (KIND=R8)  ::  DumpRate        = -0.1D0
        LOGICAL         ::  DumpPlanar      = .False.
        REAL (KIND=R8), ALLOCATABLE
     &                , DIMENSION (:) ::  GrainClass
!        REAL (KIND=R8), ALLOCATABLE
!     &               , DIMENSION (:) ::  HeapClass !> heaps of dug material sorted by grain class
!
        ! internal
!     ============= internals ==========================================
!
        LOGICAL         ::  FirstTimeActive = .TRUE.
!
        INTEGER         ::  State   = -11         !> Status of Action: 0 = not yet     active
                                                  !                    1 = currently   active
                                                  !                    2 = temporary inactive
                                                  !                    9 = for ever  inactive
        INTEGER         ::  nts     = -1234       !> number of time steps that is
                                                  !  needed till the action is finished
        INTEGER         ::  tsCount = -1234       !> count time steps while action is working
!                                                 
        REAL (KIND=R8)  :: sumInput = -123.4567890D0  !> amount of sediment that was carried
                                                      !  through sediment transport into the field.
!                                                    
        REAL (KIND=R8)  :: dt_ts    = -123.4567890D0  !> time      per  time step
        REAL (KIND=R8)  :: dz_ts    = -123.4567890D0  !> evolution per  time step
        REAL (KIND=R8)  :: dz_dt    = -123.4567890D0  !> evolution per  time
        REAL (KIND=R8)  :: dzTot    = -123.4567890D0  !> total evolution
        REAL (KIND=R8), ALLOCATABLE
     &                , DIMENSION (:) ::  dzCL_ts !> evolution per time step per class
!
        REAL (KIND=R8)  :: FillArea      = -123.4567890D0   !> sum of node areas which are to be filled         
                                                            !  while dumping 
!
        INTEGER         :: DumpMode      = -1  !> Mode: 10 = Dump_by_Time
                                               !        11 = Dump_by_Time_Planar
                                               !        20 = Dump_by_Rate
                                               !        21 = Dump_by_Rate_Planar
!
        REAL (KIND=R8)  :: MaxDig_dz_ts  = -123.4567890D0  !> interesting value in case DigPlanar=T
        REAL (KIND=R8)  :: MaxDump_dz_ts = -123.4567890D0  !> interesting value in case DumpPlanar=T
        REAL (KIND=R8)  :: SaveTime      = -123.4567890D0  !> used to save the time
        LOGICAL         :: Solo          = .FALSE.          !> indicates if action is is combined with of other actions
        REAL (KIND=R8)  :: InputDigField = -123.4567890D0   !> amount of sediment that was carried
        REAL (KIND=R8)  :: InputDumpField= -123.4567890D0   !  through sediment transport into the field.
        REAL (KIND=R8)  :: SumDiged      = -123.4567890D0   !> sum of material removed from the field
        REAL (KIND=R8)  :: SumDumped     = -123.4567890D0   !> sum of material dumped into the field
        REAL (KIND=R8)  :: MovedVolume   = -123.4567890D0
!
        INTEGER         :: nNodeToDig    = -1234        !> total number over all partitions
        INTEGER         :: nNodeToDump   = -1234        !> total number over all partitions
!     ============= internals - debug ==================================
!
        REAL (KIND=R8)  :: Time          = -123.4567890D0
!
!
!
      END TYPE t_Action !______________________________________________
!     |                                                                |
!     |                                                                |
!     |________________________________________________________________|
!
!
!***                                              ********************************************
!***                                              ********************************************
      END MODULE   m_TypeDefs_Nestor             !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
!*********************************************************************************************
