!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  MainNestor                     !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (   ts, dt_ts_sis, time, ES1_sis, z_sis
     &   , AVAIL, KNOLG, dzCL_sis, h_sis, zr_sis, zrl_sis )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      ________________________________________________________________
!     |                                                                |
!     |        we assume that the sisyphe time step is constant  !     |
!     |_______________ ________________________________________________|
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor
!
!
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  Dump_by_Time
     &                               , Dump_by_Time_Planar
     &                               , Dig_by_Time
     &                               , Dig_by_Criterion
     &                               , ReadWriteRestart
     &                               , Reset_bottom
     &                               , Backfill_to_level
     &                               , InfoMessage
#endif
!
      IMPLICIT NONE
!
      INTEGER      , INTENT(IN)    :: ts            !  time-step
      REAL (KIND=R8),INTENT(IN)    :: dt_ts_sis     !  time-step-duration  [ s ]
      REAL (KIND=R8),INTENT(IN)    :: time          !  time [s]
      REAL (KIND=R8),INTENT(IN)    :: ES1_sis(:)    ! (non const.) thickness of active laver  [m]
      REAL (KIND=R8),INTENT(IN)    :: z_sis(:)      !  bottom [ m+NN ]  (assumed-shape array)
      REAL (KIND=R8),INTENT(IN)    :: AVAIL(:,:,:)  !  assumed-shape array
      INTEGER       ,INTENT(IN)    :: KNOLG(*)      ! index list: Local to Global node index
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT) :: dzCL_Sis(:)   !  bedload evolution per Class  [ m ]
      REAL (KIND=R8),INTENT(IN)
     &          ,DIMENSION (NPOIN) :: h_sis(:)      ! water depth (at time AT0) [ m ]
      REAL (KIND=R8),INTENT(IN)
     &          ,DIMENSION (NPOIN) :: zr_sis(:)     ! ridged bed  [ m+NN ]
      REAL (KIND=R8),INTENT(IN)
     &          ,DIMENSION (NPOIN) :: zrl_sis(:)    ! reference level  [ m+NN ]
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      TYPE(t_String_Length) :: SRname  ! name of current Subroutine
!
      INTEGER              :: m
      REAL (KIND=R8)       :: dt_ts    ! time-step-duration respecting MorpholFactor   [ s ]
!
!
!
!      dbug WRITE(6,*)'?>-------  SR MainNestor ------------------'
      SRname%s = "MainNestor"         ! subroutine name
!
!
      !WRITE(6,*)'?> time_sis      = ', time          ! debug test
      !WRITE(6,*)'?> npoin         = ', npoin         ! debug test
      !WRITE(6,*)'?>  z_sis(npoin) = ',z_sis(npoin)   ! debug test
      !WRITE(6,*)'?>  z_sis(1)     = ',z_sis(1)       ! debug test
      !WRITE(6,*)'?>  z_sis(638)   = ',z_sis(638)     ! debug test
      !WRITE(6,*)'?>       -5.9782670102672562'       ! debug test
      !STOP
!
      IF( Restart ) THEN
        CALL ReadWriteRestart( time, 'read    ' )
        DO m=1, nActions
          IF( A(m)%State == 1 ) THEN
            CALL InfoMessage( A(m), m, time )
          ENDIF
        ENDDO
        Restart = .FALSE.
      ENDIF
!
      lim_dzts = 0.1D0   ! [m]  max allowed change of bottom level (dz) 
!                        !      per time step (ts)
!
      A(:)%Time = time            ! debug
!
!
      DO m=1, nActions
        !WRITE(6,'(" ?>   state of Action",I3," =",I3)') m, A(m)%State   !debug
!
        IF( A(m)%State == 9 ) CYCLE   !> Status of Action:  0 = not yet      active
                                      !                     1 = currently    active
                                      !                     2 = temporary  inactive
                                      !                     9 = for ever   inactive
!
        IF(       time >= A(m)%TimeStart )THEN !> In case we dump with a small DumpRate it's possible
!        IF(       time >= A(m)%TimeStart      !  that the dumping takes more time as given by
!     &      .AND. time <= A(m)%TimeEnd ) THEN !  TimeEnd - TimeStart.  The condition
                                               !  ".AND. time <= A(m)%TimeEnd )" may stop an action
                                               !  although dumping not completed.
!
!
          dt_ts = dt_ts_sis / MorpholFactor
!
          SELECT CASE( A(m)%ActionType )
!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   1   )  ! _________Dig_by_time______________________|
              !WRITE(6,*)'?> CASE 1   Dig_by_time'
!
              CALL Dig_by_Time(   A(m)
     &                          , F( A(m)%FieldDigID )
     &                          , dt_ts, z_sis, zr_sis, zrl_sis
     &                          , dzCL_sis, AVAIL, ES1_sis
     &                          , time, KNOLG, m
     &                         )
!
!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   2   )  ! _________Dump_by_time_____________________|
              !WRITE(6,*)'?> CASE 2   Dump_by_time'
!
              IF( A(m)%DumpPlanar )  THEN
                CALL Dump_by_Time_Planar(   A(m)
     &                                    , dt_ts, z_sis, zrl_sis
     &                                    , dzCL_sis, time, KNOLG, m
     &                                   )
              ELSE
                CALL Dump_by_Time(   A(m)
     &                             , dt_ts, dzCL_sis
     &                             , time, KNOLG, m
     &                            )
              ENDIF
!
!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   3   )  ! _________Dig_by_criterion_________________|
              !WRITE(6,*)'?> CASE 3   Dig_by_criterion'
!
              CALL Dig_by_Criterion(   A(m)
     &                               , F( A(m)%FieldDigID )
     &                               , dt_ts, z_sis, zr_sis, zrl_sis
     &                               , dzCL_sis, AVAIL, ES1_sis
     &                               , time, KNOLG, m
     &                              )
!
!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   4   )  ! _________Reset_bottom_____________________|
              !WRITE(6,*)'?> CASE 4   Reset_bottom'
!
              CALL Reset_bottom(   A(m)
     &                           , F( A(m)%FieldDumpID )
     &                           , dt_ts, z_sis
     &                           , dzCL_sis, time, m
     &                          )
!
!
!
!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   5   )  ! _________Save_water_level_________________|
              !WRITE(6,*)'?> CASE 5   Save_water_level'
!
              IF(     A(m)%ReferenceLevel(1:9)=='WATERLVL1')THEN
                IF( .NOT. ALLOCATED(waterLevel_saved_1) ) THEN
                  ALLOCATE( waterLevel_saved_1(npoin) )
                ENDIF
                waterLevel_saved_1(:) = h_sis(:) + z_sis(:)
              ELSE IF(A(m)%ReferenceLevel(1:9)=='WATERLVL2')THEN
                IF( .NOT. ALLOCATED(waterLevel_saved_2) ) THEN
                  ALLOCATE( waterLevel_saved_2(npoin) )
                ENDIF
                waterLevel_saved_2(:) = h_sis(:) + z_sis(:)
              ELSE IF(A(m)%ReferenceLevel(1:9)=='WATERLVL3')THEN
                IF( .NOT. ALLOCATED(waterLevel_saved_3) ) THEN
                  ALLOCATE( waterLevel_saved_3(npoin) )
                ENDIF
                waterLevel_saved_3(:) = h_sis(:) + z_sis(:)
              ENDIF
!
              CALL InfoMessage( A(m), m, time )
!
              A(m)%State = 9            !>   9 = for ever inactive
!
!
!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   6   )  ! _________Backfill_to_level________________|
              !WRITE(6,*)'?> CASE 5   Backfill_to_level'
!
              CALL Backfill_to_level(   A(m)
     &                                , F( A(m)%FieldDumpID )
     &                                , dt_ts, z_sis, zrl_sis
     &                                , dzCL_sis
     &                                , time, KNOLG, m
     &                               )
!
!
!
!            __________________________________________________________
!           |                                                          |
            CASE DEFAULT  ! ___________________________________________|
              !WRITE(6,*)'?>  CASE DEFault erreicht '
!
          END SELECT
!
        ENDIF !A(m)%TimeStart <= time <= A(m)%TimeEnd
      ENDDO !m=1, nActions
!
      IF( MOD(ts,GraphicOutputPeriod) == 0 )
     &  CALL ReadWriteRestart( time, 'write   ' )
!
!
!
!      dbug WRITE(6,*)'?>-------  SR MainNestor End --------------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE MainNestor                  !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

