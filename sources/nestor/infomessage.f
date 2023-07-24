!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  InfoMessage                    !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( A, m, time )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  nGrainClass, Restart, Lu, F
!
!
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  my_FLUSH
#endif
!
      IMPLICIT NONE
      TYPE(t_Action),INTENT(INOUT) :: A
!
      INTEGER       ,INTENT(IN)    :: m         !> index of action
      REAL (KIND=R8),INTENT(IN)    :: time      !> time [ s ]
!
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
      INTEGER       :: iCL, U
      CHARACTER (7) :: c_sta              ! to store a real value as string
!
!
      U = Lu   ! just to have a short variable name for the standard output
!
!      dbug WRITE(U,*)'?>-------  SR InfoMessage ------------------'
!
!
670   FORMAT( 1(' ?> info:'),61('='), '+' )
672   FORMAT( 1(' ?> info:'),61(' '), '|' )
673   FORMAT( 1(' ?> info:'),12(' '),'NESTOR',43(' '),'|' )
!
!
!     CALL my_FLUSH(6)
      CALL my_FLUSH(U)
!
      c_sta = "  start"
      IF(Restart) c_sta = "restart"
!
!
      WRITE(U,670)
      WRITE(U,672)
      WRITE(U,673)
      WRITE(U,672)
      WRITE(U,*)'?>                 '
!
      IF(Restart) WRITE(U,*)'?>          R e s t a r t'
!
      SELECT CASE( A%ActionType )
!
      CASE(   1   )  ! _________Dig_by_time______________________|
!
      IF( A%State == 1 ) THEN                                          ! 1 = Action currently active)
       WRITE(U,'(" ?>          action number    : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>        ",A," action     : ",A)')c_sta
     &                                                , A%ActionTypeStr
       WRITE(U,*)'?>  nominal start time  [s]  : ',A%TimeStart
       WRITE(U,*)'?>          start time  [s]  : ',time
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>          FieldDig         : ",A)')A%FieldDig
       WRITE(U,*)'?>          area     [m**2]  : ',F(A%FieldDigId)%Area
       WRITE(U,*)'?>     dz per time step [m]  : ',-A%MaxDig_dz_ts
       IF(A%FieldDumpID >= 0 ) THEN
         WRITE(U,*)'?>'
         WRITE(U,'(" ?>          FieldDump        : ",A)')A%FieldDump
         WRITE(U,*)'?>          area     [m**2]  : '
     &                                         ,F(A%FieldDumpId)%Area
         IF(A%DumpPlanar)THEN
           WRITE(U,*)'?> max dz per time step [m]  : ',A%MaxDump_dz_ts
         ELSE
           WRITE(U,*)'?>     dz per time step [m]  : ',A%MaxDump_dz_ts
         ENDIF
       ENDIF
      ENDIF
!
      IF( A%State == 9 ) THEN                                          ! 9 = for ever inactive
       WRITE(U,'(" ?>          action number    : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>          end action       : ",A)')A%ActionTypeStr
       WRITE(U,*)'?>  nominal start time   [s] : ',A%TimeStart
       WRITE(U,*)'?>  nomi. dig end time   [s] : ',A%TimeEnd
       WRITE(U,*)'?>        dig end time   [s] : ',A%SaveTime
       WRITE(U,'(" ?>          FieldDig         : ",A)')A%FieldDig
       WRITE(U,*)'?>'
       WRITE(U,*)'?> sediment that was carried   '
       WRITE(U,*)'?> through sediment transport  '
       WRITE(U,*)'?> into the active dig nodes   '
       WRITE(U,*)'?> while digging      [m**3] : ',A%InputDigField
       IF(A%FieldDumpID >= 0 ) THEN
        WRITE(U,*)'?>'
        WRITE(U,'(" ?>          FieldDump        : ",A)')A%FieldDump
       !WRITE(U,*)'?> end of dumping  time  [s] : ',time
        WRITE(U,*)'?>     dump end time     [s] : ',time
        WRITE(U,*)'?>   relocated volume [m**3] : ',A%MovedVolume
        WRITE(U,*)'?>                             '
        WRITE(U,*)'?> sediment that was carried   '
        WRITE(U,*)'?> through sediment transport  '
        WRITE(U,*)'?> into the active dump nodes  '
        WRITE(U,*)'?> while dumping      [m**3] : ',A%InputDumpField
       ELSE
        WRITE(U,*)'?>   removed volume   [m**3] : ',A%DumpVolume
        DO iCL=1, nGrainClass
         WRITE(U,*)'?>           GrainClass      : ',A%GrainClass(iCL)
        ENDDO
       ENDIF
      ENDIF !( A%State == 9 )
!
      CASE(   2   )  ! _________Dump_by_time_____________________|
!
      IF( A%State == 1 ) THEN                                          ! 1 = Action currently active)
       WRITE(U,'(" ?>        action number      : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>        ",A," action     : ",A)')c_sta
     &                                                , A%ActionTypeStr
       WRITE(U,*)'?>  nominal start time  [s]  : ',A%TimeStart
       WRITE(U,*)'?>          start time  [s]  : ',time
       WRITE(U,'(" ?>        FieldDump          : ",A)')A%FieldDump
       WRITE(U,*)'?>          area     [m**2]  : ',F(A%FieldDumpId)%Area
       IF( A%DumpPlanar ) THEN
        WRITE(U,*)'?> max dz per time step [m]  : ',A%MaxDump_dz_ts
       ELSE
        WRITE(U,*)'?>     dz per time step [m]  : ',A%MaxDump_dz_ts
       ENDIF
      ENDIF  !( A%State == 1)
!
      IF( A%State == 9 ) THEN                                          ! 9 = for ever inactive
       WRITE(U,'(" ?>        action number      : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>        end action         : ",A)')A%ActionTypeStr
       WRITE(U,*)'?>  nominal start time  [s]  : ',A%TimeStart
       WRITE(U,*)'?>  nominal   end time  [s]  : ',A%TimeEnd
       WRITE(U,*)'?>            end time  [s]  : ',time
       WRITE(U,'(" ?>        FieldDump          : ",A)')A%FieldDump
       IF( A%DumpPlanar ) THEN
        WRITE(U,*)'?>        DumpVolume [m**3]  : ',A%MovedVolume
       ELSE
        WRITE(U,*)'?> preset DumpVolume [m**3]  : ',A%DumpVolume
       ENDIF
       WRITE(U,*)'?>                           '
       WRITE(U,*)'?> sediment that was carried '
       WRITE(U,*)'?> through sediment transport'
!       WRITE(U,*)'?> into the field while      '
       WRITE(U,*)'?> into the active dump nodes'
       WRITE(U,*)'?> while dumping     [m**3]  : ',A%InputDumpField
      ENDIF  !( A%State == 9 )
!
!
!
      CASE(   3   )  ! _________Dig_by_criterion_________________|
!
      IF( A%State == 1 ) THEN                                          ! 1 = Action currently active)
       WRITE(U,'(" ?>          action number    : ",I3)') m
       WRITE(U,*)'?>'
      !WRITE(U,'(" ?>          start action     : ",A)')A%ActionTypeStr
       WRITE(U,'(" ?>        ",A," action     : ",A)')c_sta
     &                                                , A%ActionTypeStr
       WRITE(U,*)'?>  nominal start time  [s]  : ',A%TimeStart
       WRITE(U,*)'?>          start time  [s]  : ',time
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>          FieldDig         : ",A)')A%FieldDig
       WRITE(U,*)'?>          area      [m^2]  : ',F(A%FieldDigId)%Area
       WRITE(U,*)'?> max dz per time step [m]  : ', -A%MaxDig_dz_ts
       WRITE(U,*)'?>'
       IF(A%FieldDumpID > 0) THEN
       WRITE(U,'(" ?>          FieldDump        : ",A)')A%FieldDump
       WRITE(U,*)'?>          area      [m^2]  : ',F(A%FieldDumpId)%Area
       IF( A%DumpPlanar ) THEN
        WRITE(U,*)'?> max dz per time step [m]  : ',A%MaxDump_dz_ts
       ELSE
        WRITE(U,*)'?>     dz per time step [m]  : ',A%MaxDump_dz_ts
       ENDIF
       ENDIF !(A%FieldDumpID > 0)
!
      ELSEIF ( A%State == 2 ) THEN
       WRITE(U,'(" ?>          action number    : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>          finalise action    ")')
       WRITE(U,'(" ?>          temporarily      : ",A)')A%ActionTypeStr
       WRITE(U,'(" ?>          FieldDig         : ",A)')A%FieldDig
       IF(A%FieldDumpID > 0)
     & WRITE(U,'(" ?>          FieldDump        : ",A)')A%FieldDump
       WRITE(U,*)'?>                           '
       WRITE(U,*)'?>        maitenance period    '
       WRITE(U,*)'?>  nominal start time  [s]  : ',A%TimeStart
       WRITE(U,*)'?>                time  [s]  : ',time
       WRITE(U,*)'?>                           '
       WRITE(U,*)'?>        during the last      '
       WRITE(U,*)'?>        maitenance period    '
       WRITE(U,*)'?>        dug volume  [m^3]  : ',A%SumDiged !A%MovedVolume
       IF(A%FieldDumpID > 0) THEN
       WRITE(U,*)'?>                             '
       WRITE(U,*)'?>        dumped vol  [m^3]  : ',A%SumDumped
       WRITE(U,*)'?>                             '
       ENDIF
       WRITE(U,*)'?> sediment that was carried   '
       WRITE(U,*)'?> through sediment transport  '
       WRITE(U,*)'?> into the active dig nodes   '
       WRITE(U,*)'?> while digging      [m^3]  : ',A%InputDigField
       IF(A%FieldDumpID > 0) THEN
       WRITE(U,*)'?>                             '
       WRITE(U,*)'?> sediment that was carried   '
       WRITE(U,*)'?> through sediment transport  '
       WRITE(U,*)'?> into the active dump nodes  '
       WRITE(U,*)'?> while dumping      [m^3]  : ',A%InputDumpField
       ENDIF
!
      ELSEIF ( A%State == 9 ) THEN
       WRITE(U,'(" ?>          action number    : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>          end action       : ",A)')A%ActionTypeStr
       WRITE(U,'(" ?>          FieldDig         : ",A)')A%FieldDig
       IF(A%FieldDumpID > 0)
     & WRITE(U,'(" ?>          FieldDump        : ",A)')A%FieldDump
       WRITE(U,*)'?>                             '
       WRITE(U,*)'?>        maitenance period    '
       WRITE(U,*)'?>  nominal start time  [s]  : ',A%TimeStart
       WRITE(U,*)'?>                time  [s]  : ',time
       WRITE(U,*)'?>                             '
       WRITE(U,*)'?>        during the last      '
       WRITE(U,*)'?>        maitenance period    '
       WRITE(U,*)'?>        dug volume  [m^3]  : ',A%SumDiged !A%MovedVolume
       IF(A%FieldDumpID > 0) THEN
       WRITE(U,*)'?>                             '
       WRITE(U,*)'?>        dumped vol  [m^3]  : ',A%SumDumped
       WRITE(U,*)'?>                             '
       ENDIF
       WRITE(U,*)'?> sediment that was carried   '
       WRITE(U,*)'?> through sediment transport  '
       WRITE(U,*)'?> into the active dig nodes   '
       WRITE(U,*)'?> while digging      [m^3]  : ',A%InputDigField
       IF(A%FieldDumpID > 0) THEN
       WRITE(U,*)'?>                             '
       WRITE(U,*)'?> sediment that was carried   '
       WRITE(U,*)'?> through sediment transport  '
       WRITE(U,*)'?> into the active dump nodes  '
       WRITE(U,*)'?> while dumping      [m^3]  : ',A%InputDumpField
       ENDIF
!
       WRITE(U,*)'?>                             '
       WRITE(U,*)'?>  nominal end time  [s]    : ',A%TimeEnd
!
      ENDIF
!
      CASE(   4   )  ! _________Reset_bottom_____________________|
!
      IF( A%State == 1 ) THEN                                          ! 1 = Action currently active)
       WRITE(U,'(" ?>          action number    : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>        ",A," action     : ",A)')c_sta
     &                                                , A%ActionTypeStr
       WRITE(U,*)'?>  nominal start time  [s]  : ',A%TimeStart
       WRITE(U,*)'?>          start time  [s]  : ',time
       WRITE(U,'(" ?>          FieldDump        : ",A)')A%FieldDump
       WRITE(U,*)'?>          area      [m^2]  : ',F(A%FieldDumpId)%Area
      ENDIF
!
      IF( A%State == 9 ) THEN                                          ! 9 = for ever inactive
       WRITE(U,'(" ?>          action number    : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>          end action       : ",A)')A%ActionTypeStr
       WRITE(U,*)'?>  nominal end time  [s]    : ',A%TimeEnd
       WRITE(U,*)'?>          end time  [s]    : ',time
       WRITE(U,'(" ?>          FieldDump        : ",A)')A%FieldDump
      ENDIF
!
      CASE(   5   )  ! _________Save_water_level________________|
!
       WRITE(U,'(" ?>          action number    : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>          start action     : ",A)')A%ActionTypeStr
       WRITE(U,'(" ?>          reference Level  : ",A)')A%ReferenceLevel
       WRITE(U,*)'?>  nominal start time  [s]  : ',A%TimeStart
       WRITE(U,*)'?>          start time  [s]  : ',time
!
      CASE(   6   )  ! _________Backfill_to_level_______________|
!
      IF( A%State == 1 ) THEN                                          ! 1 = Action currently active)
       WRITE(U,'(" ?>          action number    : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>        ",A," action     : ",A)')c_sta
     &                                                , A%ActionTypeStr
       WRITE(U,*)'?>  nominal start time  [s]  : ',A%TimeStart
       WRITE(U,*)'?>          start time  [s]  : ',time
       WRITE(U,'(" ?>          FieldDump        : ",A)')A%FieldDump
       WRITE(U,*)'?>          area      [m^2]  : ',F(A%FieldDumpId)%Area
       WRITE(U,*)'?> max dz per time step [m]  : ',A%MaxDump_dz_ts

      ENDIF
!
      IF( A%State == 9 ) THEN                                          ! 9 = for ever inactive
       WRITE(U,'(" ?>          action number    : ",I3)') m
       WRITE(U,*)'?>'
       WRITE(U,'(" ?>          end action       : ",A)')A%ActionTypeStr
       WRITE(U,*)'?>  nominal end time  [s]    : ',A%TimeEnd
       WRITE(U,*)'?>          end time  [s]    : ',time
       WRITE(U,'(" ?>          FieldDump        : ",A)')A%FieldDump
      ENDIF
!
!
!
      CASE DEFAULT  ! ___________________________________________|
!
      END SELECT
      WRITE(U,*)'?>                    '
      WRITE(U,672)
      WRITE(U,672)
      WRITE(U,672)
      WRITE(U,670)
!
!     CALL my_FLUSH(6)
      CALL my_FLUSH(U)
!
!      IF( ParallelComputing ) CALL P_SYNC()
!
!
!      dbug WRITE(U,*)'?>-------  SR InfoMessage End --------------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE InfoMessage                 !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
