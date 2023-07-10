!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Set_Action_Defaults            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( A )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
!
      IMPLICIT NONE
!
      TYPE(t_Action),INTENT(INOUT)   :: A
!
#ifndef  NESTOR_INTERFACES
!
      !------- local variables ---------------
!
!       none local variables
!
!
!      dbug WRITE(6,*)'?>-------  SR Set_Action_Defaults ----------'
!
!
      A%ActionType     = -11
      A%ActionTypeStr  = 'aaaaaaaaaaa'
      A%ActionDateStr  = 'aaaaaaaaaaa'
      A%FieldDig       = '000_aaaaaaaa'
      A%FieldDigID     = -1234
      A%ReferenceLevel = '-1aaaa'!
      A%TimeStart      = -123.4567890D34   !> in practice here negative values may occure
      A%TimeEnd        = -123.4567890D34   !  thus to initialise a value far bejond probability is used
      A%TimeRepeat     = -123.4567890D0
!
      A%DigVolume       = -123.4567890D0
      A%DigRate         = -123.4567890D0
      A%DigDepth        = -123.4567890D34  !> in practice here negative values may occure
      A%DigPlanar       = .False.          !  thus to initialise a value far bejond probability is used
      A%CritDepth       = -123.4567890D34  !> in practice here negative values may occure
      A%MinVolume       = -123.4567890D0   !  thus to initialise a value far bejond probability is used
      A%MinVolumeRadius = -123.4567890D0
      A%FieldDump       = '000_aaaaaaaa'
      A%FieldDumpID     = -1234
      A%DumpVolume      = -123.4567890D0
      A%DumpRate        = -123.4567890D0
      A%DumpPlanar      = .False.
!
!
!     ==============internals ============================================
!
      A%FirstTimeActive = .TRUE.
!
      A%State    = -1234      !> Status of Action: 0 = not yet     active
                              !                    1 = currently   active
                              !                    2 = temporary inactive
                              !                    9 = for ever  inactive
      A%DumpMode = -1234      !> Mode: 10 = Dump_by_Time
                              !        11 = Dump_by_Time_Planar
                              !        20 = Dump_by_Rate
                              !        21 = Dump_by_Rate_Planar
      A%nts      = -1234      !> number of time steps that is
                              !  needed till the action is finished
      A%tsCount  = -1234      !> count time steps while action is working
!
      A%sumInput = -123.4567890D0   !> amount of sediment that was carried
                                    !  through sediment transport into the field.
!
      A%dt_ts    = -123.4567890D0   !> time      per  time step
      A%dz_ts    = -123.4567890D0   !> evolution per  time step
      A%dz_dt    = -123.4567890D0   !> evolution per  time
      A%dzTot    = -123.4567890D0   !> total evolution
!
      A%FillArea     = -123.4567890D0   !> sum of node areas which are to be filled
                                        !  while dumping planar#
!
      A%DumpMode     = -1  !> Mode: 10 = Dump_by_Time
                           !        11 = Dump_by_Time_Planar
                           !        20 = Dump_by_Rate
                           !        21 = Dump_by_Rate_Planar
!
      A%MaxDig_dz_ts  = -123.4567890D0  !> interesting value in case DigPlanar=T
      A%MaxDump_dz_ts = -123.4567890D0  !> interesting value in case DumpPlanar=T
      A%SaveTime      = -123.4567890D0  !> used to save the time
      A%Solo          = .FALSE.          !> indicates if action is is combined with of other actions
      A%InputDigField = -123.4567890D0   !> amount of sediment that was carried
      A%InputDumpField= -123.4567890D0   !  through sediment transport into the field.
      A%SumDiged      = -123.4567890D0   !> sum of material removed from the field
      A%SumDumped     = -123.4567890D0   !> sum of material dumped into the field
      A%MovedVolume   = -123.4567890D0
!
      A%nNodeToDig    = -1234 !> total number over all partitions
      A%nNodeToDump   = -1234 !> total number over all partitions
!
!
!      dbug WRITE(6,*)'?>-------  SR Set_Action_Defaults END ------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Set_Action_Defaults         !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
