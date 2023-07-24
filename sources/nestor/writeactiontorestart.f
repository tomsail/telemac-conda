!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  WriteActionToRestart           !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( time, fileName )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor, ONLY :   A, nActions, nGrainClass, ipid
     &                     , ParallelComputing, ncsize
!
!
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY : open_File
#endif
!
!
      IMPLICIT NONE
!
      REAL (KIND=R8),INTENT(IN)    :: time          !  time [s]
      CHARACTER(128),INTENT(IN)    :: fileName
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      INTEGER :: i, n, fu  ! fu: file unit
!
!
!      dbug WRITE(6,*)'?>-------  SR WriteActionToRestart ---------'
!
      IF( ParallelComputing ) CALL P_SYNC()
!
      IF( ipid == 0 ) THEN
       fu = 451
       !fileName = "_restart_DigActions.dat"
       CALL open_File( fileName, fu, 'w' )        ! open File
!
       ! === Write global variables ====
!                  1234567890123456789012345678
       WRITE(fu,*)'I am the Nestor restart file'               ! 1
       WRITE(fu,*)'nActions             =     ', nActions      ! 3
       WRITE(fu,*)'nGrainClass          =     ', nGrainClass   ! 2
       WRITE(fu,*)'time                 =     ', time          ! 4
       WRITE(fu,*)'ncsize               =     ', ncsize        ! 5
       WRITE(fu,*)'xx                   =     ', -11.10        ! 6
       WRITE(fu,*)'xx                   =     ', -11.10        ! 7
       WRITE(fu,*)'xx                   =     ', -11.10        ! 8
       WRITE(fu,*)'xx                   =     ', -11.10        ! 9
       WRITE(fu,*)'xx                   =     ', -11.10        !10
!
       DO n=1, nActions    ! === Write Actions ====
!                     1234567890123456789012345678
         WRITE(fu,*)'ActionType ++++++++++      ', A(n)%ActionType
        WRITE(fu,'(" ActionTypeStr              ",A)')A(n)%ActionTypeStr
        WRITE(fu,'(" FieldDig                   ",A)') A(n)%FieldDig
         WRITE(fu,*)'FieldDigID                 ', A(n)%FieldDigID
       WRITE(fu,'(" ReferenceLevel             ",A)')A(n)%ReferenceLevel
         WRITE(fu,*)'TimeStart                  ', A(n)%TimeStart
         WRITE(fu,*)'TimeEnd                    ', A(n)%TimeEnd
         WRITE(fu,*)'TimeRepeat                 ', A(n)%TimeRepeat
         WRITE(fu,*)'DigVolume                  ', A(n)%DigVolume
         WRITE(fu,*)'DigRate                    ', A(n)%DigRate
         WRITE(fu,*)'DigDepth                   ', A(n)%DigDepth
         WRITE(fu,*)'DigPlanar                  ', A(n)%DigPlanar
         WRITE(fu,*)'CritDepth                  ', A(n)%CritDepth
         WRITE(fu,*)'MinVolume                  ', A(n)%MinVolume
         WRITE(fu,*)'MinVolumeRadius            ', A(n)%MinVolumeRadius
        WRITE(fu,'(" FieldDump                  ",A)')A(n)%FieldDump
         WRITE(fu,*)'FieldDumpID                ', A(n)%FieldDumpID
         WRITE(fu,*)'DumpVolume                 ', A(n)%DumpVolume
         WRITE(fu,*)'DumpRate                   ', A(n)%DumpRate
         WRITE(fu,*)'DumpPlanar                 ', A(n)%DumpPlanar
        DO i=1, nGrainClass
          WRITE(fu,*)'GrainClass                 ',A(n)%GrainClass(i)
        ENDDO
         WRITE(fu,*)'FirstTimeActive            ', A(n)%FirstTimeActive
         WRITE(fu,*)'State                      ', A(n)%State
         WRITE(fu,*)'nts                        ', A(n)%nts
         WRITE(fu,*)'tsCount                    ', A(n)%tsCount
         WRITE(fu,*)'sumInput                   ', A(n)%sumInput
         WRITE(fu,*)'dt_ts                      ', A(n)%dt_ts
         WRITE(fu,*)'dz_ts                      ', A(n)%dz_ts
         WRITE(fu,*)'dz_dt                      ', A(n)%dz_dt
         WRITE(fu,*)'dzTot                      ', A(n)%dzTot
        DO i=1, nGrainClass
          WRITE(fu,*)'dzCL_ts                    ',A(n)%dzCL_ts(i)
        ENDDO
!
         WRITE(fu,*)'FillArea                   ', A(n)%FillArea       ! new Jan 2018     
         WRITE(fu,*)'DumpMode                   ', A(n)%DumpMode       ! new Jan 2018
         WRITE(fu,*)'MaxDig_dz_ts               ', A(n)%MaxDig_dz_ts   ! new Jan 2018
         WRITE(fu,*)'MaxDump_dz_ts              ', A(n)%MaxDump_dz_ts  ! new Jan 2018
         WRITE(fu,*)'SaveTime                   ', A(n)%SaveTime       ! new Jan 2018
         WRITE(fu,*)'Solo                       ', A(n)%Solo           ! new Jan 2018
         WRITE(fu,*)'InputDigField              ', A(n)%InputDigField
         WRITE(fu,*)'InputDumpField             ', A(n)%InputDumpField
         WRITE(fu,*)'SumDiged                   ', A(n)%SumDiged
         WRITE(fu,*)'SumDumped                  ', A(n)%SumDumped
         WRITE(fu,*)'MovedVolume                ', A(n)%MovedVolume
!
         WRITE(fu,*)'nNodeToDig                 ', A(n)%nNodeToDig
         WRITE(fu,*)'nNodeToDump                ', A(n)%nNodeToDump
!
!
       ENDDO  ! n=1, nActions
       WRITE(fu,*)'A-END  ============================================'
!
       CLOSE(fu)
!
!
      ENDIF !( ipid == 0 )
!
!
!      dbug WRITE(6,*)'?>-------  SR WriteActionToRestart End -----'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE WriteActionToRestart        !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************