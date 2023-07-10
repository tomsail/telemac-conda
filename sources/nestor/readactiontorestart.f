!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ReadActionToRestart            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ()
!
      USE m_TypeDefs_Nestor
      USE m_Nestor, ONLY :  A, nActions, nGrainClass
     &                    , ipid , ncsize, LuRstF
!
!
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop
#endif
!
      IMPLICIT NONE
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
      INTEGER         :: i, n, fu      ! fu: file unit
      INTEGER         :: prior_ncsize  ! parallel threads of restart file
      CHARACTER (128) :: zeile, valuePart
      REAL (KIND=R8)  :: time
!
      TYPE(t_String_Length) :: SRname     ! subroutine where the error occured
!
!      dbug WRITE(6,*)'?>-------  SR ReadActionToRestart ----------'
!
      SRname%s = "ReadActionToRestart"
!
!
!     ---- Check if logical unit LuRstF pionts to -------
!          the NESTOR RESTART FILE
      REWIND( LuRstF )
      READ( LuRstF, '(A)') zeile
      IF( zeile /= ' I am the Nestor restart file') Call ErrMsgAndStop(
     &   "while try to read the Nestor restart file        "
     &  ,"reason:  I am reading a wrong file!              "
     &  ,"         Check setting for NESTOR RESTART FILE   "
     &  ,"         in the telemac2d or sisyphe cas-file    "
     &  , -1, SRname, ipid     )
!
      fu = LuRstF  ! LuRstF: Logical Unit ReSTart File
!
      REWIND( fu )
      ! === Read global variables ====
      READ( fu, '(A)') zeile
!      write(6,*) zeile    ! debug                    ! read line  1
      READ( fu, '(A)') zeile
!      write(6,*) zeile    ! debug                    ! read line  2
      valuePart = zeile(25:)
      READ(valuePart,*)        nActions
      READ( fu, '(A)') zeile
!      write(6,*) zeile    ! debug                    ! read line  3
      valuePart = zeile(25:)
      READ(valuePart,*)        nGrainClass
      READ( fu, '(A)') zeile
!      write(6,*) zeile    ! debug                    ! read line  4
      valuePart = zeile(25:)
      READ(valuePart,*)        time
!
      READ( fu, '(A)') zeile
!      write(6,*) zeile    ! debug                    ! read line  5
      valuePart = zeile(25:)
      READ(valuePart,*)        prior_ncsize
!
      READ( fu, '(A)') zeile                    ! read line  6  (dummy line )
      READ( fu, '(A)') zeile                    ! read line  7  (dummy line )
      READ( fu, '(A)') zeile                    ! read line  8  (dummy line )
      READ( fu, '(A)') zeile                    ! read line  9  (dummy line )
      READ( fu, '(A)') zeile                    ! read line 10  (dummy line )#
!
!
      IF( prior_ncsize /= ncsize) THEN
        IF(prior_ncsize == 0) prior_ncsize = 1  ! in the message text looks 1 better than 0 
        Call ErrMsgAndStop(
     &   "while read the Nestor restart file          "
     &  ,"reason:  number of parallel threads differ  "
     &  ,"                                            "
     &  ,"         cpus used for previous computation:"
     &  , prior_ncsize, SRname, ipid     )
      ENDIF
!
!
!
      DO n=1, nActions    ! === read Actions ====
        READ( fu, '(A)') zeile
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%ActionType
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%ActionTypeStr
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%FieldDig
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%FieldDigID
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%ReferenceLevel
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%TimeStart
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%TimeEnd
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%TimeRepeat
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%DigVolume
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%DigRate
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%DigDepth
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%DigPlanar
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%CritDepth
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%MinVolume
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%MinVolumeRadius
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%FieldDump
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%FieldDumpID
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%DumpVolume
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%DumpRate
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%DumpPlanar
        DO i=1, nGrainClass
          READ( fu, '(A)') zeile
!         write(6,*) zeile    ! debug
          valuePart = zeile(25:)
          READ(valuePart,*)      A(n)%GrainClass(i)
        ENDDO
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%FirstTimeActive
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%State
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%nts
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%tsCount
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%sumInput
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%dt_ts
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%dz_ts
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%dz_dt
        READ( fu, '(A)') zeile
!       write(6,*) zeile    ! debug
        valuePart = zeile(25:)
        READ(valuePart,*)       A(n)%dzTot
        DO i=1, nGrainClass
          READ( fu, '(A)') zeile
!         write(6,*) zeile    ! debug
          valuePart = zeile(25:)
          READ(valuePart,*)     A(n)%dzCL_ts(i)
        ENDDO
!
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%FillArea       ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%DumpMode       ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%MaxDig_dz_ts   ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%MaxDump_dz_ts  ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%SaveTime       ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%Solo           ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%InputDigField  ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%InputDumpField ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%SumDiged       ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%SumDumped      ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%MovedVolume    ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%nNodeToDig     ! Jan2018
        READ( fu, '(A)') zeile                       ! Jan2018
!       write(6,*) zeile    ! debug                  ! Jan2018
        valuePart = zeile(25:)                       ! Jan2018
        READ(valuePart,*)        A(n)%nNodeToDump    ! Jan2018
!
      ENDDO  ! n=1, nActions
!
!      WRITE(6,*)'?>  time = ',time
!
!      dbug WRITE(6,*)'?>-------  SR ReadActionToRestart End ------'
!
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE ReadActionToRestart         !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
