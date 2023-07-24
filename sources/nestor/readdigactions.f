!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ReadDigActions                 !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ()
!
      USE m_TypeDefs_Nestor
!
      USE m_Nestor, ONLY :  A, nActions, ipid, ParallelComputing
     &                     , nGrainClass, eps, Restart, LuActF
     &                     , called_by_t2d
!
!
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop
     &                               , ParseSteerLine
     &                               , Set_Action_Defaults
     &                               , IsActionCompletelyDefined
     &                               , Write_Action_Visualisation
#endif
!
      IMPLICIT NONE
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      INTEGER         :: i,j, m, lineCount, countClasses
      INTEGER         :: iMaxVal
      INTEGER         :: stat = 0
      INTEGER         :: status
      CHARACTER (128) :: line
!
      REAL (KIND=R8)  :: sumGrainCL
      REAL (KIND=R8)  :: DateStringToSeconds  ! function
      EXTERNAL        :: DateStringToSeconds  ! function
      LOGICAL         :: ThreeDigitsNumeral   ! function
      EXTERNAL        :: ThreeDigitsNumeral   ! function
      CHARACTER  (16) :: rCh              ! to store a real value as string
      CHARACTER (128) :: str              ! to store a string
!
      TYPE(t_String_Length) :: SRname     ! subroutine where the error occured
      INTEGER,ALLOCATABLE,DIMENSION (:) :: FieldID
      LOGICAL          :: FieldIDisDouble
      LOGICAL          :: GrCLNumErr, GrCLSumErr
      LOGICAL          :: PassedKeywordENDACTION
      LOGICAL          :: PassedKeywordACTION
      LOGICAL          :: PassedKeywordRESTART
      LOGICAL          :: wlevel_set
!
      CHARACTER (128) :: KeyWord
      CHARACTER (128) :: valueStr
      INTEGER         :: Le
!
      LOGICAL,ALLOCATABLE,DIMENSION (:) ::  Flag
!
!      dbug WRITE(6,*)'?>-------  SR ReadDigActions ---------------'
      SRname%s = "ReadDigActions"
!
      REWIND LuActF    ! LuActF  = Logical Unit ACTion File
!
      PassedKeywordRESTART   = .FALSE.
      PassedKeywordACTION    = .FALSE.
      PassedKeywordENDACTION = .TRUE.
      lineCount = 0
      nActions  = 0
!
      ALLOCATE( Flag( nGrainClass ), stat=status)
!
      DO   !  loop to read the dig action file to detect the number of Actions
        lineCount = lineCount + 1
        READ( LuActF, '(A)', IOSTAT = stat ) line
        IF( stat /= 0 ) EXIT
        line = ADJUSTL(line)      !      "   blabla "
                                  !  --> "blabla    "
        IF(line(1:1) == ''  ) CYCLE
        IF(line(1:1) == '/' ) CYCLE
        IF(line(1:7) == 'ENDFILE') EXIT
!
        IF(line(1:6) == 'ACTION' ) THEN
          IF( PassedKeywordENDACTION .EQV. .FALSE.) EXIT  ! error message is called
                                                          ! after this do-loop
          nActions = nActions + 1
          PassedKeywordACTION    = .TRUE.
          PassedKeywordENDACTION = .FALSE.
        ENDIF
!
        IF(line(1:9) == 'ENDACTION') THEN
          IF( PassedKeywordACTION .EQV. .FALSE.)Call ErrMsgAndStop(
     &     "while read the Action file             "
     &    ,"reason:  missing keyword   ACTION      " ," "
     &    ,"occured in line: " , lineCount, SRname, ipid         )
!
          PassedKeywordACTION    = .FALSE.
          PassedKeywordENDACTION = .TRUE.
        ENDIF
!
      ENDDO !  loop to read the dig action file to detect the number of Actions
!
      IF( PassedKeywordENDACTION .EQV. .FALSE.)THEN
        Call ErrMsgAndStop( "while read the Action file "
     &  ,"reason:  missing keyword   ENDACTION  " ," "
     &  ,"occured in line: " , lineCount, SRname, ipid     )
      ENDIF
!
!
      ALLOCATE( A( nActions ), stat=status)
      DO m=1, nActions
        CALL Set_Action_Defaults( A(m) )
        ALLOCATE( A(m)%GrainClass(nGrainClass), stat=status)
        A(m)%GrainClass(:) = -11.1D0  ! initialise array
        ALLOCATE( A(m)%dzCL_ts(nGrainClass), stat=status)
        A(m)%dzCL_ts(:)    = -11.1D0  ! initialise array  dz per  grainCLass   per  TimeStep
      ENDDO
!
      REWIND LuActF
      m = 0
      lineCount = 0
      DO   ! loopt to read the file containing the dig actions
        READ( LuActF, '(A)', IOSTAT = stat ) line
        !WRITE(*,*) TRIM(line)           !debug
        !CALL FLUSH(6)                    !debug
        IF( stat /= 0 .OR. line(1:7) == 'ENDFILE') EXIT
        lineCount = lineCount + 1
        line = ADJUSTL(line)                   !  "   blabla "  --> "blabla    "
        IF( line(1:1) == ''  ) CYCLE
        IF( line(1:1) == '/' ) CYCLE
        IF( line(1:6) == 'ACTION' ) THEN
          m = m + 1   ! from here a new Action block begins
          countClasses = 0
          CYCLE
        ENDIF
!
!
!
        IF( line(1:9) == 'ENDACTION' ) THEN  !>  do some checks
!
          IF(      called_by_t2d
     &       .AND. (      A(m)%ActionType == 4           !> 4: Reset_bottom
     &               .OR. A(m)%ActionType == 6 )  ) THEN !> 6: Backfill_to_level
            countClasses       = 1                       !> If coupled with t2d there is no need of settings
            A(m)%GrainClass(1) = 1.D0                    !  for GrainClass in the action file.
          ENDIF                                          !  To suppress the error message we do settings here.
!
!
          CALL IsActionCompletelyDefined( A(m), m )
!
!
          GrCLNumErr = .FALSE.       !>  Grain Class Number Error
          GrCLSumErr = .FALSE.       !>  Grain Class Sum of all classes Error
          IF(     A(m)%ActionType == 2                               !> 2: Dump_by_time
     &       .OR. A(m)%ActionType == 6 ) THEN                        !> 6: Backfill_to_level
!
            IF( countClasses /= nGrainClass )   GrCLNumErr = .True.  !> check number of grain classes in the Action
!
            sumGrainCL = SUM( A(m)%GrainClass(:) )
            IF( ABS(sumGrainCL - 1.0D0) > eps ) GrCLSumErr = .True.  !> check sum of grain classes  it must be 1
!
            !> To assure that the sum of the grain classes = 1
            !  we recalculate the biggest class(iMaxVal) dependent
            !  on all the other classes.
            iMaxVal = MAXLOC( A(m)%GrainClass(:), 1 )    ! index of element with max. value
            Flag(:)       = .TRUE.
            Flag(iMaxVal) = .FALSE.
            A(m)%GrainClass(iMaxVal) = 1.D0 - SUM( A(m)%GrainClass(:)    ! recalculate class(iMaxVal)
     &                                                 , MASK=Flag(:) )
!
          ENDIF   ! Dump_by_time or Backfill_to_level
!
          IF( GrCLNumErr) THEN  !> check user input relating number of  grain classes
            WRITE(rCh,'(I4)') m                !> convert integer value to string and then
            WRITE(rCh,'(16A)') adjustl(rCh)    !  convert string to left-aligned string
            Call ErrMsgAndStop( "while read the Action file     "
     &      ,"         reason: Wrong number of grain classes was"
     &      ,"                 defined in Action    "//rCh(1:4)
     &      ,"                 It must be  ",nGrainClass,SRname,ipid)
          ENDIF
!
          IF( GrCLSumErr ) THEN
            WRITE(rCh,'(F16.8)') sumGrainCL    !> convert real value to string and then
            WRITE(rCh,'(16A)') adjustl(rCh)    !  convert string to left-aligned string
            Call ErrMsgAndStop( "while read the Action file        "
     &,"reason: sum(grain classes) = "//rCh//" but it must be 1.0 !"
     &,"        In case dummy values are used it must be negative !"
     &,"occured in Action: ", m, SRname, ipid      )
          ENDIF
!
          CYCLE
        ENDIF        ! line(1:9) == 'ENDACTION'   do some checks
!
!
        CALL ParseSteerLine(line, KeyWord, valueStr)
        !   WRITE(6,*) '-------lineCount = ',lineCount                      !debug
        !   WRITE(6,*) 'KeyWord = >', KeyWord(1:LEN_TRIM(KeyWord)),'<'      !debug
        !   WRITE(6,*) 'valueStr = >', valueStr(1:LEN_TRIM(valueStr)),'<'   !debug
!
        Le = LEN_TRIM(KeyWord)
!
        SELECT CASE(  KeyWord(1:Le)  )
          CASE( "ActionType" )
            IF(valueStr(1:17) == 'Backfill_to_level') THEN
              A(m)%ActionType = 6
            ENDIF
            IF(valueStr(1:16) == 'Save_water_level') THEN
              A(m)%ActionType = 5
            ENDIF
            IF(valueStr(1:12) == 'Reset_bottom') THEN
              A(m)%ActionType = 4
              IF( .NOT. called_by_t2d ) THEN
                Call ErrMsgAndStop( "while read the Action file     "    !  to the demand format
     &          ,"         reason: Use Reset_bottom ONLY for pure   "
     &          ,"                 hydrodynamic computation !       "
     &          ,"occured in line: ", lineCount, SRname, ipid       )
              ENDIF
            ENDIF
!
            IF(valueStr(1:16) == 'Dig_by_criterion') THEN
              A(m)%ActionType = 3
            ENDIF
            IF(valueStr(1:12) == 'Dump_by_time') THEN
              A(m)%ActionType = 2
            ENDIF
            IF(valueStr(1:11) == 'Dig_by_time') THEN
              A(m)%ActionType = 1
            ENDIF
            IF(A(m)%ActionType == -11) THEN    !>  -11 (=default value from initialisation)
              stat = -1                        !   then throw error message
            ENDIF
            READ(valueStr,*) A(m)%ActionTypeStr
!
          CASE( "FieldDig" )
            READ(valueStr,*) A(m)%FieldDig
            ! check if Field name conforms to the demand format
            IF( .NOT. ThreeDigitsNumeral(A(m)%FieldDig(1:3))) THEN     !> check if Field name conforms
              Call ErrMsgAndStop( "while read the Action file     "    !  to the demand format
     &        ,"         reason: FieldDig-name must have at posi- "
     &        ,"                 tion 1-3 numerals like: 123_aName"
     &        ,"occured in line: ", lineCount, SRname, ipid       )
            ENDIF
            READ( valueStr,'(I3)',IOSTAT=stat) A(m)%FieldDigID   ! read the first string elements as integer
!
          CASE( "ReferenceLevel" )
            READ(valueStr,*) A(m)%ReferenceLevel
            IF(.NOT.(     valueStr(1:2) == 'NO'
     &               .OR. valueStr(1:4) == 'GRID'
     &               .OR. valueStr(1:8) == 'SECTIONS'
!     &               .OR. valueStr(1:9) == 'SECTIONS1'
!     &               .OR. valueStr(1:9) == 'SECTIONS2'
!     &               .OR. valueStr(1:9) == 'SECTIONS3'
!     &               .OR. valueStr(1:9) == 'SECTIONS4'
!     &               .OR. valueStr(1:9) == 'SECTIONS5'
!     &               .OR. valueStr(1:9) == 'SECTIONS6'
!     &               .OR. valueStr(1:9) == 'SECTIONS7'
!     &               .OR. valueStr(1:9) == 'SECTIONS8'
!     &               .OR. valueStr(1:9) == 'SECTIONS9'
     &               .OR. valueStr(1:9) == 'WATERLVL1'
     &               .OR. valueStr(1:9) == 'WATERLVL2'
     &               .OR. valueStr(1:9) == 'WATERLVL3')) THEN
              Call ErrMsgAndStop( "while read the Action file     "
     &        ,"reason: here is a keyword expected, it must be:   "
     &        ,"NO or GRID or SECTIONS or WATERLVL1;...;WATERLVL3 "
     &        ,"occured in line: ", lineCount, SRname, ipid       )
            ENDIF
!
!
!
!
          CASE( "TimeStart" )
            A(m)%TimeStart = DateStringToSeconds(valueStr,lineCount)
          CASE( "TimeEnd" )
           IF(A(m)%ActionType /= 5) THEN  ! at Save_water_level TimeEnd is not set
            A(m)%TimeEnd   = DateStringToSeconds(valueStr, lineCount)
            IF( A(m)%TimeEnd <= A(m)%TimeStart ) THEN
              Call ErrMsgAndStop( "while read the Action file     "
     &        ,"         reason:  TimeEnd is before TimeStart     "
     &        ,"                                                  "
     &        ,"occured in line: ", lineCount, SRname, ipid      )
            ENDIF
           ENDIF
          CASE( "TimeRepeat" )
            READ(valueStr,*)  A(m)%TimeRepeat
          CASE( "DigVolume" )
            READ(valueStr,*)  A(m)%DigVolume
          CASE( "DigRate" )
            READ(valueStr,*,IOSTAT=stat)  A(m)%DigRate
          CASE( "DigDepth" )
            READ(valueStr,*,IOSTAT=stat)  A(m)%DigDepth
          CASE( "DigPlanar" )
            READ(valueStr,*,IOSTAT=stat)  A(m)%DigPlanar
            IF( A(m)%DigPlanar ) THEN
              Call ErrMsgAndStop( "while read the Action file     "
     &        ,"         reason: the method DigPlanar is not      "
     &        ,"                 supported in this version        "
     &        ,"occured in line: ", lineCount, SRname, ipid      )
            ENDIF
          CASE( "CritDepth" )
            READ(valueStr,*,IOSTAT=stat)  A(m)%CritDepth
          CASE( "MinVolume" )
            READ(valueStr,*,IOSTAT=stat)  A(m)%MinVolume
          CASE( "MinVolumeRadius" )
            READ(valueStr,*,IOSTAT=stat)  A(m)%MinVolumeRadius
          CASE( "FieldDump" )
            READ(valueStr,*)  A(m)%FieldDump
            IF( .NOT. ThreeDigitsNumeral(A(m)%FieldDump(1:3))) THEN    !> check if Field name conforms
              Call ErrMsgAndStop( "while read the Action file     "    !  to the demand format
     &        ,"         reason: FieldDump-name must have at posi-"
     &        ,"                 tion 1-3 numerals like: 123_aName"
     &        ,"occured in line: ", lineCount, SRname, ipid      )
            ENDIF
            READ( valueStr,'(I3)',IOSTAT=stat) A(m)%FieldDumpID   ! read the first string elements as integer
          CASE( "DumpRate" )
            READ(valueStr,*,IOSTAT=stat)  A(m)%DumpRate
          CASE( "DumpPlanar" )
            READ(valueStr,*,IOSTAT=stat)  A(m)%DumpPlanar
          CASE( "DumpVolume" )
            READ(valueStr,*,IOSTAT=stat)  A(m)%DumpVolume
          CASE( "GrainClass" )
            countClasses  =  countClasses + 1
            IF(countClasses <= nGrainClass ) THEN      !  prevent severe memory access
              READ(valueStr,*,IOSTAT=stat) A(m)%GrainClass(countClasses)
            ENDIF
!
          CASE( "RESTART" )
            READ(valueStr,*,IOSTAT=stat)  Restart
            PassedKeywordRESTART   = .TRUE.
!
          CASE DEFAULT
            Call ErrMsgAndStop( "while read the Action file "
     &      ,"reason:  unknown keyword:      "//KeyWord(1:Le)
     &      ,"         check spelling !                     "
     &      ,"occured in line: ", lineCount, SRname, ipid      )
        END SELECT
!
        IF( stat /= 0 ) Call ErrMsgAndStop(
     &   "while read the Action file                    "
     &  ,"reason:  bad value for:        "//KeyWord(1:Le)
     &  ,"              value is:        "//valueStr
     &  ,"occured in line: " , lineCount, SRname, ipid      )
!
      ENDDO   ! loop to read the file
!
      IF( ParallelComputing ) CALL P_SYNC()
!
!
      !----------------------------------------------------------------+
      !>    Check if grain classes are defined correctly               |
      !                                                                |
      DO m=1, nActions
!
        sumGrainCL = SUM( A(m)%GrainClass(:) )
        IF(      sumGrainCL              > 0.0D0             !> If sum of grain classes is
     &     .AND. ABS(sumGrainCL - 1.0D0) > eps   ) THEN      !  bigger than 0.0 and not 1
          WRITE(rCh,'(F16.8)') sumGrainCL    !> convert real value to string and then
          WRITE(rCh,'(16A)') adjustl(rCh)    !  convert string to left-aligned string
          Call ErrMsgAndStop( "while read the Action file              "
     &    ,"reason: sum(grain classes) = "//rCh//" but it must be 1.0 !"
     &    ,"        In case dummy values are used it must be negative !"
     &    ,"occured in Action: ", m, SRname, ipid      )
        ENDIF
!
        IF( sumGrainCL < 0.0D0 ) CYCLE !> ok a negative value indicates
                                       !  that dummy values are used. Thus 
                                       !  we don't recalculate the classes
!
        !> To assure that the sum of the grain classes = 1
        !  we recalculate the biggest class(iMaxVal) dependent
        !  on all the other classes.
        iMaxVal = MAXLOC( A(m)%GrainClass(:), 1 )    ! index of element with max. value
        Flag(:)       = .TRUE.
        Flag(iMaxVal) = .FALSE.
        A(m)%GrainClass(iMaxVal) = 1.D0 - SUM( A(m)%GrainClass(:)    ! recalculate class(iMaxVal)
     &                                            ,  MASK=Flag(:) )
!
      ENDDO
      !                                                                |
      !----------------------------------------------------------------+
!
      !----------------------------------------------------------------+
      !>    Check if in the Action file a Field is                     |
      !     referenced more than once                                  |
      !                                                                |
      ALLOCATE( FieldID( 2 * nActions ), stat=status)
      i = 0
      DO m=1, nActions   !> fill the array FieldID with
        i = i + 1        !  all FieldDigIDs and FieldDumpIDs
        FieldID(i) = A(m)%FieldDigID
        i = i + 1
        FieldID(i) = A(m)%FieldDumpID
      ENDDO
!
      FieldIDisDouble = .FALSE.                !> check if a FieldID occures
      outerloop:DO i=1, 2*nActions - 1         !  more than once in the array
                  IF( FieldID(i) <= 0 ) CYCLE
                  DO j=i+1, 2*nActions
                    IF( FieldID(j) <= 0 ) CYCLE
                    IF( FieldID(j) == FieldID(i) ) THEN
                      FieldIDisDouble = .TRUE.
                      EXIT outerloop
                    ENDIF
                  ENDDO
                ENDDO outerloop
!
      IF( FieldIDisDouble ) THEN        ! throw error message
        WRITE(str,'(I3.3)') FieldID(j)  ! example: FieldID = 1  ==> str = "001"
            !> If you want to operate with more than one Action
            !  on one Field, comment out the following CAll.
            !  BUT BE SHURE THE ACTIONS DO NOT OVERLAP IN TIME !
        CAll ErrMsgAndStop( "while read the Action file             "
     &  ,"reason: Field "//str(:3)//"... is refenced repeatedly     "
     &  ,"        To identify a Field only the three numerals at the"
     &  ,"        begin of the Field name are decisive.             "
     &  ,-1, SRname, ipid )     
      ENDIF
      DEALLOCATE( FieldID )
      !                                                                |
      !----------------------------------------------------------------+
!
!
       IF( .NOT. PassedKeywordRESTART ) THEN
         Call ErrMsgAndStop( "while read the Action file"," "
     &   ,"reason: missing the KeyWord     RESTART      "," "
     &   , -1, SRname, ipid )
       ENDIF
!
       CALL Write_Action_Visualisation
!
!
      !----------------------------------------------------------------+
      !>    Check if a reference level is defined as a water level     |
      !     which is not defined before it is used                     |
      !                                                                |
      DO m=1, nActions
        wlevel_set = .FALSE.
        IF( A(m)%ActionType /= 5 ) THEN                             !not Save_water_level
!
          IF( A(m)%Referencelevel(1:9) == 'WATERLVL1') THEN
            DO i=1, nActions
              IF(      A(i)%ActionType == 5                         !type 5: Save_water_level
     &           .AND. A(i)%Referencelevel(1:9) == 'WATERLVL1') THEN
                IF(A(i)%TimeStart < A(m)%TimeStart) wlevel_set = .TRUE.
              ENDIF
            ENDDO
            IF( .NOT. wlevel_set ) GOTO 123
          ENDIF ! if WATERLVL1
!
          IF( A(m)%Referencelevel(1:9) == 'WATERLVL2') THEN
            DO i=1, nActions
              IF(      A(i)%ActionType == 5                         !type 5: Save_water_level
     &           .AND. A(i)%Referencelevel(1:9) == 'WATERLVL2') THEN
                IF(A(i)%TimeStart < A(m)%TimeStart) wlevel_set = .TRUE.
              ENDIF
            ENDDO
            IF( .NOT. wlevel_set ) GOTO 123
          ENDIF ! if WATERLVL2
!
          IF( A(m)%Referencelevel(1:9) == 'WATERLVL3') THEN
            DO i=1, nActions
              IF(      A(i)%ActionType == 5                         !type 5: Save_water_level
     &           .AND. A(i)%Referencelevel(1:9) == 'WATERLVL3') THEN
                IF(A(i)%TimeStart < A(m)%TimeStart) wlevel_set = .TRUE.
              ENDIF
            ENDDO
            IF( .NOT. wlevel_set ) GOTO 123
          ENDIF ! if WATERLVL3
!
        ENDIF ! ( A(m)%ActionType /= 5 )
      ENDDO
!
      wlevel_set = .TRUE.     !> 07.05.2018 Korrektur: sonst geht er in allen 
                              !  Faellen die oben nicht geprueft wurden in 
                              !  ErrMsgAndStop
  123 CONTINUE   !
      IF( .NOT. wlevel_set )   Call ErrMsgAndStop(
     & "reason: An action asks for a water level as reference level. "
     &,"        You must run the action Save_water_level before      "
     &,"        to set the values of the reference level.            "
     &,"        See action No."
     &, m, SRname, ipid )
      !                                                                |
      !----------------------------------------------------------------+
!
      DEALLOCATE( Flag )
!
!      dbug WRITE(6,*)'?>-------  SR ReadDigActions End -----------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE ReadDigActions              !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
