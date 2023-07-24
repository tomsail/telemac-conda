!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  IsActionCompletelyDefined      !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( A, m )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor, ONLY : ipid
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop
#endif
!
      IMPLICIT NONE
!
      TYPE(t_Action), INTENT(IN) :: A   ! one element of array A
      INTEGER, INTENT(IN)        :: m   ! index to define element of array A
!
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      INTEGER                    :: sL
      CHARACTER (128)            :: str
      TYPE(t_String_Length)      :: SRname    ! name of current Subroutine
!
!      dbug WRITE(6,*)'?>-------  SR IsActionCompletelyDefined ----'
      SRname%s = "IsActionCompletelyDefined"     ! subroutine name
!
      str = "#####"
      SELECT CASE( A%ActionType ) !=====================================
       CASE( 1 )    !------------- Dig_by_time ------------------------
          IF( A%TimeEnd      < -10.0D32)THEN
            str = "TimeEnd"
            GOTO 123
          ENDIF
          IF( A%TimeStart    < -10.0D32)THEN
            str = "TimeStart"
            GOTO 123
          ENDIF
          IF( A%FieldDigID   < 0       )THEN
            str = "FieldDig"               ! FieldDumpID is an internal value
            GOTO 123
          ENDIF
          IF( A%DigVolume    < 0.0D0   )THEN
            str = "DigVolume"
            GOTO 123
          ENDIF
          IF(      A%DigPlanar                               !> only if DumpPlanar = true
     &       .AND. A%ReferenceLevel(1:6) == "-1aaaa" )THEN   !> we need a type of reference level
            str = "ReferenceLevel"                           !  the default name string = "-1aaaa"
            GOTO 123
          ENDIF
!
          IF( A%FieldDumpID  > 0  )THEN  ! dumping is ordered
            IF(      A%DumpPlanar                              !> only if DumpPlanar = true
     &         .AND. A%ReferenceLevel(1:6) == "-1aaaa" )THEN   !> we need a type of reference level
              str = "ReferenceLevel"                           !  the default name string = "-1aaaa"
              GOTO 123
            ENDIF
          ENDIF   ! dumping is ordered  in this context
!
!
!
!
       CASE( 2 )    !------------- Dump_by_time ------------------------
          IF( A%TimeEnd      < -10.0D32)THEN
            str = "TimeEnd"
            GOTO 123
          ENDIF
          IF( A%TimeStart    < -10.0D32)THEN
            str = "TimeStart"
            GOTO 123
          ENDIF
          IF( A%FieldDumpID  < 0       )THEN
            str = "FieldDump"               ! FieldDumpID is an internal value
            GOTO 123
          ENDIF
          IF( A%DumpVolume   < 0.0D0   )THEN
            str = "DumpVolume"
            GOTO 123
          ENDIF
          
          IF( A%DumpRate     > 0.0D0  ) THEN  
            Call ErrMsgAndStop( "while read the Action file   "
     &      ,"         reason: DumpRate is not allowed here ! "
     &      ,"                 Remove the Keyword !           "
     &      ,"occured in Action: ", m, SRname, ipid      )
          ENDIF
          IF(      A%DumpPlanar                              !> only if DumpPlanar = true
     &       .AND. A%ReferenceLevel(1:6) == "-1aaaa" )THEN   !> we need a type of reference level
            str = "ReferenceLevel"                           !  the default name string = "-1aaaa"
            GOTO 123
          ENDIF
!
       CASE( 3 )    !------------- Dig_by_criterion --------------------
          IF( A%TimeEnd      < -10.0D32)THEN
            str = "TimeEnd"
            GOTO 123
          ENDIF
          IF( A%TimeStart    < -10.0D32)THEN
            str = "TimeStart"
            GOTO 123
          ENDIF
          IF( A%TimeRepeat   <   0.00D0)THEN
            str = "TimeRepeat"
            GOTO 123
          ENDIF
          IF( A%FieldDigID   <   0     )THEN
            str = "FieldDig"                ! FieldDigID is an internal value
            GOTO 123
          ENDIF
          IF( A%DigRate      <   0.0D0 )THEN
            str = "DigRate"
            GOTO 123
          ENDIF
          IF( A%CritDepth     < -10000.0D0 )THEN
            str = "CritDepth"
            GOTO 123
          ENDIF
          IF( A%DigDepth     < -10000.0D0 )THEN
            str = "DigDepth"
            GOTO 123
          ENDIF
          IF( A%MinVolume    <   0.0D0 )THEN
            str = "MinVolume"
            GOTO 123
          ENDIF
          IF( A%MinVolumeRadius  <  0.0D0 )THEN
            str = "MinVolumeRadius"
            GOTO 123
          ENDIF
          IF( A%ReferenceLevel(1:6) == "-1aaaa"   )THEN      !> we need a type of reference level
            str = "ReferenceLevel"                           !  the default name string = "-1aaaa"
            GOTO 123
          ENDIF
          IF(       A%FieldDumpID  > 0                 !> the dug material will be dumped
     &       .AND.  A%DumpRate     < 0.0D0 )THEN
            str = "DumpRate"
            GOTO 123
          ENDIF
!
       CASE( 4 )    !------------- Reset_bottom ------------------------
          IF( A%TimeEnd      < -10.0D32)THEN
            str = "TimeEnd"
            GOTO 123
          ENDIF
          IF( A%TimeStart    < -10.0D32)THEN
            str = "TimeStart"
            GOTO 123
          ENDIF
          IF( A%FieldDumpID  < 0       )THEN
            str = "FieldDump"               ! FieldDumpID is an internal value
            GOTO 123
          ENDIF
         !IF( A%DumpRate     < 0.0D0   )THEN
         !  str = "DumpRate"
         !  GOTO 123
         !ENDIF
!
       CASE( 5 )    !------------- Save_water_level ---------------------
          IF( A%TimeStart    < -10.0D32)THEN
            str = "TimeStart"
            GOTO 123
          ENDIF
          IF( A%ReferenceLevel(1:6) == "-1aaaa"   )THEN      !> we need a type of reference level
            str = "ReferenceLevel"                           !  the default name string = "-1aaaa"
            GOTO 123
          ENDIF
          !IF( A%TimeEnd      < -10.0D32)THEN
          !  str = "TimeEnd"
          !  GOTO 123
          !ENDIF
!
       CASE( 6 )    !------------- Backfill_to_level -------------------
          IF( A%ReferenceLevel(1:6) == "-1aaaa"   )THEN      !> we need a type of reference level
            str = "ReferenceLevel"                           !  the default name string = "-1aaaa"
            GOTO 123
          ENDIF
          IF( A%TimeEnd      < -10.0D32)THEN
            str = "TimeEnd"
            GOTO 123
          ENDIF
          IF( A%TimeStart    < -10.0D32)THEN
            str = "TimeStart"
            GOTO 123
          ENDIF
          IF( A%FieldDumpID  < 0       )THEN
            str = "FieldDump"               ! FieldDumpID is an internal value
            GOTO 123
          ENDIF
          IF( A%CritDepth     < -10000.0D0 )THEN
            str = "CritDepth"
            GOTO 123
          ENDIF
         !IF( A%DumpRate     < 0.0D0   )THEN
         !  str = "DumpRate"
         !  GOTO 123
         !ENDIF
!
!
!
!
        CASE DEFAULT !------------- No ActionType -----------------------
          str = "ActionType"
          GOTO 123
!
      END SELECT    !===================================================
!
!
  123 CONTINUE   !
      IF( str(1:1) /= "#" ) THEN  ! value of str is no more "#####"  ==> error occured
        sL  = LEN_TRIM(str)
        Call ErrMsgAndStop( "while read the Action file        "
     &  ,"         reason: An Action is not completely defined "
     &  ,"                 missing proper values at: "//str(:sL)
     &  ,"occured in Action: ", m, SRname, ipid      )
      ENDIF
!
!      dbug WRITE(6,*)'?>-------  SR IsActionCompletelyDefined End '
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE IsActionCompletelyDefined   !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
