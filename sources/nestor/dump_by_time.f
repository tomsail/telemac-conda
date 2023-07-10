!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dump_by_Time                   !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &(   A, dt_ts, dzCL_sis, time, KNOLG, m )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  F, ParallelComputing, nGrainClass, ipid
     &                      , lim_dzts
      USE INTERFACE_PARALLEL, ONLY : P_DSUM, P_DMAX
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  InfoMessage
     &                               , ErrMsgAndStop
     &                               , Write_Node_Info
     &                               , Dealloc_Dump_Field
#endif
!
      IMPLICIT NONE
      TYPE(t_Action),INTENT(INOUT) :: A            !> Action
      REAL (KIND=R8),INTENT(IN)    :: dt_ts        !> time-step-duration  [ s ]
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT) :: dzCL_sis(:)  !> bedload evolution per Class  [ m ]
      REAL (KIND=R8),INTENT(IN)    :: time         !> time [ s ]
      INTEGER       ,INTENT(IN)    :: KNOLG(*)     ! index list: Local to Global node index
      INTEGER       ,INTENT(IN)    :: m            !> number of Action
!
#ifndef  NESTOR_INTERFACES
!
      !------- local variables ---------------
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
      INTEGER               :: i, n, iCL, iMesh !, nodeIndex
      REAL (KIND=R8)        :: sumInput_ts
      REAL (KIND=R8)        :: areaRatio
      CHARACTER  (16)       :: rCh              ! to store a real value as string
!
!
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Time -----------------'
      SRname%s = "Dump_by_Time"        !> subroutine name
!
      n = A%FieldDumpID
!
      !___________________________________________________________
      !                                                          _|
      !                                                        _|
      IF( A%FirstTimeActive )  THEN  !________________________|
!
        A%Solo = .FALSE.
        IF( A%ActionTypeStr(1:12) == 'Dump_by_time') THEN !> If Dump_by_Time is not part
                                                          !  of other actions we have to do
                                                          !  some aditional initialisation
          A%Solo    = .TRUE.
          A%State   = 1                                   !> 1 = Action currently active
          A%tsCount = 0
!
          A%nts = INT(   (A%TimeEnd - time) / dt_ts ) !> calculate number of time
                                                      !  steps (nts) to fulfil the Action
          IF( A%nts < 1 ) Call ErrMsgAndStop( " "
     &    ,"reason:  period for this action is too short "
     &    ," ","occured in action number:", m, SRname, ipid )
!
          A%dzTot      = A%DumpVolume / F(n)%Area     !> change of z to fulfil the Action
          A%dz_ts      = A%dzTot / DBLE( A%nts )      !> change of z per time step
          A%dzCL_ts(:) = A%dz_ts * A%GrainClass(:)    !> change of z per time step per
                                                      !  Grain CLass
          A%MaxDump_dz_ts = A%dz_ts
!
        ELSE  ! dumping is part of digging action
                                              !> We dig and dump each time step the same volume.
          areaRatio =   F(A%FieldDigID)%Area  !  Thus applying to the evolution per time step (dz_ts): 
     &                / F(A%FieldDumpID)%Area !      dump_dz_ts = areaRatio * dig_dz_ts
!
          A%MaxDump_dz_ts = -A%dz_ts * areaRatio !> Here dumping is part of digging
                                                 !  thus A%dz_ts is given by the digging.
!
        ENDIF !( A%ActionTypeStr(1:12) == 'Dump_by_time')
!
        IF( A%MaxDump_dz_ts > lim_dzts ) THEN
          WRITE(rCh,'(F16.8)') A%MaxDump_dz_ts !> convert real value to string and then
          WRITE(rCh,'(16A)') adjustl(rCh)      !  convert string to left-aligned string
          Call ErrMsgAndStop(
     &     "reason: Change of bottom level per time step "
     &    ,"        dzts = "//rCh//"[m] is too big.      "
     &    ,"        lim_dzts = 0.1[m]                    "
     &    ,"occured in Action: ", m, SRname, ipid      )
        ENDIF
!
        A%InputDumpField  = 0.0D0
!
!
        IF( A%Solo ) THEN
          A%FirstTimeActive = .FALSE.
          CALL InfoMessage( A, m, time )
        ENDIF
!                                  ___________________________
      ENDIF  !( A%FirstTimeActive )                           |_
      !                                                         |_
      !___________________________________________________________|
!
      IF( .NOT. A%FirstTimeActive ) THEN
!
        IF( .NOT. A%Solo ) THEN               !> We dig and dump each time step the same volume.
          areaRatio =   F(A%FieldDigID)%Area  !  Thus applying to the evolution per time step (dz_ts): 
     &                / F(A%FieldDumpID)%Area !  dump_dz_ts = areaRatio * dig_dz_ts
!
          A%dzCL_ts(:) = -A%dz_ts * A%GrainClass(:) * areaRatio !> Change of z per time step per Grain CLass.
                                                                !  Here dumping is part of dig action
        ENDIF                                                   !  thus A%dz_ts is given by the digging.
!
        IF( A%tsCount <=  A%nts ) THEN
!
        !> Before dumping we calc. the amount of sediment that was
        !  transported by morphodynamic during the last time step
        !  into or out of the dump nodes WHILE THEY WERE ACTIVE.
        !  I case there is a further action operating at the same time
        !  on this field and it is carried out already (depends on the
        !  internal order of execution), then it will
        !  appear here as sumInput_ts too.
        sumInput_ts = 0.0D0
        DO iCL=1, nGrainClass       !  Only nodes below the planar
          DO i=1, F(n)%nNodes       !  level (NodeToDump is TRUE) are included for it.
            iMesh = F(n)%Node(i)    !> mesh index of field node
           !IF( F(n)%NodeToDump(i) ) THEN
              sumInput_ts =   sumInput_ts
     &                      + dzCL_sis(iCL)%R(iMesh) * F(n)%NodeArea(i)
           !ENDIF
          ENDDO
        ENDDO
!
          !----  dump it -------------------------------
          !countDump = countDump + 1                            ! debug
          !WRITE(6,*) 'countDump = ',countDump                  ! debug
          DO iCL=1, nGrainClass
            DO i=1, F(n)%nNodes
              iMesh = F(n)%Node(i)     ! mesh index of field node
              dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &                                 + A%dzCL_ts(iCL) 
            ENDDO
          ENDDO !----------------------------------------
          !mySum = mySum + A%dz_ts                           ! debug
        ENDIF !( A%tsCount <=  A%nts )
!
        !IF( A%tsCount ==  A%nts ) THEN                      ! debug
        !  WRITE(6,*) 'preset dump   volume= ', A%DumpVolume ! debug
        !  WRITE(6,*) '       dumped volume= ', mySum*F%Area ! debug
        !ENDIF                                               ! debug
!
        IF(ParallelComputing) THEN
          sumInput_ts = P_DSUM(sumInput_ts)
        ENDIF 
        A%InputDumpField = A%InputDumpField + sumInput_ts
!
        IF( A%Solo ) THEN
          A%tsCount = A%tsCount + 1
          IF( A%tsCount  >=  A%nts ) THEN !> The action is over
            A%State = 9                   !> 9 = Action for ever inactive
            CALL Write_Node_Info( 'dump', A, m, time, KNOLG )
            CALL InfoMessage( A, m, time )
            CALL Dealloc_Dump_Field( A )
          !IF(ParallelComputing) A%sumInput = P_DSUM(A%sumInput)
          ENDIF
        ENDIF !( A%Solo )
!
      ENDIF !( .NOT. A%FirstTimeActive )
!
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Time END -------------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Dump_by_Time                !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
