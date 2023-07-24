!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dump_by_Time_Planar            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( A, dt_ts, z_sis, zrl_sis, dzCL_sis, time, KNOLG, m )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  F, ParallelComputing, nGrainClass, ipid
     &                      , lim_dzts
      USE INTERFACE_PARALLEL, ONLY : P_DSUM, P_DMAX
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop
     &                               , Dealloc_Dump_Field
     &                               , Calculate_PlanarLevel
     &                               , Set_by_Profiles_Values_for
     &                               , Set_RefLevel_by_Waterlevel
#endif
!
      IMPLICIT NONE
      TYPE(t_Action),INTENT(INOUT) :: A
      REAL (KIND=R8),INTENT(IN)    :: dt_ts        ! time-step-duration  [ s ]
      REAL (KIND=R8),INTENT(IN)    :: z_sis(:)     ! bottom [m+NN] at time (assumed-shape array)
      REAL (KIND=R8),INTENT(IN)    :: zrl_sis(:)    ! reference level  [ m+NN ]
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT) :: dzCL_sis(:)
      REAL (KIND=R8),INTENT(IN)    :: time         !  time [s]
      INTEGER       ,INTENT(IN)    :: KNOLG(*)     ! index list: Local to Global node index
      INTEGER       ,INTENT(IN)    :: m            ! number of Action
#ifndef  NESTOR_INTERFACES
!
      !------- local variables ---------------
      INTEGER            :: i, n, status
      INTEGER            :: iCL, iMesh
      REAL (KIND=R8)     :: maxDump_dz_ts
      REAL (KIND=R8)     :: dumpSum, sumInput_ts
!
      CHARACTER  (16)    :: rCh              ! to store a real value as string
!
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Time_Planar ----------'
      SRname%s = "Dump_by_Time_Planar" ! subroutine name
!
      n = A%FieldDumpID
      
      
!      __________________________________________________________
      !                                                        __|
      !                                                     __|
      IF( A%FirstTimeActive )  THEN  !_____________________|
!
        A%Solo = .FALSE.
        IF( A%ActionTypeStr(1:12) == 'Dump_by_time') THEN !> If Dump_by_Time_Planar is not part
                                                          !  of other actions we have to do 
                                                          !  some additional initialization
          A%Solo    = .TRUE.
          A%State   = 1                                   !> 1 = Action currently active
          A%tsCount = 0
!
          A%nts = INT(   (A%TimeEnd - time) / dt_ts ) !> calculate number of time
                                                      !  steps (nts) to fulfil the Action
          IF( A%nts < 1 ) Call ErrMsgAndStop( " "
     &    ,"reason:  period for this action is too short "
     &    ," ","occured in action number:", m, SRname, ipid ) 
        ENDIF !( A%ActionTypeStr(1:12) == 'Dump_by_time')
!
!
!
        ALLOCATE( F(n)%Z( F(n)%nNodes ), stat=status)
        DO i=1, F(n)%nNodes                 !>  copy value to the Field-Structur
          F(n)%Z(i) = z_sis( F(n)%Node(i) ) !>  F(n)%Node(i)= mesh index of field node
        ENDDO
!
        ALLOCATE( F(n)%dZ( F(n)%nNodes ), stat=status)
        F(n)%dZ(:) = -1234.5D0
!
        ALLOCATE( F(n)%NodeToDump( F(n)%nNodes ), stat=status)
        F(n)%NodeToDump(:) = .FALSE.
!
        ALLOCATE( F(n)%refZ( F(n)%nNodes ), stat=status)
        F(n)%refZ(:) = -1234.5D0   ! value must be -1234.5

        ALLOCATE( F(n)%km( F(n)%nNodes )  , stat=status)
        F(n)%km(:) = -1234.5D0
!
        CALL Set_by_Profiles_Values_for( "refZ", "km", F(n) )     !> the result is F%refZ(:) and F%km(:)
!
        IF(      A%ReferenceLevel(1:8) == 'WATERLVL')  THEN       !> overwrite F%refZ(:)
          CALL Set_RefLevel_by_Waterlevel( F(n), A, m)
        ELSE IF( A%ReferenceLevel(1:8) == 'GRID')      THEN       !> overwrite F%refZ(:)
          F(n)%refZ(:) = zrl_sis( F(n)%Node(:) )
        ENDIF
!
        IF( A%Solo ) THEN
          CALL Calculate_PlanarLevel( F(n),  A%DumpVolume, 1 )!> 1 => dump;  the result is total F%dz(:) and F%NodeToDump(:) 
        ELSE                                                     
          CALL Calculate_PlanarLevel( F(n), -A%DigVolume,  1 )!> 1 => dump;  the result is total F%dz(:)and F%NodeToDump(:) 
                                                              !  A%DigVolume because we want to
                                                              !  dump the volume which is to dig
        ENDIF
!
        !DO i=1, F(n)%nNodes                                           ! debug
        !  IF( F(n)%Node(i) == 415 ) THEN                              ! debug
        ! !IF(F(n)%dZ(i) > 0.0D0 ) THEN                                ! debug
        !    WRITE(6,*)'?> i    =', i                                  ! debug
        !    WRITE(6,*)'?> F(n)%Z  =', F(n)%Z(i)                       ! debug
        !    WRITE(6,*)'?> F(n)%dZ =',F(n)%dZ(i)                       ! debug
        !    WRITE(6,*)'?> sum     =',F(n)%dZ(i) +  F(n)%Z(i)          ! debug
        !    WRITE(6,*)'?>          A%nts =',A%nts                     ! debug
        !    WRITE(6,*)'?>  DBLE( A%nts ) =',DBLE( A%nts )             ! debug
        !    WRITE(6,*)'?> F(n)%dZ per ts =', F(n)%dZ(i)/DBLE( A%nts ) ! debug
        !    !WRITE(6,*)'',                                            ! debug
        !    !WRITE(6,*)'',                                            ! debug
        !    !WRITE(6,*)'',                                            ! debug
        !  ENDIF                                                       ! debug
        !ENDDO                                                         ! debug
        !STOP                                                          ! debug
!
!
        F(n)%dz(:) = F(n)%dz(:) / DBLE( A%nts )  ! convert total F(n)%dz to F(n)%dz per time step
!
        maxDump_dz_ts =  MAXVAL( F(n)%dz )
        IF( ParallelComputing )
     &  maxDump_dz_ts = P_DMAX( maxDump_dz_ts )
!
        IF( maxDump_dz_ts > lim_dzts ) THEN
          WRITE(rCh,'(F16.8)') maxDump_dz_ts  !> convert real value to string and then
          WRITE(rCh,'(16A)') adjustl(rCh)     !  convert string to left-aligned string
          Call ErrMsgAndStop(
     &     "reason: Change of bottom level per time step "
     &    ,"        dzts = "//rCh//"[m] is too big.      "
     &    ,"        lim_dzts = 0.1[m]                    "
     &    ,"occured in Action: ", m, SRname, ipid      )
        ENDIF
!
        A%MaxDump_dz_ts = maxDump_dz_ts
!
        A%InputDumpField  = 0.0D0
        A%MovedVolume     = 0.0D0
!
        IF( A%Solo ) THEN
          A%FirstTimeActive = .FALSE.
          CALL InfoMessage( A, m, time )
        ENDIF
!
!             _____________________________________________
      ENDIF  !(IF A%FirstTimeActive )                      |__
      !                                                       |__
      !__________________________________________________________|
!
      IF( .NOT. A%FirstTimeActive ) THEN
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
            IF( F(n)%NodeToDump(i) ) THEN
              sumInput_ts =   sumInput_ts
     &                      + dzCL_sis(iCL)%R(iMesh) * F(n)%NodeArea(i)
            ENDIF
          ENDDO
        ENDDO
!
        DO iCL=1, nGrainClass  !----- dump one time step -----
          DO i=1, F(n)%nNodes
            iMesh = F(n)%Node(i)     !> iMesh = mesh index of field node
            dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &      + A%GrainClass(iCL) * F(n)%dz(i)
          ENDDO
        ENDDO !-----------------------------------------------
!
!
        dumpSum = 0.0D0
        DO iCL=1, nGrainClass
          DO i=1, F(n)%nNodes
            dumpSum = dumpSum  +  F(n)%dz(i)
     &                         *  A%GrainClass(iCL)
     &                         *  F(n)%NodeArea(i)
          ENDDO
        ENDDO
!
        IF(ParallelComputing) THEN
          dumpSum     = P_DSUM(dumpSum)
          sumInput_ts = P_DSUM(sumInput_ts)
        ENDIF 
        A%InputDumpField = A%InputDumpField + sumInput_ts
        A%DumpVolume     = A%DumpVolume - dumpSum
!
        IF( A%Solo ) THEN
          A%tsCount     = A%tsCount + 1
          A%MovedVolume = A%MovedVolume + dumpSum
          IF( A%tsCount  >=  A%nts ) THEN !> The action is over
            A%State = 9                   !> 9 = Action for ever inactive
            CALL Write_Node_Info( 'dump', A, m, time, KNOLG )
            CALL InfoMessage( A, m, time )
            CALL Dealloc_Dump_Field( A )
          ENDIF
        ENDIF !( A%Solo )
!
!
      ENDIF !( .NOT. A%FirstTimeActive )
!
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Time_Planar END-------'
!
      RETURN
#endif
      END SUBROUTINE Dump_by_Time_Planar         !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

