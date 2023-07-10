!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dump_by_Rate_Planar            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( A, dt_ts, z_sis, zrl_sis, dzCL_sis, time, KNOLG, m )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  F, ParallelComputing, nGrainClass
      USE INTERFACE_PARALLEL, ONLY : P_DSUM, P_ISUM, P_DMAX
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  Calculate_PlanarLevel
     &                               , Set_by_Profiles_Values_for
     &                               , Set_RefLevel_by_Waterlevel
#endif
!     set nNTDu to 0
      IMPLICIT NONE
!
      TYPE(t_Action),INTENT(INOUT) :: A
      REAL (KIND=R8),INTENT(IN)    :: dt_ts        ! time-step-duration  [ s ]
      REAL (KIND=R8),INTENT(IN)    :: z_sis(:)     ! bottom [m+NN] at time (assumed-shape array)
      REAL (KIND=R8),INTENT(IN)    :: zrl_sis(:)    ! reference level  [ m+NN ]
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT) :: dzCL_sis(:)
      REAL (KIND=R8),INTENT(IN)    :: time         ! time [s]
      INTEGER       ,INTENT(IN)    :: KNOLG(*)     ! index list: Local to Global node
      INTEGER       ,INTENT(IN)    :: m            ! number of Action

#ifndef NESTOR_INTERFACES
!
      !------- local variables ---------------
      INTEGER            :: i, iCL, iMesh, n, nodeIndex, status
      INTEGER            :: nLessNodesToDump
!
      REAL (KIND=R8)     :: dz_ts, dzts
      REAL (KIND=R8)     :: sumDump_ts, sumInput_ts
!
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Rate_Planar ----------'
      SRname%s = "Dump_by_Rate_Planar" ! subroutine name
!
9119  FORMAT(A5,1X,4(G18.11,1X),I8,1X,F8.3,1X,2(G15.8,1X),A16,I4)
!
!
      n = A%FieldDumpID
      !_____________________________________________________________
      !                                                           __|
      !                                  Dump_by_Rate_Planar   __|
      IF( A%FirstTimeActive )  THEN  !________________________|
!
        ALLOCATE( F(n)%Z( F(n)%nNodes ), stat=status)
        F(n)%Z(:) = z_sis( F(n)%Node(:) ) !>  F(n)%Node(i)= mesh index of field node
!
        ALLOCATE( F(n)%dZ( F(n)%nNodes ), stat=status)
        F(n)%dz(:) = 1234.567890D0
!
        ALLOCATE( F(n)%dZ_Tot( F(n)%nNodes ), stat=status)
        F(n)%dz_Tot(:) = 1234.567890D0
!
        ALLOCATE( F(n)%NodeToDump( F(n)%nNodes ), stat=status)
        F(n)%NodeToDump(:) = .FALSE.
!
        ALLOCATE( F(n)%refZ( F(n)%nNodes ), stat=status)
        F(n)%refZ(:) = -1234.5D0   ! value must be -1234.5
!
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
        CALL Calculate_PlanarLevel( F(n), -A%DigVolume, 1 ) !> 1 => dump; The result is
                                                            !  total F%dz(:) and F%NodeToDump(:).
                                                            !  We use A%DigVolume because we
                                                            !  want to dump the DigVolume.
                                                            !  DigVolume is based upon the current shape
                                                            !  of the bottom.
                                                            !  BUT if you Dig_by_criterion the volume which will
                                                            !  be dug and dumped in the end, will be changed
                                                            !  due to morphodynamic processes while digging.
        F(n)%dz_Tot(:) = F(n)%dz(:)
!
        F(n)%nNodeToDump = COUNT( F(n)%NodeToDump(:) )
!
        A%MaxDump_dz_ts = A%DumpRate * dt_ts
!
        A%fillArea = SUM( F(n)%NodeArea(:), MASK=F(n)%NodeToDump(:) )
!
        IF(ParallelComputing) THEN
          A%fillArea       = P_DSUM( A%fillArea )
          F(n)%nNodeToDump = P_ISUM( F(n)%nNodeToDump )
        ENDIF
!
        A%SumDumped      = 0.0D0
        A%InputDumpField = 0.0D0
!
        !IF( A%Solo ) A%FirstTimeActive = .FALSE. is set by the calling dig action (Dump_by_Rate_Planar is never solo)
!
!             ________________________________________________
      ENDIF  ! FirstTimeActive                                |__
      !                                Dump_by_Rate_Planar       |__
      !_____________________________________________________________|
!
!
!
!
      IF( .NOT. A%FirstTimeActive )  THEN
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
!
        !> --- calc hight to dump during current time step ----
        dz_ts = dt_ts * A%DumpRate
        IF( A%DumpVolume  <  dz_ts * A%fillArea ) THEN  !> if the available DumpVolume
           dz_ts = A%DumpVolume / A%fillArea            !  is little, the dump rate must be
        ENDIF                                           !  reduced to a suitable value
!
        dzts             = dz_ts
        sumDump_ts       = 0.0D0
        nLessNodesToDump = 0
!
        DO i=1, F(n)%nNodes  !----- dump one time step -----------------
!
          IF( .NOT. F(n)%NodeToDump(i) ) CYCLE
!
          iMesh = F(n)%Node(i)     !> mesh index of field node
          dz_ts = dzts
!
!          IF( i==12 )then
!           WRITE(*,*)' ?>W1 iMesh = ', iMesh
!           WRITE(*,*)' ?>W1         dz_ts =',dz_ts                ! debug
!           WRITE(*,*)' ?>W1       F(n)%dz =',F(n)%dz(i)           ! debug
!           WRITE(*,*)' ?>W1          diff =',F(n)%dz(i) - dz_ts   ! debug
!           WRITE(*,*)' ?>W1         Tdu12 =',F(n)%NodeToDump(i)   ! debug
!          endif                                                   ! debug
!
          IF( F(n)%dz(i) <   dz_ts ) THEN
            dz_ts = F(n)%dz(i)
            F(n)%NodeToDump(i) = .FALSE.
            nLessNodesToDump = nLessNodesToDump + 1
            !---- write output -------------
            nodeIndex = iMesh
            IF( ParallelComputing )  nodeIndex = KNOLG(iMesh)
            WRITE(6,9119)    !> Write the action concerning data of this node to the log-file
     &      'XdumX', A%TimeStart, time
     &      ,F(n)%dz_Tot(i)                      !> total dz of this node
     &      ,F(n)%dz_Tot(i) * F(n)%NodeArea(i)   !> total dumped volume of this node
     &      ,nodeIndex, F(n)%km(i),F(n)%X(i),F(n)%Y(i),F(n)%Name, m
          ENDIF
!
          DO iCL=1, nGrainClass          !< dumping happens here
            dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &                               + A%GrainClass(iCL) * dz_ts
          ENDDO
!
          F(n)%dz(i) = F(n)%dz(i) - dz_ts
!
          sumDump_ts = sumDump_ts + dz_ts * F(n)%NodeArea(i)
!
        ENDDO !-- i=1, F(n)%nNodes--------------------------------------
!
!
!
        A%fillArea = SUM( F(n)%NodeArea(:), MASK=F(n)%NodeToDump(:) )
!
        IF(ParallelComputing) THEN
          A%fillArea       = P_DSUM( A%fillArea )
          sumDump_ts       = P_DSUM( sumDump_ts )
          nLessNodesToDump = P_ISUM( nLessNodesToDump )
          sumInput_ts      = P_DSUM( sumInput_ts )
        ENDIF
!
        A%InputDumpField = A%InputDumpField + sumInput_ts
        A%SumDumped      = A%SumDumped + sumDump_ts
!
        F(n)%nNodeToDump = F(n)%nNodeToDump - nLessNodesToDump
!
!
        A%DumpVolume = A%DumpVolume - sumDump_ts
!
!
        IF(       ABS( A%DumpVolume ) <  1.0D-32
     &      .AND. A%nNodeToDig        == 0       ) THEN
          DO i=1, F(n)%nNodes  !---- write output -------------
            IF( .NOT. F(n)%NodeToDump(i) ) CYCLE
            nodeIndex = F(n)%Node(i)                                !> mesh index of field node
            IF( ParallelComputing )  nodeIndex = KNOLG( nodeIndex ) !> global index of mesh index
            WRITE(6,9119)    !> Write the action concerning data of this node to the log-file
     &      'XdumX', A%TimeStart, time
     &      ,F(n)%dz_Tot(i)                      !> total dz of this node
     &      ,F(n)%NodeArea(i) * F(n)%dz_Tot(i)   !> total dumped volume of this node
     &      ,nodeIndex, F(n)%km(i),F(n)%X(i),F(n)%Y(i),F(n)%Name, m
          ENDDO
          F(n)%nNodeToDump   = 0
          F(n)%NodeToDump(:) = .FALSE.
         !A%DumpVolume = -123.4567890D0    ! neue Bedingung

        ENDIF
!
!
        !> While Dig_by_criterion is operating, the DigVolume and accordingly the
        !  DumpVolume will change due to bed load discharge. To take this into account,
        !  we execute the following IF-Block .
        !  Other actions operate with the Dig/DumpVolume which is determined when
        !  the action is FirstTimeActive. Bedload discharge has no effect on the
        !  determined Dig/DumpVolume.
        IF(       F(n)%nNodeToDump == 0
     &      .AND. A%nNodeToDig     == 0
     &      .AND. A%DumpVolume     >  1.0D-3 ) THEN
          F(n)%Z(:) = z_sis( F(n)%Node(:) ) !>  F(n)%Node(i)= mesh index of field node
          F(n)%NodeToDump(:) = .FALSE.
           CALL Calculate_PlanarLevel( F(n), A%DumpVolume, 1 ) !> 1 => dump;  The result is
                                                              !  total F%dz(:) and F%NodeToDump(:).
          F(n)%dz_Tot(:) = F(n)%dz(:)
!
          F(n)%nNodeToDump = COUNT( F(n)%NodeToDump(:) )
          A%fillArea = SUM( F(n)%NodeArea(:), MASK=F(n)%NodeToDump(:) )
!
          IF(ParallelComputing) THEN
            A%fillArea       = P_DSUM( A%fillArea )
            F(n)%nNodeToDump = P_ISUM( F(n)%nNodeToDump )
          ENDIF
!
!
        ENDIF
!
!
        IF(       F(n)%nNodeToDump == 0
     &      .AND. A%nNodeToDig     == 0
!     &      .AND. A%DumpVolume     <= 1.0D-9
     &                                        ) THEN
          A%DumpVolume = -123.4567890D0
        ENDIF
!
!
      ENDIF !( .NOT. A%FirstTimeActive )
!
!
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Rate_Planar END-------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Dump_by_Rate_Planar         !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
