!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dig_by_Criterion               !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &(   A, F, dt_ts, z_sis, zr_sis, zrl_sis, dzCL_sis
     &  , AVAIL, ES1_sis, time, KNOLG, m  )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY : ParallelComputing, nGrainClass, ipid, Lu
!
      USE INTERFACE_PARALLEL, ONLY : P_DSUM, P_ISUM
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  InfoMessage
     &                               , Dump_by_Rate
     &                               , ErrMsgAndStop
     &                               , Write_Node_Info              !node-info
     &                               , Dealloc_Dump_Field
     &                               , Dump_by_Rate_Planar
     &                               , CalcDigVolumeInRadius
     &                               , Set_by_Profiles_Values_for
     &                               , Set_RefLevel_by_Waterlevel
#endif
!
!
      IMPLICIT NONE
!
      TYPE(t_Action),INTENT(INOUT)   :: A
      TYPE(t_Field) ,INTENT(INOUT)   :: F
      REAL (KIND=R8),INTENT(IN)      :: dt_ts
      REAL (KIND=R8),INTENT(IN)      :: z_sis(:)     ! bottom [m+NN] at time (assumed-shape array)
      REAL (KIND=R8),INTENT(IN)      :: zr_sis(:)    ! ridged bed [m+NN] 
      REAL (KIND=R8),INTENT(IN)      :: zrl_sis(:)   ! reference level [m+NN] 
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT)   :: dzCL_sis(:)
      REAL (KIND=R8),INTENT(IN)      :: AVAIL(:,:,:) ! debug test!  assumed-shape array
      REAL (KIND=R8),INTENT(IN)      :: ES1_sis(:)   ! (non const.) thickness of active laver  [m]
      REAL (KIND=R8),INTENT(IN)      :: time         !  time [s]
      INTEGER       ,INTENT(IN)      :: KNOLG(*)     ! index list: Local to Global node index
      INTEGER       ,INTENT(IN)      :: m            ! number of Action
!
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      INTEGER            :: i, iCL, iM, status, nodeIndex, U
      INTEGER            :: nLessNodesToDig = 0
      REAL (KIND=R8)     :: dzDig_ts, dzDig
      REAL (KIND=R8)     :: dzEvo_sis, sumInput_ts
      REAL (KIND=R8)     :: remainingDepthToDig
!
!
      REAL (KIND=R8)                       :: heap   !> total dug volume of all FieldNodes
      REAL (KIND=R8),ALLOCATABLE                     !  during the current timestep
     &              ,SAVE ,DIMENSION   (:) :: heapCL !> total dug volume per grain class of all
      REAL (KIND=R8),ALLOCATABLE                     !  FieldNodes during the current timestep
     &              ,SAVE ,DIMENSION   (:) :: layCL
!
      CHARACTER  (21) :: NL                         ! to store the NEW-LINE character
      CHARACTER  (16) :: S1,S2,S3                   ! to convert num. values to string
      CHARACTER (256) :: S4,S5,S6                   ! to create the error message
!
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
      U = Lu   ! just to have a short variable name for the standard output
!
!      dbug WRITE(6,*)'?>-------  SR Dig_by_Criterion -------------'
      SRname%s = "Dig_by_Criterion"    ! subroutine name
!
!
      IF( .NOT. ALLOCATED(layCL) ) ALLOCATE(  layCL( nGrainClass ))
      IF( .NOT. ALLOCATED(heapCL)) ALLOCATE( heapCL( nGrainClass ))
      !_____________________________________________________________
      !                                                           __|
      !                                   Dig_by_Criterion     __|
      IF( A%FirstTimeActive )  THEN  !________________________|
        A%State = 1     ! 1 = Action currently active
!
!
        ALLOCATE( F%km( F%nNodes )  , stat=status)
        F%km(:) = -1234.5D0

        ALLOCATE( F%NodeToDig( F%nNodes ), stat=status)
        F%NodeToDig(:) = .FALSE. ! initialisation
!
        IF( .NOT. ALLOCATED( F%refZ ) ) THEN
          ALLOCATE( F%refZ( F%nNodes ), stat=status)
          F%refZ(:) = -1234.5D0   ! value must be -1234.5
        ENDIF
!
        ALLOCATE( F%targZ( F%nNodes ), stat=status)
        F%targZ(:) = 1234.5D0
!
        CALL Set_by_Profiles_Values_for( "refZ", "km", F )     !> the result is F%refZ(:) and F%km(:)
!
        IF(      A%ReferenceLevel(1:8) == 'WATERLVL')  THEN    !> overwrite F%refZ(:)
          CALL Set_RefLevel_by_Waterlevel( F, A, m)
        ELSE IF( A%ReferenceLevel(1:8) == 'GRID')      THEN    !> overwrite F%refZ(:)
          F%refZ(:) = zrl_sis( F%Node(:) )
        ENDIF
!
!
        A%tsCount = 0            ! counter of time steps while digger is working
!
!
        F%nNodeToDig   = 0
        DO i=1, F%nNodes         ! mark nodes to dig
          iM = F%Node(i)      ! mesh index of field node
          IF( z_sis(iM) > (F%refZ(i) - A%CritDepth) ) THEN
            F%NodeToDig(i) = .TRUE.
            F%nNodeToDig   = F%nNodeToDig + 1
          ENDIF
        ENDDO
!
!
        F%targZ(:) =  F%refZ(:) - A%DigDepth !> Set refZ to target digging level
!
!
        IF( A%MinVolume > 0.0D0 ) THEN
          CALL CalcDigVolumeInRadius( A, F, z_sis, KNOLG )  !> The result is F%NodeToDig(:)
        ENDIF                                               !  and F%nNodeToDig
!
        A%DigVolume = SUM((  ( F%targZ(:) - z_sis(F%Node(:)) )
     &                      *  F%NodeArea(:)),  MASK = F%NodeToDig(:) )
!
        IF( ParallelComputing ) THEN
          F%nNodeToDig = P_ISUM( F%nNodeToDig )
          A%DigVolume  = P_DSUM( A%DigVolume )
        ENDIF
!
        SELECT CASE( A%DumpMode )                    !> The value of  A%MaxDump_dz_ts  is needed
          CASE( 20 )                                 !  by the InfoMessage-routine. It will be
            CALL Dump_by_Rate ( A, dt_ts, dzCL_sis ) !  calculated in one of the Dump-routines.
          CASE( 21 )
            CALL Dump_by_Rate_Planar(   A, dt_ts, z_sis, zrl_sis
     &                                , dzCL_sis, time, KNOLG, m )
          CASE DEFAULT
        END SELECT
!
        A%MaxDig_dz_ts = dt_ts * A%DigRate
!
        CALL InfoMessage( A, m, time )
!
!       WRITE(6,'(A8,A16,A15,A19,A8,A8,A16,A14,A9)')
        WRITE(U,'(A8,A16,A15,A19,A8,A8,A16,A14,A12,A13)')
     &    "Lable   ","TimeStart       ","time           "
     &   ,"dig volume         ","node    ","km      "
     &   ,"x-coordinate    ","y-coordinate  ","field name  "
     &   ,"action number"
!
        A%GrainClass(:) = 0.0D0
        A%DumpVolume    = 0.0D0
        A%SumDiged      = 0.0D0
        A%InputDigField = 0.0D0
!
        A%FirstTimeActive = .FALSE.
!             ________________________________________________
      ENDIF  ! FirstTimeActive                                |__
      !                                   Dig_by_Criterion       |__
      !_____________________________________________________________|
!
!
        !> Before digging we calc. the amount of sediment that was
        !  transported by morphodynamic during the last time step (ts)
        !  into (or out of) the dig nodes WHILE THEY WERE ACTIVE.
        !  I case there is a further action operating at the same time
        !  on this field and it is carried out already (what depends on the
        !  internal order of execution), then it will
        !  appear here as sumInput_ts too.
        sumInput_ts = 0.0D0
        DO iCL=1, nGrainClass    !  Only nodes with
          DO i=1, F%nNodes       !  NodeToDig is TRUE are included for it.
            iM = F%Node(i)       !> mesh index of field node
            IF( F%NodeToDig(i) ) THEN
              sumInput_ts =   sumInput_ts
     &                      + dzCL_sis(iCL)%R(iM) * F%NodeArea(i)
            ENDIF
          ENDDO
        ENDDO
!
!
      !> The point where Nestor is linked in the Sisyphe time loop we have
      !  the following situation:
      !  1.) The active layer respectively the AVAIL array
      !      has the state of the previous time step.
      !  2.) The bottom level (z_sis) has the state of the previous time step.
      !  3.) The evolution per class (dzCL_sis) is already calculated
      !      for the current time step. But it's not mixed into the
      !      active layer yet.
      !  To know what sediment mixture the digger will grab, we combine
      !  the active layer and the evolution.
!
      heapCL(:)       = 0.0D0              !> Initialisation must happen here AND again below
      nLessNodesToDig = 0
      IF( F%nNodeToDig > 0 ) THEN
        A%tsCount = A%tsCount + 1          !> Count time steps while digger is working
        dzDig_ts  = dt_ts * A%DigRate      !> Depth to dig during one time step (ts)
        heapCL(:) = 0.0D0
        DO i=1, F%nNodes
!
          IF( .NOT. F%NodeToDig(i) ) CYCLE
!
          dzDig = dzDig_ts  !> positive value although dig means going down
          iM = F%Node(i)    !> mesh index of field node
!
          dzEvo_sis = 0.0D0
          DO iCL=1, nGrainClass
            dzEvo_sis  =   dzEvo_sis + dzCL_sis(iCL)%R(iM)  !> Sum of evolution of all classes
                                                            !  at node at current time step
            layCL(iCL) =   dzCL_sis(iCL)%R(iM)            !>  Thickness of evolution per class
     &                   + AVAIL(iM,1,iCL) * ES1_sis(iM)  ! + thickness of class in active layer
          ENDDO            !.......................!----- convert fraction to thickness
!
          remainingDepthToDig = z_sis(iM) + dzEvo_sis - F%targZ(i)
!
          IF( remainingDepthToDig < dzDig ) THEN   !> dzDig is to big ==> digger would dig to deep.
            dzDig = remainingDepthToDig            !> Fit digging depth for this node and time step so
                                                   !  that the specified depth will be reached exactly.
            F%NodeToDig(i)  = .FALSE.              !> No more digging for this node.
            nLessNodesToDig = nLessNodesToDig + 1  !> count nodes that are no longer supposed to be dug.
            !---- write output -------------
            IF( ParallelComputing ) THEN
              nodeIndex = KNOLG(iM)
            ELSE
              nodeIndex = iM
            ENDIF
9119        FORMAT(A5,1X,4(G18.11,1X),I8,1X,F8.3,1X,2(G15.8,1X),A16,I4)
            WRITE(6,9119)    !> Write the action concerning data of this node to the log-file
     &           'XdigX'     !  (6 => if parallel: all to log-file of parallel prozess zero)
     &          , A%TimeStart
     &          , time
     &          , -( dzDig_ts * (A%tsCount-1) + dzDig )                 !> For this period calc. dz of this node
     &          , -( dzDig_ts * (A%tsCount-1) + dzDig ) * F%NodeArea(i) !> For this period calc. dug volume of this node
     &          , nodeIndex
     &          , F%km(i)
     &          , F%X(i)
     &          , F%Y(i)
     &          , F%Name
     &          , m             ! action number
!
          ENDIF ! ( remainingDepthToDig < dzDig )
!
!
          IF( dzEvo_sis - dzDig <= -ES1_sis(iM) ) THEN  !Digger exceeds active layer thickness
            ! ----- prepare the error message
            NL = NEW_LINE(NL)//" ?> error:          "
            WRITE(S1,'(I9)') iM                    !> convert integer value to string
            IF( ParallelComputing )
     &        WRITE(S1,'(I9)') KNOLG(iM)           !> convert the global mesh index to string
            WRITE(S2,'(E12.4)') ES1_sis(iM)        !> convert real value to string
            WRITE(S3,'(E12.4)') dzDig + dzEvo_sis  !> convert real value to string
            S1 = adjustl(S1)
            S2 = adjustl(S2)
            S3 = adjustl(S3)
            S4 = " At node "//TRIM(S1)//" dzDig+dzEvol exceeds the"//NL
     &          //"active layer thickness (="//TRIM(S2)//"[m])."
            S5 = " dzDig+dzEvol = "//TRIM(S3)//"[m] is too much."
            IF( z_sis(iM) + dzEVO_sis - dzDig <= zr_sis(iM) ) THEN ! If digging into ridgid bed
              S6 = ' You try to dig into the ridged bed,'//
     &             ' which is not allowed !'
            ELSE
              S6 = " ==> reduce the DigRate "//NL//
     &              "    or increase the active layer thickness"//NL//
     &              "    or ..."
            ENDIF
!
            Call ErrMsgAndStop(
     &                  "reason:"//TRIM(S4)
     &                 ,"       "//TRIM(S5)
     &                 ,"       "//TRIM(S6)
     &                 ,"occured in Action: ", m, SRname, ipid)
          ENDIF
!
          layCL(:) = layCL(:) / (dzEvo_sis + ES1_sis(iM)) !> Convert thickness to fraction
!
          DO iCL=1, nGrainClass   !>  Digging happens here
            dzCL_sis(iCL)%R(iM) =      dzCL_sis(iCL)%R(iM)         !> We calculate the mixture which
     &                               - layCL(iCL) * dzDig          !  is left in the ground
          ENDDO
                                  !..............................!--- convert fraction to volume
          heapCL(:) = heapCL(:) + layCL(:) * dzDig * F%NodeArea(i) !> During the current time step of all nodes
                                                                   !  we collect for each class the dug material
                                                                   !  and put it in heaps per class
        ENDDO  ! nNodes
      ENDIF  ! F%nNodeToDig > 0
!
!
      IF( ParallelComputing ) THEN
        DO iCL=1, nGrainClass
          heapCL(iCL) = P_DSUM( heapCL(iCL) )
        ENDDO
        nLessNodesToDig = P_ISUM( nLessNodesToDig )
        sumInput_ts     = P_DSUM( sumInput_ts )
      ENDIF
!
      F%nNodeToDig = F%nNodeToDig - nLessNodesToDig

      A%nNodeToDig = F%nNodeToDig    !> because A%nNodeToDig is used in the dumping routines
!
      A%InputDigField = A%InputDigField + sumInput_ts
!
!
      !=================================================================
      !> calc. new DumpVolume and its sediment composition
      !  each time step we add the dug material per class to A%DumpVolume
!
      heap = SUM( heapCL(:) )  ! Total volume of dug material at current time step
!
      ! Add per class   volume of dug material and old DumpVolume
      heapCL(:) = heapCL(:) + A%GrainClass(:) * A%DumpVolume
                              !............................!--- convert fraction to volume
!
      ! new DumpVolume
      A%DumpVolume  = A%DumpVolume  + heap
!
      ! new sediment composition of the DumpVolume
      IF( A%DumpVolume  > 0.0D0 ) THEN
        A%GrainClass(:) = heapCL(:) / A%DumpVolume
                          !......................!----- convert volume to fraction
      ENDIF
!
      A%SumDiged = A%SumDiged + heap
!
!
!
      IF(     F%nNodeToDig  >  0                     !> Digging not accomplished
     &   .OR. (       A%FieldDumpID > 0              !> Dumping is assigned (a dump field is linked to the action)
     &          .AND. A%DumpVolume  > 0.0D0 ) ) THEN !  and Dumping is not accomplished
!
        IF( A%DumpMode == 21 ) CALL Dump_by_Rate_Planar(  A, dt_ts
     &                  , z_sis, zrl_sis, dzCL_sis, time, KNOLG, m )

        IF( A%DumpMode == 20 ) CALL Dump_by_Rate( A, dt_ts, dzCL_sis )
!
      ELSE     !> finalise action temporarily or for ever
!
        IF( time <= A%TimeEnd ) THEN
!
          IF( A%TimeStart + A%TimeRepeat  >  A%TimeEnd )THEN
            A%State           = 9     ! 9 = for ever inactive
            CALL Write_Node_Info( 'dump', A, m, time, KNOLG )
            CALL InfoMessage( A, m, time )
          ELSE
            A%State           = 2     ! 2 = temporary inactive
            CALL Write_Node_Info( 'dump', A, m, time, KNOLG )
            CALL InfoMessage( A, m, time )
            A%TimeStart       = A%TimeStart + A%TimeRepeat
            A%FirstTimeActive = .TRUE.
            A%tsCount         = 0     !  counter of time steps while digger is working
          ENDIF
!
        ELSE
          A%State           = 9     ! 9 = for ever inactive
          CALL InfoMessage( A, m, time )
!
        ENDIF
!
        DEALLOCATE( F%refZ )
        DEALLOCATE( F%targZ )
        DEALLOCATE( F%km )
        DEALLOCATE( F%NodeToDig )
        CALL Dealloc_Dump_Field( A )
        !DEALLOCATE( layCL )
        !DEALLOCATE( heapCL )
        !DEALLOCATE( A%GrainClass )
      ENDIF
!
!      dbug WRITE(6,*)'?>-------  SR Dig_by_Criterion END ---------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Dig_by_Criterion            !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
