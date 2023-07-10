!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  CalcDigVolumeInRadius          !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( A, F, z_sis, KNOLG )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor,ONLY : ParallelComputing, npoinGlobal, npoin
      USE INTERFACE_PARALLEL, ONLY : P_DSUM, P_ISUM
!
      IMPLICIT NONE
      TYPE(t_Action),INTENT(INOUT) :: A
      TYPE(t_Field) ,INTENT(INOUT) :: F
      REAL (KIND=R8),INTENT(IN)    :: z_sis(:)  ! z-coordinate [ m ]  (assumed-shape array)
      INTEGER       ,INTENT(IN)    :: KNOLG(*)  ! index list: Local to Global node index
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      INTEGER              ::  i,ii, j, k, kk, iMesh, iField, status
      INTEGER              ::  hitNodeToDig = 0
      REAL (KIND=R8)       ::  x, y
      REAL (KIND=R8)       ::  sqrdRadius,  sqrdDist
      REAL (KIND=R8)       ::  DigVolumeInRadius
!
      INTEGER,ALLOCATABLE
     &       ,DIMENSION(:) ::  iM2iF ! iMesh(= local) node index to Field node index
      LOGICAL,ALLOCATABLE
     &       ,DIMENSION(:) ::  NodeMeetsVolumeInRadiusCriterion
      INTEGER,ALLOCATABLE
     &       ,DIMENSION(:) ::  DigNodeInsideRadius
      INTEGER              :: nDigNodeInsideRadius
!
!      dbug WRITE(6,*)'?>-------  SR CalcDigVolumeInRadius --------'
!
      ALLOCATE( NodeMeetsVolumeInRadiusCriterion(F%nNodes) )
      ALLOCATE( DigNodeInsideRadius(F%nNodeToDig) )
!
      sqrdRadius = A%MinVolumeRadius**2    !> doing this we save on calculating
                                           !  the square root later when we
                                           !  compare with "sqrdDist"
!      ________________________________________________________________
!     |                                    parallel computing          |
      IF(ParallelComputing) THEN !_____________________________________|
!
      ALLOCATE( iM2iF(0:npoin), stat=status )
!
      iM2iF(:) = 0
      DO iMesh=1, npoin         !> Fill the index list: iMesh(= local) node index to Field node index
        DO j=1, F%nNodes        !  the local node index is the node number for this partition
          IF(iMesh == F%Node(j)) iM2iF(iMesh) = j
        ENDDO
      ENDDO
!
      DigNodeInsideRadius(:)              = 0
      NodeMeetsVolumeInRadiusCriterion(:) = .FALSE.
!
      DO i=1, npoinGlobal  ! =========
!
!       iMesh  = KNOGL(i)  !> Convert global to local index. If "global node" is
                           !  not located in the current partition iMesh becomes 0,
                           !  that's why the dimension of array
                           !  iM2iF must be (0:npoin)  and not (1:npoin)
!
        iMesh = 0                   !> Convert global to local index. If "global node" is
        DO ii=1, npoin              !  not located in the current partition iMesh becomes 0,
          IF( KNOLG(ii) == i ) THEN !  that's why the dimension of array
            iMesh = ii              !  iM2iF must be (0:npoin)  and not (1:npoin)
            EXIT                    !
          ENDIF                     !
        ENDDO                       !
!
        !WRITE(6,*)'?> IPID global local = yxy',IPID, i, iMesh  ! debug
!
!
        iField = iM2iF( iMesh )    !> Convert local index to Field index
                                   !  If "global node" is not located in the
                                   !  Field iField will be set to 0
!
        hitNodeToDig = 0
        IF( iField > 0 ) THEN   !> Local Node resides in the Field
          IF( F%NodeToDig(iField) ) hitNodeToDig = 1
        ENDIF
!
        IF( hitNodeToDig == 1 ) THEN !> In this partition the "global node" resides and
          x = F%X(iField )           !  is marked as to dig. That's why we save his location.
          y = F%Y(iField )           !  This must be done before P_ISUM(hitNodeToDig)
        ELSE
          x = 0.0D0                  !> The "global node" does not
          y = 0.0D0                  !  reside in this partition
        ENDIF
!
        hitNodeToDig = P_ISUM( hitNodeToDig ) !> Count and broadcast number of hits.
                                              !  If node is an interface node it
                                              !  exists multiple.
        IF( hitNodeToDig == 0 ) CYCLE         !> In none partition a node to dig was hit
!
        x = P_DSUM( x )     !> Broadcast valid location to all partitions
        y = P_DSUM( y )     !  no interface node: " x = 0.0 + 0.0 + ... + 0.0 + 5.3 =  5.3 "
                            !     interface node: " x = 0.0 + 0.0 + ... + 5.3 + 5.3 = 10.6 "
!
        IF( hitNodeToDig > 1 ) THEN     !> Node is an interface node thus the value of
          x = x / DBLE( hitNodeToDig )  !  x-, y-coordinate is double or triple or .... .
          y = y / DBLE( hitNodeToDig )  !  We have to compensate this.
        ENDIF
!
        DigVolumeInRadius = 0.0D0
        k = 0
        DO j=1,F%nNodes                       !> Now every partition got the coordinates
          IF( .NOT. F%NodeToDig(j) ) CYCLE    !  of the center point of the test radius.
                                              !  We calculate the volume that is to dig
          iMesh = F%Node(j)                   !  inside the test radius.
          sqrdDist = (F%x(j)-x)**2  +  (F%y(j)-y)**2 !> Here it's needless to calc. the square root because
          IF( sqrdDist <= sqrdRadius ) THEN          !  it's enough to compare the arguments of the square root
            DigVolumeInRadius =   DigVolumeInRadius + F%NodeArea(j)  !  ... + area
     &                          * ( z_sis(iMesh) - F%targZ(j) )      !      * depth to dig
            k = k + 1
            DigNodeInsideRadius(k) = j  ! save index of node that's inside the radius
          ENDIF
        ENDDO
        nDigNodeInsideRadius = k        ! number of nodes to dig inside radius
!
        DigVolumeInRadius = P_DSUM( DigVolumeInRadius )
!
        IF( DigVolumeInRadius >= A%MinVolume ) THEN
          DO k=1, nDigNodeInsideRadius
            kk  =  DigNodeInsideRadius(k)          ! convert to Field index
            NodeMeetsVolumeInRadiusCriterion(kk) = .TRUE.
          ENDDO
        ENDIF
!
      ENDDO   ! i=1, npoinGlobal
      ENDIF !(ParallelComputing)
!
!      ________________________________________________________________
!     |                                       serial computing         |
      IF(.NOT.ParallelComputing) THEN !________________________________|
        DigNodeInsideRadius(:)              = 0
        NodeMeetsVolumeInRadiusCriterion(:) = .FALSE.
        DO i=1, F%nNodes
          IF( .NOT. F%NodeToDig(i) ) CYCLE
!
          x = F%X(i)
          y = F%Y(i)
          DigVolumeInRadius = 0.0D0
          k = 0
          DO j=1,F%nNodes
            IF( .NOT. F%NodeToDig(j) ) CYCLE
!
            iMesh = F%Node(j)
            sqrdDist = (F%X(j)-x)**2  +  (F%Y(j)-y)**2 !> Here it's needless to calc. the square root because
            IF( sqrdDist <= sqrdRadius ) THEN          !  we limit to compare the arguments of the square root
              DigVolumeInRadius =   DigVolumeInRadius + F%NodeArea(j)  !  ... + area
     &                            * ( z_sis(iMesh) - F%targZ(j) )      !      * depth to dig
              k = k + 1
              DigNodeInsideRadius(k) = j  ! save index of node that's inside the radius
            ENDIF
          ENDDO
          nDigNodeInsideRadius = k        ! number of nodes to dig inside radius
!
          IF( DigVolumeInRadius >= A%MinVolume ) THEN
            DO k=1, nDigNodeInsideRadius
              kk  =  DigNodeInsideRadius(k)          ! convert to Field index
              NodeMeetsVolumeInRadiusCriterion(kk) = .TRUE.
            ENDDO
          ENDIF
!
        ENDDO                      !___________________________________
      ENDIF !.NOT.ParallelComputing                                    |
      !________________________________________________________________|
!
      F%NodeToDig(:) = NodeMeetsVolumeInRadiusCriterion(:)
      F%nNodeToDig   = COUNT( NodeMeetsVolumeInRadiusCriterion(:))
!
!
      !DO j=1,F%nNodes                                 ! debug
      !  IF( F%NodeToDig(j) ) THEN                     ! debug
      !    WRITE(*,*)'?> satisfyed:: ', F%X(j), F%Y(j) ! debug
      !  ENDIF                                         ! debug
      !ENDDO                                           ! debug
!
      !IF(ParallelComputing) CALL P_SYNC()     ! debug
      !WRITE(*,*)'?> vor STOP '                ! debug
      !stop                                    ! debug
!
!
      IF( ALLOCATED( NodeMeetsVolumeInRadiusCriterion ) )
     &   DEALLOCATE( NodeMeetsVolumeInRadiusCriterion )
      IF( ALLOCATED( DigNodeInsideRadius ) )
     &   DEALLOCATE( DigNodeInsideRadius )
      IF( ALLOCATED( iM2iF ) ) DEALLOCATE( iM2iF )
!
!      dbug WRITE(6,*)'?>-------  SR CalcDigVolumeInRadius END ----'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE CalcDigVolumeInRadius       !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
