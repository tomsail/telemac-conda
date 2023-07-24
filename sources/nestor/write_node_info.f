!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Write_Node_Info                !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( mode, A, m, time, KNOLG )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY : ParallelComputing, F
!
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  Set_by_Profiles_Values_for
#endif
!
      IMPLICIT NONE
      CHARACTER  (len=*)             :: mode         ! values are "dump" or "dig"
      TYPE(t_Action),INTENT(INOUT)   :: A            ! Action
      INTEGER       ,INTENT(IN)      :: m            ! number of Action
      REAL (KIND=R8),INTENT(IN)      :: time         ! time [s]
      INTEGER       ,INTENT(IN)      :: KNOLG(*)     ! index list: Local to Global node
!
#ifndef NESTOR_INTERFACES
!
      !--------------------- local variables ---------------
!
      INTEGER               :: i, n, status
      INTEGER               :: nodeIndex, nFieldNodes
      REAL (KIND=R8)        :: dz_Node, dz_tot
      CHARACTER  (5)        :: lable
      LOGICAL               :: km_is_temporary
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
!
!      dbug WRITE(6,*)'?>-------  SR Write_Node_Info --------------'
      SRname%s = "Write_Node_Info"    ! subroutine name
!
9119  FORMAT (A5,1X,4(G18.11,1X),I8,1X,F8.3,1X,2(G15.8,1X),A16,I4)
!
!
      IF(.NOT.(mode == 'dump' .OR. mode == 'dig')) THEN
        WRITE(*,*)' wrong mode in SR Write_Node_Info'
        STOP
      ENDIF
!
      IF( mode == 'dump' ) THEN
        lable  = "XdumX"
        n      = A%FieldDumpID
        SELECT CASE( A%DumpMode )
          CASE ( 10 )         !> Dump_by_Time
            IF( A%Solo ) THEN
              dz_tot =   A%DumpVolume / F(n)%Area
            ELSE
              dz_tot = - A%DigVolume / F(n)%Area
            ENDIF
          CASE ( 20 )         !> Dump_by_Rate
            dz_tot =   A%SumDumped / F(n)%Area
        END SELECT
      ELSEIF( mode == 'dig' ) THEN
        lable  = "XdigX"
        n      = A%FieldDigID
        dz_tot = - A%SumDiged/F(n)%Area
      ENDIF
!
      nFieldNodes = F(n)%nNodes
!
      !> If F%km is so far not allocated, we do it here.
      !  km is used for the node info output to the listing.
      km_is_temporary = .FALSE.
      IF( .NOT. ALLOCATED( F(n)%km ) ) THEN
        km_is_temporary = .TRUE.                        !> because it F%km was not used before 
        ALLOCATE( F(n)%km( nFieldNodes ), stat=status)
        F(n)%km(:) = -1234.5D0   ! value must be -1234.5
        CALL Set_by_Profiles_Values_for( "--", "km", F(n) )
      ENDIF
!
      DO i=1, nFieldNodes
!
        IF( mode == 'dump' .AND. A%DumpMode == 11 ) THEN     !> Dump_by_Time_Planar
          IF( .NOT. F(n)%NodeToDump(i) ) CYCLE
          dz_Node = F(n)%dZ(i) * A%nts
        ELSEIF( mode == 'dump' .AND. A%DumpMode == 21 ) THEN !> Dump_by_Rate_Planar 
          IF( .NOT. F(n)%NodeToDump(i) ) CYCLE
          dz_Node = F(n)%dZ_Tot(i)
        ELSE
          dz_Node = dz_tot
        ENDIF
!
!
        nodeIndex = F(n)%Node(i)             !> mesh index of field node
        IF( ParallelComputing ) nodeIndex = KNOLG(F(n)%Node(i))
!
        IF( abs(dz_Node) > 1.E-14 ) THEN
                           !> Write the action concerning data of this node to the log-file
          WRITE(6,9119)    !  (unit:6 => if parallel: all to log-file of parallel prozess zero)
     &          lable, A%TimeStart, time
     &        , dz_Node                        !> total dz of node caused by dig or dump
     &        , dz_Node * F(n)%NodeArea(i)     !> total dumped volume of this node
     &        , nodeIndex
     &        , F(n)%km(i)
     &        , F(n)%X(i), F(n)%Y(i), F(n)%Name
     &        , m                                ! Action number
         ENDIF 
!
      ENDDO  ! nNodes
!
      IF( km_is_temporary ) THEN      !> Because F%km was not allocated somewhere else before
        DEALLOCATE( F(n)%km  )        !  we can deallocate it.
      ENDIF                           !  Here it was only used for the node info output.
!
!
!      dbug WRITE(6,*)'?>-------  SR Write_Node_Info END ----------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Write_Node_Info             !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
