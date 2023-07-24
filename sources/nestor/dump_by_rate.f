!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dump_by_Rate                   !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( A, dt_ts, dzCL_sis )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY : F, ParallelComputing, nGrainClass
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
!
!
      IMPLICIT NONE
      TYPE(t_Action),INTENT(INOUT) :: A
      REAL (KIND=R8),INTENT(IN)    :: dt_ts      ! time-step-duration  [ s ]
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT) :: dzCL_sis(:)
!
#ifndef NESTOR_INTERFACES
!
      !------- local variables ---------------
      INTEGER            :: i, iCL, iMesh, n
      REAL (KIND=R8)     :: dz_ts, sumInput_ts
!
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Rate -----------------'
!
!
      n = A%FieldDumpID
      !__________________________________________________________
      !                                                         _|
      !                                Dump_by_Rate           _|
      IF( A%FirstTimeActive )  THEN  !_______________________|
!
        A%MaxDump_dz_ts  = A%DumpRate * dt_ts    !> because every node elevates with the same rate
        A%SumDumped      = 0.0D0
        A%InputDumpField = 0.0D0
!             _______________________________________________
      ENDIF  !(FirstTimeActive )                             |_
      !                                Dump_by_Rate            |_
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
            sumInput_ts =   sumInput_ts
     &                    + dzCL_sis(iCL)%R(iMesh) * F(n)%NodeArea(i)
          ENDDO
        ENDDO
!
!
        dz_ts = dt_ts * A%DumpRate                   ! Hight to dump during one time step
!
        IF( dz_ts * F(n)%Area  >=  A%DumpVolume ) THEN  !> Dump hight calc. by DumpRate is bigger than DumpVolume
          dz_ts        = A%DumpVolume / F(n)%Area       !> Reduce dz_ts
          A%dzCL_ts(:) = dz_ts * A%GrainClass(:)        !> Change of z per time step per class
          DO iCL=1, nGrainClass
            DO i=1, F(n)%nNodes
              iMesh = F(n)%Node(i)     ! Mesh index of field node
              dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh) !> dumping 
     &                                 + A%dzCL_ts(iCL)         !  happens here !
            ENDDO
          ENDDO
          A%DumpVolume = 0.0D0
        ELSE
          A%dzCL_ts(:) = dz_ts * A%GrainClass(:)     !> change of z per time step per class
          DO iCL=1, nGrainClass                      !> dump it
            DO i=1, F(n)%nNodes
              iMesh = F(n)%Node(i)     ! Mesh index of field node
              dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh) !> dumping 
     &                                 + A%dzCL_ts(iCL)         !  happens here !
            ENDDO
          ENDDO
!
          A%DumpVolume = A%DumpVolume - dz_ts * F(n)%Area  !> Because the value of dz_ts is the same for all nodes
                                                           !  and because F(n)%Area is the whole area over all
        ENDIF                                              !  partitions there is no need of "P_DSUM" at this point.
!
        A%SumDumped  = A%SumDumped  + dz_ts * F(n)%Area    !> No need of "P_DSUM" at this point too (see above).
!       
        IF(ParallelComputing) THEN
          sumInput_ts = P_DSUM( sumInput_ts )
        ENDIF
!
        A%InputDumpField = A%InputDumpField + sumInput_ts
!
!
!        dbug WRITE(6,*)'?>-------  SR Dump_by_Rate END -------------'
!
      ENDIF !( .NOT. A%FirstTimeActive )    
!
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Dump_by_Rate                !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
