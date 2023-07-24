!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Reset_bottom                   !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &(   A, F, dt_ts, z_sis, dzCL_sis
     &  , time, m   )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  nGrainClass, ipid, lim_dzts
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  InfoMessage, ErrMsgAndStop
#endif
!
!
      IMPLICIT NONE
      TYPE(t_Action),INTENT(INOUT) :: A            !> Action
      TYPE(t_Field) ,INTENT(INOUT) :: F            !> Field
      REAL (KIND=R8),INTENT(IN)    :: dt_ts        !> time-step-duration  [ s ]
      REAL (KIND=R8),INTENT(IN)    :: z_sis(:)     !> bottom [ m+NN ] assumed-shape array
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT) :: dzCL_sis(:)  !> bedload evolution per Class  [ m ]
      REAL (KIND=R8),INTENT(IN)    :: time         !> time [ s ]
      INTEGER       ,INTENT(IN)    :: m            !> number of Action
!
#ifndef  NESTOR_INTERFACES
!
      !------- local variables ---------------
      !DOUBLEPRECISION, EXTERNAL ::  P_DSUM
!
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
!
      INTEGER            :: i, iCL, iMesh, status !, nodeIndex
      REAL (KIND=R8)     :: max_dz_dt
      CHARACTER  (16)    :: rCh              ! to store a real value as string
!
!
!
!      dbug WRITE(6,*)'?>-------  SR Reset_bottom -----------------'
      SRname%s = "Reset_bottom"        !> subroutine name
!
!--------------------------------------------------------
      IF( A%FirstTimeActive ) THEN
        A%State = 1     ! 1 = Action currently active
        CALL InfoMessage( A, m, time )
!
        ALLOCATE( F%Z(    F%nNodes ), stat=status)
        ALLOCATE( F%dz(   F%nNodes ), stat=status)
!
        A%sumInput = 0.0D0
!
        F%dZ   = 999.9D0
        DO i=1, F%nNodes     !> set value in the Field-Structur
          iMesh     = F%Node(i)   !> mesh index of field node
          F%Z(i)    = z_sis( iMesh )
        ENDDO
!
!
        F%dz(:)  =  F%Z0(:) - F%Z(:)
!
!
        A%nts = INT( (A%TimeEnd - time) / dt_ts ) !> calculate number of time
                                                  !  steps (nts) to fulfil the Action
!
        max_dz_dt =  MAXVAL( F%dz ) / DBLE( A%nts )
        IF( max_dz_dt  >  lim_dzts ) THEN
          WRITE(rCh,'(F16.8)') max_dz_dt       !> convert real value to string and then
          WRITE(rCh,'(16A)') adjustl(rCh)      !  convert string to left-aligned string
          Call ErrMsgAndStop(
     &     "reason: Change of bottom level per time step "
     &    ,"        dzts = "//rCh//"[m] is too big.      "
     &    ,"        lim_dzts = 0.1[m]                    "
     &    ,"occured in Action: ", m, SRname, ipid      )
        ENDIF
!
!
        A%tsCount         = 1
        A%FirstTimeActive = .FALSE.
      ENDIF  !( FirstTimeActive )
!--------------------------------------------------------
!
!
!
      IF( A%tsCount <=  A%nts ) THEN
!
        DO iCL=1, nGrainClass  !----- dump one time step -----
          DO i=1, F%nNodes
            iMesh = F%Node(i)     !> mesh index of field node
            dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &      + A%GrainClass(iCL) *F%dz(i) /DBLE(A%nts)
          ENDDO
        ENDDO !-----------------------------------------------
!
!
      ENDIF !( A%tsCount <=  A%nts )
!
      A%tsCount = A%tsCount + 1
!
      IF( A%tsCount  >  A%nts ) THEN !> The action is over
!
        DEALLOCATE( F%Z   , stat=status )
        DEALLOCATE( F%dZ  , stat=status )
        A%State = 9                        !> 9 = Action for ever inactive
        CALL InfoMessage( A, m, time )
!
!        A%TimeStart = A%TimeStart + (24.0D0 * 3600.0D0)    ! ACHTUNG  mor Faktor berucksichtigen
!        A%TimeEnd   = A%TimeEnd   + (24.0D0 * 3600.0D0)    ! ACHTUNG
!        A%State     = 0     !> Status of Action:  0 = not yet active ! ACHTUNG
!        A%FirstTimeActive = .TRUE.                         ! ACHTUNG
!
      ENDIF
!
!
!      dbug WRITE(6,*)'?>-------  SR Reset_bottom END -------------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Reset_bottom                !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
