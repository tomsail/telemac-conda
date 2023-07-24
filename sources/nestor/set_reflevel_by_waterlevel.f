!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Set_RefLevel_by_Waterlevel     !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( F, A, m )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  ipid
     &                     , waterLevel_saved_1
     &                     , waterLevel_saved_2
     &                     , waterLevel_saved_3
!
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop
#endif
!
      IMPLICIT NONE
!
      TYPE(t_Field)  ,INTENT(INOUT) :: F
      TYPE(t_Action) ,INTENT(IN)    :: A
      INTEGER        ,INTENT(IN)    :: m  !> number of Action
!
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!      CHARACTER (128)              :: fileName     ! file containing profiles for reference level
!
      INTEGER         :: i, iMesh
!
!      REAL (KIND=R8)  :: r, angleABCD                         ! relation, angle(AB,CD), angle(SA,SN)
!      !REAL (KIND=R8):: r2d = 565.48667764602D0 ! = 360/2*Pi conversion factor: radian to degree ! debug only
!
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
!
!      dbug WRITE(6,*)'?>-------  SR Set_RefLevel_by_Waterlevel ---'
      SRname%s = "Set_RefLevel_by_Waterlevel"                 ! subroutine name
!
       IF( A%ReferenceLevel(1:9) == 'WATERLVL1'  .AND.
     &      .NOT.ALLOCATED(waterLevel_saved_1)) Call ErrMsgAndStop(
     &    "while Action  Dig_by_Criterion                       "
     &   ,"reason:    ReferezLevel Water_level-1 has no values. "
     &   ,"           ==> Save water level before using it.     "
     &   ,"occured in Action: ", m, SRname, ipid      )
!
       IF( A%ReferenceLevel(1:9) == 'WATERLVL2'  .AND.
     &    .NOT.ALLOCATED(waterLevel_saved_2)) Call ErrMsgAndStop(
     &    "while Action  Dig_by_Criterion                       "
     &   ,"reason:    ReferezLevel Water_level-2 has no values. "
     &   ,"           ==> Save water level before using it.     "
     &   ,"occured in Action: ", m, SRname, ipid      )
!
       IF( A%ReferenceLevel(1:9) == 'WATERLVL3'  .AND.
     &    .NOT.ALLOCATED(waterLevel_saved_3)) Call ErrMsgAndStop(
     &    "while Action  Dig_by_Criterion                       "
     &   ,"reason:    ReferezLevel Water_level-3 has no values. "
     &   ,"           ==> Save water level before using it.     "
     &   ,"occured in Action: ", m, SRname, ipid      )
!
       IF(      A%ReferenceLevel(1:9) == 'WATERLVL1') THEN
         DO i=1, F%nNodes
           iMesh     = F%Node(i)      ! mesh index of field node
           F%refZ(i) = waterLevel_saved_1(iMesh)
         ENDDO
       ELSE IF( A%ReferenceLevel(1:9) == 'WATERLVL2') THEN
         DO i=1, F%nNodes
           iMesh     = F%Node(i)      ! mesh index of field node
           F%refZ(i) = waterLevel_saved_2(iMesh)
         ENDDO
       ELSE IF( A%ReferenceLevel(1:9) == 'WATERLVL3') THEN
         DO i=1, F%nNodes
           iMesh     = F%Node(i)      ! mesh index of field node
           F%refZ(i) = waterLevel_saved_3(iMesh)
         ENDDO
       ENDIF
!
!
!      dbug WRITE(6,*)'?>-------  SR Set_RefLevel_by_Waterlevel End'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Set_RefLevel_by_Waterlevel  !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
