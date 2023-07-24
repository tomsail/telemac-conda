!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  WriteField                     !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( F )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor, ONLY : ipid, t_Field
!
!
      IMPLICIT NONE
      TYPE(t_Field),INTENT(IN) :: F
!
!
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      INTEGER :: i
!
!663   FORMAT(' ?>',2(/,' ?>'))            ! 3 lines like "?>         "
!      dbug WRITE(6,*)'?>-------  SR WriteField -------------------'
!
      WRITE(6,*)'?> F%name    = ',ipid,F%name
      WRITE(6,*)'?> F%FieldID = ',ipid,F%FieldID
      WRITE(6,*)'?> F%nNodes  = ',ipid,F%nNodes
      WRITE(6,*)'?> F%Area    = ',ipid,F%Area
      DO i=1, F%nNodes
         WRITE(6,*)'?> F%Node = ', ipid, F%Node(i), i
!     &                     , i, MESH%KNOLG%I( F%Node(i) )
      ENDDO
      WRITE(6,*)'?> F%nIntFacNodes = ',F%nIntFacNodes
      DO i=1, F%nIntFacNodes
         WRITE(6,*)'?> F%IntFacNode%Index = ',F%IntFacNode(i)%Index
     &                      ,' nPartit = ',F%IntFacNode(i)%nNeighbPart
      ENDDO
!
!      dbug WRITE(6,*)'?>-------  SR WriteField End ---------------'
!110   FORMAT('Iam:',I2,I3,' NeighbP:',I3,
!     &                     ' nNodes:',I3,' N:',8I4,' ...')
       RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE WriteField                  !********************************************
!**                                               ********************************************
!**                                               ********************************************
!*********************************************************************************************
!*********************************************************************************************