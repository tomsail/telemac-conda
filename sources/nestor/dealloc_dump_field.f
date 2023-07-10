!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dealloc_Dump_Field             !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( A )
!
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  F
!
      IMPLICIT NONE
      TYPE(t_Action),INTENT(INOUT) :: A            !> Action
!
#ifndef  NESTOR_INTERFACES
      !--------------------- local variables ---------------
      INTEGER  :: n
!
!      dbug WRITE(6,*)'?>-------  SR Dealloc_Dump_Field -----------'
!
      IF( A%FieldDumpID > 0 ) THEN
        n = A%FieldDumpID
        IF( ALLOCATED( F(n)%Z )          ) DEALLOCATE(F(n)%Z)
        IF( ALLOCATED( F(n)%dZ )         ) DEALLOCATE(F(n)%dZ)
        IF( ALLOCATED( F(n)%dZ_Tot )     ) DEALLOCATE(F(n)%dZ_Tot)
        IF( ALLOCATED( F(n)%km )         ) DEALLOCATE(F(n)%km)
        IF( ALLOCATED( F(n)%refZ )       ) DEALLOCATE(F(n)%refZ)
        IF( ALLOCATED( F(n)%NodeToDump ) ) DEALLOCATE(F(n)%NodeToDump)
      ENDIF
!
!      dbug WRITE(6,*)'?>-------  SR Dealloc_Dump_Field END -------'
      RETURN
#endif
      END SUBROUTINE Dealloc_Dump_Field          !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
