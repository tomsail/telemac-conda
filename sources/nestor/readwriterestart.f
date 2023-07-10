!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ReadWriteRestart               !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( time, whatToDo )
!
      USE m_TypeDefs_Nestor, ONLY : R8
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ReadActionToRestart
     &                               , ReadFieldToRestart
     &                               , WriteActionToRestart
     &                               , WriteFieldToRestart
#endif
!
      IMPLICIT NONE
!
      REAL (KIND=R8),INTENT(IN)    :: time          !  time [s]
      CHARACTER(8)  ,INTENT(IN)    :: whatToDo
!
#ifndef NESTOR_INTERFACES
      !------- local variables ---------------
!
      CHARACTER (128) :: fileName
!
!      dbug WRITE(6,*)'?>-------  SR ReadWriteRestart -------------'
!
      IF( whatToDo(1:4) == 'read' ) THEN  ! read one restart file
!
        !fileName = "_restart_Nestor.dat"
!        fileName = "DSCFG1"
!        CALL ReadActionToRestart( fileName )
!        CALL ReadFieldToRestart(  fileName )
        CALL ReadActionToRestart()
        CALL ReadFieldToRestart()
      ENDIF
!
      IF( whatToDo(1:5) == 'write' ) THEN ! write two restart files
!
        fileName = "_restart_DigActions.dat"
        CALL WriteActionToRestart( time, fileName )
!
        fileName = "_restart_FieldData.dat"
         CALL WriteFieldToRestart(  time, fileName )
      ENDIF
!
!      dbug WRITE(6,*)'?>-------  SR ReadWriteRestart End ---------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE ReadWriteRestart            !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************