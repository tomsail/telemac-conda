!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  open_File                      !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( fileName, FU, whatToDo )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY : ParallelComputing, ipid
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop
#endif
!
      IMPLICIT NONE
      CHARACTER(128), INTENT(IN)    :: fileName
      INTEGER,        INTENT(INOUT) :: FU         ! file unit
      CHARACTER,      INTENT(IN)    :: whatToDo
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
      CHARACTER (128)     :: fiName, chtmp, part2
      INTEGER             :: stat, unitTest
      LOGICAL             :: is_used
!
      ! 1. if write: create file names dependent on the current number of partition
      !    and open to write
      !     e.g.:  fileName =     _restart_Dig.dat
      !              =>       .../_restart_Dig.dat-0000   partititon 0
      !           or =>       .../_restart_Dig.dat-0001   partititon 1
      !                              :
      !           or =>       .../_restart_Dig.dat-0128   partititon 128
      !                              :
      !
      ! 2. open file to read or to write
!
!
!
!
!      dbug WRITE(6,*)'?>-------  SR open_File --------------------'
      SRname%s = "open_File"         ! subroutine name
!
      fiName = fileName              ! make a local copy of fileName
!
!
      IF( whatToDo == 'r' ) THEN   ! open to read ===============
        !IF(ParallelComputing) THEN
        !  WRITE(chtmp,*) ipid            ! write integer into buffer
        !  READ(chtmp,*) part2            ! readout string from buffer
        !  IF    ( ipid >= 0   .AND. ipid <= 9   ) THEN
        !    fiName = TRIM(Path)//TRIM(fiName)//"-000"//TRIM(part2)
        !  ELSEIF( ipid >= 10  .AND. ipid <= 99  ) THEN
        !    fiName = TRIM(Path)//TRIM(fiName)//"-00"//TRIM(part2)
        !  ELSEIF( ipid >= 100 .AND. ipid <= 999 ) THEN
        !    fiName = TRIM(Path)//TRIM(fiName)//"-0"//TRIM(part2)
        !  ELSE
        !    fiName = TRIM(Path)//TRIM(fiName)//"-"//TRIM(part2)
        !  ENDIF
        !ELSE
        !  fiName = TRIM(Path)//TRIM(fiName)
        !ENDIF
!
        !fiName = TRIM(Path)//TRIM(fiName)
         fiName =             TRIM(fiName)
        !WRITE(6,'("?>             fiName = ",A)') fiName    ! 
!        
!       
!       
        WRITE(*,*)'?> error        '
        WRITE(*,*)'?> error        '
        WRITE(*,*)'?> error Nestor should not open files to read.' 
        WRITE(*,*)'?> error This should be done by telemac or sysiphe.'
        WRITE(*,*)'?> error        '
        STOP
!
        OPEN( FU, FILE = fiName, STATUS = 'OLD'
     &          , IOSTAT = stat, ACTION = 'read' )
          !WRITE(*,*)'?>  while open returned status = ', stat       ! debug
!
      ENDIF  ! open to read
!
      IF( whatToDo == 'w' ) THEN   ! open to write ==============
        IF(ParallelComputing) THEN
          WRITE(chtmp,*) ipid            ! write integer into buffer
          READ(chtmp,*) part2            ! readout string from buffer
          IF    ( ipid >= 0   .AND. ipid <= 9   ) THEN
            fiName = TRIM(fiName)//"-000"//TRIM(part2)
          ELSEIF( ipid >= 10  .AND. ipid <= 99  ) THEN
            fiName = TRIM(fiName)//"-00"//TRIM(part2)
          ELSEIF( ipid >= 100 .AND. ipid <= 999 ) THEN
            fiName = TRIM(fiName)//"-0"//TRIM(part2)
          ELSE
            fiName = TRIM(fiName)//"-"//TRIM(part2)
          ENDIF
        ELSE
          fiName = TRIM(fiName)
        ENDIF
!
        !WRITE(6,'("?>             fiName = ",A)') fiName    ! debug
!        
!        
!        
        unitTest = 10
        DO
          INQUIRE(unitTest, OPENED=is_used)
          IF(.NOT.is_used) THEN
            FU = unitTest
            EXIT
          ENDIF
          unitTest = unitTest + 1
        ENDDO
!
!
        OPEN( FU, FILE = fiName, STATUS = 'REPLACE'
     &          , IOSTAT = stat, ACTION = 'write' )
      ENDIF  ! open to write
!
      IF( stat  /=  0 ) THEN
        Call ErrMsgAndStop( "while open file: "//TRIM(fiName)
     &  ," "," "," ", -1, SRname, ipid       )
      ENDIF
!
!      dbug WRITE(6,*)'?>-------  SR open_File END ----------------'
!
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE open_File                   !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
