!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  WriteFieldToRestart            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( time, fileName )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor, ONLY :  F, nFields, ipid  , waterLevel_saved_1
     &                    , waterLevel_saved_2, waterLevel_saved_3
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  open_File
#endif
!
!
      IMPLICIT NONE
!
      REAL (KIND=R8),INTENT(IN)    :: time          !  time [s]
      CHARACTER(128),INTENT(IN)    :: fileName
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      INTEGER :: n, fu  ! fu: file unit
!
!      dbug WRITE(6,*)'?>-------  SR WriteFieldToRestart ----------'
!
!
      CALL open_File( fileName, fu, 'w' )        ! open File
!
!      === Write global variables ====
      WRITE(fu,*)'info text: time      =    ', time              ! 1
      WRITE(fu,*)'nFields              =    ', nFields           ! 2
      WRITE(fu,*)'xx                   =    ', -11               ! 3
      WRITE(fu,*)'xx                   =    ', -11               ! 4
      WRITE(fu,*)'xx                   =    ', -11               ! 5
      WRITE(fu,*)'xx                   =    ', -11               ! 6
!
      WRITE(fu,*)'ipid                 =    ', ipid
!
      DO n=1, nFields
        WRITE(fu,'(" Field name           =    ",A)') F(n)%Name
!
        WRITE(fu,*)'Field index          =    ', n
        WRITE(fu,*)'Field nNodeToDig     =    ', F(n)%nNodeToDig
        WRITE(fu,*)'Field nNodeToDump    =    ', F(n)%nNodeToDump  ! new Jan 2018  
        IF( ALLOCATED(F(n)%Z) )THEN
          WRITE(fu,*)"____Z"
          WRITE(fu,*)  F(n)%Z(:)
        ENDIF
        IF( ALLOCATED(F(n)%Z0) )THEN             ! new Jan 2018
          WRITE(fu,*)"___Z0"                     ! new Jan 2018
          WRITE(fu,*)  F(n)%Z0(:)                ! new Jan 2018
        ENDIF                                    ! new Jan 2018
        IF( ALLOCATED(F(n)%dZ) )THEN
          WRITE(fu,*)"___dZ"
          WRITE(fu,*) F(n)%dZ(:)
        ENDIF
        IF( ALLOCATED(F(n)%dZ_Tot) )THEN
          WRITE(fu,*)"dZ_To"
          WRITE(fu,*) F(n)%dZ_Tot(:)
        ENDIF
        IF( ALLOCATED(F(n)%refZ) )THEN
          WRITE(fu,*)"_refZ"
          WRITE(fu,*) F(n)%refZ(:)
        ENDIF
        IF( ALLOCATED(F(n)%km) )THEN
          WRITE(fu,*)"___km"
          WRITE(fu,*) F(n)%km(:)
        ENDIF
        IF( ALLOCATED(F(n)%targZ) )THEN
          WRITE(fu,*)"targZ"
          WRITE(fu,*) F(n)%targZ(:)
        ENDIF
        IF( ALLOCATED(F(n)%NodeToDig) )THEN
          WRITE(fu,*)"ToDig"
          WRITE(fu,*) F(n)%NodeToDig(:)
        ENDIF
        IF( ALLOCATED(F(n)%NodeToDump) )THEN    ! new Jan 2018
          WRITE(fu,*)"ToDum"                    ! new Jan 2018
          WRITE(fu,*) F(n)%NodeToDump(:)        ! new Jan 2018
        ENDIF                                   ! new Jan 2018
!
!
        WRITE(fu,*)"F-END"
!
      ENDDO  ! n=1, nFields
!
!
      IF( ALLOCATED(waterLevel_saved_1) )THEN  ! new Jan 2018
        WRITE(fu,*)"WLev1"                     ! new Jan 2018
        WRITE(fu,*) waterLevel_saved_1(:)      ! new Jan 2018
      ENDIF                                    ! new Jan 2018
      IF( ALLOCATED(waterLevel_saved_2) )THEN  ! new Jan 2018
        WRITE(fu,*)"WLev2"                     ! new Jan 2018
        WRITE(fu,*) waterLevel_saved_2(:)      ! new Jan 2018
      ENDIF                                    ! new Jan 2018
      IF( ALLOCATED(waterLevel_saved_3) )THEN  ! new Jan 2018
        WRITE(fu,*)"WLev3"                     ! new Jan 2018
        WRITE(fu,*) waterLevel_saved_3(:)      ! new Jan 2018
      ENDIF                                    ! new Jan 2018
!
      WRITE(fu,*)"W-END"
!
      CLOSE(fu)
!      dbug WRITE(6,*)'?>-------  SR WriteFieldToRestart End ------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE WriteFieldToRestart         !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
