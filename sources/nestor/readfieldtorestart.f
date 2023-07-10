!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ReadFieldToRestart             !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ()
!
      USE m_TypeDefs_Nestor
      USE m_Nestor, ONLY :  F, nFields, ipid, LuRstF, npoin
     &                     , waterLevel_saved_1, waterLevel_saved_2
     &                     , waterLevel_saved_3
!
!
      IMPLICIT NONE
!
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
      INTEGER :: n, j, fu, ilc  ! fu: file unit
      
      INTEGER :: partition
      CHARACTER (128) :: zeile
      REAL (KIND=R8)  ::  time          !  time [s]
!
!      dbug WRITE(6,*)'?>-------  SR ReadFieldToRestart -----------'
!
!     WRITE(6,*)'ipid     = ', ipid     ! debug   
!     WRITE(6,*)'ncsize   = ', ncsize   ! debug   
!
!
      fu = LuRstF  ! LuRstF: Logical Unit ReSTart File
      REWIND( fu )
!
!
      READ( fu,*) zeile
      DO WHILE( zeile(1:5) /= 'A-END' )  !> run over Action data to the
        READ( fu,*) zeile                !  position where Field data begins
      ENDDO
!
!
      ! === Read global variables ====
!
      READ( fu,'(A)') zeile
      READ(zeile(25:),*)        time
!     write(6,*) zeile    ! debug                    ! read line  1
!
      READ( fu,'(A)') zeile
      READ(zeile(25:),*)        nFields
!     write(6,*)   nFields    ! debug                    ! read line  2
!
      READ( fu,'(A)') zeile
!     write(6,*) zeile    ! debug                    ! read line  2
!
      READ( fu,'(A)') zeile
!     write(6,*) zeile    ! debug                    ! read line  2
!
      READ( fu,'(A)') zeile
!     write(6,*) zeile    ! debug                    ! read line  2
!
      READ( fu,'(A)') zeile
!     write(6,*) zeile    ! debug                    ! read line  2
!
      DO
        READ( fu,'(A)', END=900 ) zeile ! ipid
        IF(     zeile(2:5) == 'ipid'
     &     .OR. zeile(1:4) == 'ipid'  ) THEN
!         write(6,*) zeile                        ! debug
          READ(zeile(25:),*)  partition
          IF( partition == ipid )THEN
!           write(6,*)'partition =', partition   ! debug
            EXIT
          ENDIF !( partition == ipid )
        ENDIF !( zeile(2:5) == 'ipid' )            !
      ENDDO
!
      DO j=1, nFields
        READ(fu,'(A)') zeile                        ! Field name (and ignore)
!       write(6,*) zeile                                         ! debug
        READ(fu,'(A)') zeile
!       write(6,*) zeile                                         ! debug
        READ(zeile(25:),*)  n                       ! Field index value
!       write(6,*) 'n = ',n                                      ! debug
!
        READ(fu,'(A)') zeile
!       write(6,*) zeile                                         ! debug
        READ(zeile(25:),*)  F(n)%nNodeToDig         ! Field nNodeToDig value
!       write(6,*) 'F(n)%nNodeToDig = ',F(n)%nNodeToDig          ! debug
!
        READ(fu,'(A)') zeile                                     !             ! new Jan 2018
!       write(6,*) zeile                                         ! debug       ! new Jan 2018
        READ(zeile(25:),*)  F(n)%nNodeToDump        ! Field nNodeToDump value  ! new Jan 2018
!       write(6,*) 'F(n)%nNodeToDump = ',F(n)%nNodeToDump        ! debug       ! new Jan 2018
!
!
        DO
          READ(fu,*) zeile    ! identifier
!         write(6,*) zeile    ! debug
          SELECT CASE( zeile(1:5) )
           CASE( '____Z' ) !----------------------------------------
!            write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ALLOCATED( F(n)%Z) )
     &                 ALLOCATE( F(n)%Z( F(n)%nNodes ))
!            Read(fu,*)  F(n)%Z(:)
             Read(fu,*) (F(n)%Z(ilc), ilc=1, F(n)%nNodes)
             CYCLE
           CASE( '___Z0' ) !---------------------------------------- ! new Jan 2018
!            write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ALLOCATED( F(n)%Z0) )                          ! new Jan 2018
     &                 ALLOCATE( F(n)%Z0( F(n)%nNodes ))             ! new Jan 2018
!            Read(fu,*)  F(n)%Z0(:)                                  ! new Jan 2018
             Read(fu,*) (F(n)%Z0(ilc), ilc=1, F(n)%nNodes)
             CYCLE                                                   ! new Jan 2018
           CASE( '___dZ' ) !----------------------------------------
!            write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ALLOCATED( F(n)%dZ) )
     &                 ALLOCATE( F(n)%dZ( F(n)%nNodes ))
!            Read(fu,*)  F(n)%dZ(:)
             Read(fu,*) (F(n)%dZ(ilc), ilc=1, F(n)%nNodes)
             CYCLE
           CASE( 'dZ_To' ) !----------------------------------------
!            write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ALLOCATED( F(n)%dZ_Tot) )
     &                 ALLOCATE( F(n)%dZ_Tot( F(n)%nNodes ))
!            Read(fu,*)  F(n)%dZ_Tot(:)
             Read(fu,*) (F(n)%dZ_Tot(ilc), ilc=1, F(n)%nNodes)
             CYCLE
           CASE( '_refZ' ) !----------------------------------------
!            write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT.ALLOCATED( F(n)%refZ) )
     &                 ALLOCATE( F(n)%refZ( F(n)%nNodes ))
!            Read(fu,*)  F(n)%refZ(:)
             Read(fu,*) (F(n)%refZ(ilc), ilc=1, F(n)%nNodes)
             CYCLE
           CASE( '___km' ) !----------------------------------------
!            write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ALLOCATED( F(n)%km) )
     &                 ALLOCATE( F(n)%km( F(n)%nNodes ))
!            Read(fu,*)  F(n)%km(:)
             Read(fu,*) (F(n)%km(ilc), ilc=1, F(n)%nNodes)
             CYCLE
           CASE( 'targZ' ) !----------------------------------------
!            write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT.ALLOCATED( F(n)%targZ) )
     &                 ALLOCATE( F(n)%targZ( F(n)%nNodes ))
!            Read(fu,*)  F(n)%targZ(:)
             Read(fu,*) (F(n)%targZ(ilc), ilc=1, F(n)%nNodes)
             CYCLE
           CASE( 'ToDig' ) !----------------------------------------
!            write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ALLOCATED( F(n)%NodeToDig) )
     &                 ALLOCATE( F(n)%NodeToDig( F(n)%nNodes ))
!            Read(fu,*)  F(n)%NodeToDig(:)
             Read(fu,*) (F(n)%NodeToDig(ilc), ilc=1, F(n)%nNodes)
             CYCLE
           CASE( 'ToDum' ) !---------------------------------------- ! new Jan 2018
!            write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ALLOCATED( F(n)%NodeToDump) )                  ! new Jan 2018
     &                 ALLOCATE( F(n)%NodeToDump( F(n)%nNodes ))     ! new Jan 2018
!            Read(fu,*)  F(n)%NodeToDump(:)                          ! new Jan 2018
             Read(fu,*) (F(n)%NodeToDump(ilc), ilc=1, F(n)%nNodes)
             CYCLE                                                   ! new Jan 2018
           CASE DEFAULT    !----------------------------------------
!            write(6,*)'case:   DEFAULT'    ! debug
             EXIT
          END SELECT
        ENDDO
!
      ENDDO !  j=1, nFields
!
!
!
      DO                                                              ! new Jan 2018
        READ(fu,*) zeile    ! identifier WLev1 or WLev2 ...           ! new Jan 2018
!       write(6,*) zeile    ! debug                                   ! new Jan 2018
        SELECT CASE( zeile(1:5) )                                     ! new Jan 2018
!                                                                     ! new Jan 2018
          CASE( 'WLev1' ) !----------------------------------------   ! new Jan 2018
            IF(.NOT. ALLOCATED( waterLevel_saved_1) )                 ! new Jan 2018
     &                ALLOCATE( waterLevel_saved_1( npoin ))          ! new Jan 2018
!           Read(fu,*)  waterLevel_saved_1(:)                         ! new Jan 2018
            Read(fu,*) (waterLevel_saved_1(ilc), ilc=1, npoin)
            CYCLE                                                     ! new Jan 2018
          CASE( 'WLev2' ) !----------------------------------------   ! new Jan 2018
            IF(.NOT. ALLOCATED( waterLevel_saved_2) )                 ! new Jan 2018
     &                ALLOCATE( waterLevel_saved_2( npoin ))          ! new Jan 2018
!           Read(fu,*)  waterLevel_saved_2(:)                         ! new Jan 2018
            Read(fu,*) (waterLevel_saved_2(ilc), ilc=1, npoin)
            CYCLE                                                     ! new Jan 2018
          CASE( 'WLev3' ) !----------------------------------------   ! new Jan 2018
            IF(.NOT. ALLOCATED( waterLevel_saved_3) )                 ! new Jan 2018
     &                ALLOCATE( waterLevel_saved_3( npoin ))          ! new Jan 2018
!           Read(fu,*)  waterLevel_saved_3(:)                         ! new Jan 2018
            Read(fu,*) (waterLevel_saved_3(ilc), ilc=1, npoin)
            CYCLE                                                     ! new Jan 2018
          CASE( 'W-END' ) !----------------------------------------   ! new Jan 2018
            EXIT                                                      ! new Jan 2018
          CASE DEFAULT    !----------------------------------------   ! new Jan 2018
!           write(6,*)'case:   DEFAULT'    ! debug                    ! new Jan 2018
            EXIT                                                      ! new Jan 2018
!                                                                     ! new Jan 2018
        END SELECT                                                    ! new Jan 2018
      ENDDO                                                           ! new Jan 2018
!
!
!
!
!
900   CONTINUE
!
!      dbug WRITE(6,*)'?>-------  SR ReadFieldToRestart End -------'
       WRITE(6,*)'?>-------  SR ReadFieldToRestart End -------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE ReadFieldToRestart          !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
