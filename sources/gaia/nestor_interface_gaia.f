!                       ********************************
                        SUBROUTINE NESTOR_INTERFACE_GAIA
!                       ********************************
!
     &(OPTION,GRAFCOUNT,XMVS0,XKV01,VOLU2D)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief This is the interface to nestor, containing all
!!           dependencies to nestor libraries
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] OPTION  1 : Initialisation (called in gaia)
!!                   2 : called every time step (from
!!                   bedload_posttreatment)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY: NESTOR,NPOIN,IELMH_GAI
     &                            ,GAI_FILES,SINACT,SINPOL
     &                            ,SINREF,SINRST
     &                            ,ZF, HN
     &                            ,MESH
     &                            ,LT, DT, AT0
     &                            ,MARDAT, MARTIM
     &                            ,ZFCL_C, AVAIL
     &                            ,ZRL
     &                            ,ZR, NSICLA, NOMBLAY
     &                            ,MOFAC, ES
     &                            ,EVCL_MB      !     evolution of mass of each sand class for laver-1 [ kg/m**2 ]
     &                            ,MASS_SAND    !     mass of each sand class for each laver           [ kg/m**2 ]
     &                            ,NSAND        !     number of sand classes
     &                            ,MPA2T        !     conversion factor MassPerArea to Thickness
!
      USE M_INTERFACES_NESTOR, ONLY: INTERFACEINITNESTOR,
     &                               INTERFACERUNNESTOR
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: OPTION, GRAFCOUNT ! period of graphical outputs
      DOUBLE PRECISION, INTENT(IN) :: XMVS0(NSICLA)     ! density sediment classes
      DOUBLE PRECISION, INTENT(IN) :: XKV01             ! non cohesive bed porosity of layer 1
      TYPE (BIEF_OBJ),  INTENT(IN) :: VOLU2D            ! node area
!
!--------------------- local variables ---------------
      LOGICAL             :: CALLEDBY_T2D
      INTEGER             :: I, ICLA
!
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      IF(OPTION.EQ.1) THEN    ! initialisation of Nestor
!
!     INITIALISATION
!
      CALLEDBY_T2D = .FALSE.

!
!
!
      CALL INTERFACEINITNESTOR(  NCSIZE, IPID              ! Number of parallel threads, ID of thread
     &                         , NPOIN, NSICLA             ! Number of: nodes, SIze CLAsses
     &                         , MARDAT, MARTIM            ! Sis start: date , time
     &                         , MOFAC                     ! morphological factor
     &                         , GRAFCOUNT                 ! period of graphical outputs
     &                         , MESH%X%R                  ! X-coordinate of node
     &                         , MESH%Y%R                  ! Y-coordinate of node
     &                         , VOLU2D%R                  ! node areas
     &                         , MAXVAL( MESH%KNOLG%I(:) ) ! local maxIndex, needed to calc. npion of global mesh
     &                         , ZF%R                      ! bottom at time 0 [m+NN]    Itera
     &                         , LU                        ! logical unit for standard output
     &                         , GAI_FILES(SINACT)%LU      ! logical unit to NESTOR ACTION FILE
     &                         , GAI_FILES(SINPOL)%LU      ! logical unit to NESTOR POLYGON FILE
     &                         , GAI_FILES(SINREF)%LU      ! logical unit to NESTOR SURFACE REFERENCE FILE
     &                         , GAI_FILES(SINRST)%LU      ! logical unit to NESTOR RESTART FILE
     &                         , CALLEDBY_T2D              ! true if coupled with telemac2d
     &                         , ZRL%R                     ! reference level [m+NN]
     &                        )
!
!
! LEOPRD aus T2D, GRAFCOUNT in gaia.f and sisyphe.F
!
!
! NO QUASI BUBBLE SUPPORTED
        IF(IELMH_GAI.NE.11) THEN
          WRITE(*,*)'ONLY LINEAR 2D ELEMENTS ARE SUPPORTED'
          WRITE(*,*)'NESTOR -STOP, PLEASE START WITHOUT'
          WRITE(*,*)'NESTOR OR USE ANOTHER ELEMENT TYPE'
          WRITE(*,*)'ACTUAL ELEMENT TYPE OF ZF: ',IELMH_GAI
          CALL PLANTE(1)
          STOP
        ENDIF
!
!
      !#########################################
      !
      ! Run Nestor
      !
      !#########################################
      ELSEIF(OPTION.EQ.2) THEN
!
!     CALL FROM WITHIN BEDLOAD_MAIN_GAIA
!
        IF (NESTOR.EQV..TRUE.) THEN
!
        IF(NSAND.NE.NSICLA)THEN
          WRITE(*,*)'A L A R M   there seems to be mud !'
          WRITE(*,*)'Coupling of Nestor with mud is not realized'
          WRITE(*,*)'stop in SUBROUTINE NESTOR_INTERFACE_GAIA'
          STOP
        ENDIF
!
!       !==== conversion of mass to thickness and fraction =============!
!       EVCL_MB  and  MASS_SAND   given in [ kg/m**2 ]
        DO ICLA = 1,NSICLA  ! convert for layer-1 for each sand class,
          DO I = 1,NPOIN
            ZFCL_C%ADR(ICLA)%P%R(I)                              !>  mass evolution to thickness evolution
     &         = EVCL_MB%ADR(ICLA)%P%R(I) * MPA2T(ICLA)        !   thickness evolution

            IF(ES(I,1).GT.0.D0) THEN
              AVAIL(I,1,ICLA)                                      !>  for layer-1
     &         = MASS_SAND(ICLA,1,I) * MPA2T(ICLA) / ES(I,1)   !   mass to fraction
            ELSE
              AVAIL(I,1,ICLA) = 0.D0
            ENDIF
          ENDDO
        ENDDO
!
          CALL INTERFACERUNNESTOR(  NPOIN      !  Number of POINts (nodes)
     &                            , NOMBLAY    !  Number of LAYers
     &                            , NSICLA     !  Number of SIze CLAsses
     &                            , LT         !  Telemac time step
     &                            , DT         !  duration of Sisyphe time step
     &                            , AT0        !  time
     &                            , ES(1:NPOIN,1)   !(non const.) thickness of active laver [m]
     &                            , ZF%R       !  bottom [m+NN]
     &                            , ZFCL_C     !  evolution per class per time step [m]
     &                            , AVAIL      !  grain composition (part of grainClass per node per layer)
     &                            , MESH%KNOLG%I    ! index list: Local to Global node index
     &                            , HN%R       !  water depth [m]
     &                            , ZR%R       !  ridged bed  [m+NN]
     &                            , ZRL%R      ! reference level [m+NN]
     &                           )
        ENDIF
!
!       !==== conversion of thickness to mass  ========================!
        DO ICLA = 1,NSICLA  ! convert for each class thickness evolution to mass evolution
          DO I = 1,NPOIN
            EVCL_MB%ADR(ICLA)%P%R(I)
     &         =  ZFCL_C%ADR(ICLA)%P%R(I) / MPA2T(ICLA)
          ENDDO
        ENDDO
!
!
      ELSE  ! OPTION is neither 1 nor 2
!
!       ERROR
!
        IF(LNG.EQ.1) WRITE(LU,*) 'MAUVAISE OPTION POUR NESTOR'
        IF(LNG.EQ.2) WRITE(LU,*) 'BAD OPTION FOR NESTOR'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
