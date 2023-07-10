!                       ******************************
                        SUBROUTINE NESTOR_INTERFACE
!                       ******************************
!
     &(OPTION)
!
!***********************************************************************
! SISYPHE VERSION
!
!
!***********************************************************************
!
!  FUNCTION: THIS IS THE INTERFACE TO NESTOR, CONTAINING ALL
!            DEPENDENCIES TO NESTOR LIBRARIES
!
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   OPTION       | -->| 1 : INITIALISATION (CALLED IN SISYPHE)
! |                |    | 2 : CALLED EVERY TIME STEP (FROM
! |                |    |     BEDLOAD_MAIN)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! PROGRAMME APPELANT : SISYPHE, BEDLOAD_MAIN
! PROGRAMMES APPELES :
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : NESTOR,NPOIN,IELMH_SIS
     &                                ,SIS_FILES,SINACT,SINPOL
     &                                ,SINREF,SINRST
     &                                ,ZF, HN
     &                                ,MESH
     &                                ,LT, DT, AT0, LEOPR
     &                                ,MARDAT, MARTIM
     &                                ,ZFCL_C,AVAIL
     &                                ,ZRL
     &                                ,ZR, NSICLA, NOMBLAY
     &                                ,MOFAC, DT, NSOUS, ES, VOLU2D
!
      USE M_INTERFACES_NESTOR, ONLY: INTERFACEINITNESTOR,
     &                               INTERFACERUNNESTOR
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: OPTION
      DOUBLE PRECISION    :: DTS
      LOGICAL             :: CALLEDBY_T2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      IF(OPTION.EQ.1) THEN
!
!     INITIALISATION
!
      CALLEDBY_T2D = .FALSE.
!
!
      CALL INTERFACEINITNESTOR(  NCSIZE, IPID, NPOIN
     &                         , NSICLA
     &                         , MARDAT, MARTIM ! Sis start: date , time
     &                         , MOFAC          ! morphological factor
     &                         , LEOPR          ! period of graphical outputs
     &                         , MESH%X%R
     &                         , MESH%Y%R
     &                         , VOLU2D%R                 ! node area
     &                         , MAXVAL( MESH%KNOLG%I(:) )
     &                         , ZF%R                     !  bottom at time 0 [m+NN]
     &                         , LU                       ! logical unit for standard output
     &                         , SIS_FILES(SINACT)%LU     ! logical unit to NESTOR ACTION FILE
     &                         , SIS_FILES(SINPOL)%LU     ! logical unit to NESTOR POLYGON FILE
     &                         , SIS_FILES(SINREF)%LU     ! logical unit to NESTOR SURFACE REFERENCE FILE
     &                         , SIS_FILES(SINRST)%LU     ! logical unit to NESTOR RESTART FILE
     &                         , CALLEDBY_T2D
     &                         , ZRL%R                    ! reference level [m+NN]
     &                        )
!
!
! LEOPRD aus T2D, GRAFCOUNT in sisyphe.f
! LEOPR = GRAFCOUNT in sisyphe.F
!
!
! NO QUASI BUBBLE SUPPORTED
        IF(IELMH_SIS.NE.11) THEN
          WRITE(*,*)'ONLY LINEAR 2D ELEMENTS ARE SUPPORTED'
          WRITE(*,*)'NESTOR -STOP, PLEASE START WITHOUT'
          WRITE(*,*)'NESTOR OR USE ANOTHER ELEMENT TYPE'
          WRITE(*,*)'ACTUAL ELEMENT TYPE OF ZF: ',IELMH_SIS
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
!     CALL FROM WITHIN BEDLOAD_MAIN
!
        IF (NESTOR.EQV..TRUE.) THEN
!
          IF( NSICLA .EQ. 1) THEN           !> in case of single grain simulation
            ES(:,1) = ZF%R(:) - ZR%R(:)     !  layer thickness is not set by sisyphe
          ENDIF
!
          DTS = DT/NSOUS
          CALL INTERFACERUNNESTOR(  NPOIN      !  Number of POINts (NODES)
     &                            , NOMBLAY    !  Number of LAYers
     &                            , NSICLA     !  Number of SIze CLAsses
     &                            , LT         !  Telemac time step
     &                            , DTS        !  duration of Sisyphe time step
     &                            , AT0        !  time
     &                            , ES(1:NPOIN,1)   !(non const.) thickness of active laver [m]
     &                            , ZF%R       !  bottom [m+NN]
     &                            , ZFCL_C     !  evolution per class per time step [m]
     &                            , AVAIL      !  grain composition (part of grainClass per node per layer)
     &                            , MESH%KNOLG%I    ! index list: Local to Global node index
     &                            , HN%R       !  water depth [m]
     &                            , ZR%R       !  ridged bed  [m+NN]
     &                            , ZRL%R      !  reference level [m+NN]
     &                           )
        ENDIF
!
!
      ELSE
!
!     ERROR
!
        WRITE(LU,*) 'BAD OPTION FOR NESTOR'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
