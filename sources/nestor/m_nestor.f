!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      MODULE      m_Nestor                       !******       SUBROUTINE  ********************
!**                                               *********************************************
!**                                               *********************************************
      USE m_TypeDefs_Nestor
!
!    __________________________________________________________________
!   /__________________________________________________________________\
!  //                                                                  \\
! <(              declaration part                                      )>
!  \\__________________________________________________________________//
!   \__________________________________________________________________/
      LOGICAL       :: ParallelComputing = .FALSE.
      LOGICAL       :: Restart           = .FALSE.
      LOGICAL       :: called_by_t2d     = .FALSE.
      LOGICAL       :: morphodynamic     = .FALSE.
      INTEGER       :: npoin             = 0
      INTEGER       :: npoinGlobal       = 0
      INTEGER       :: ncsize            = 0
      INTEGER       :: ipid              = 0   ! wird nur fuer Tests benoetigt
      INTEGER       :: nGrainClass       = 0
      INTEGER       :: GraphicOutputPeriod = 999999999
      INTEGER       :: LuActF            = -1
      INTEGER       :: LuPolF            = -1
      INTEGER       :: LuRefF            = -1
      INTEGER       :: LuRstF            = -1
      INTEGER       :: Lu                = -1
!
!
      REAL (KIND=R8):: MorpholFactor  = 0.0D0   ! morphological factor
!
      REAL (KIND=R8):: lim_dzts       = 0.1D0   ! [m]  max allowed change of bottom level (dz) 
!                                               !      per time step (ts)
!
      REAL (KIND=R8), PARAMETER   ::  eps = 0.0000000001D0
!
      CHARACTER(128)::Path = " "
!
      TYPE(t_Polygon),ALLOCATABLE,DIMENSION (:) ::  Poly
      INTEGER                                   :: nPolys
      TYPE(t_Action),ALLOCATABLE,DIMENSION  (:) ::  A
      INTEGER                                   :: nActions
      TYPE(t_Field),ALLOCATABLE,DIMENSION   (:) ::  F
      INTEGER                                   :: nFields
!
      TYPE( t_DateTime ) :: SisStart
!
!
!
      REAL (KIND=R8),ALLOCATABLE,DIMENSION  (:) :: waterLevel_saved_1  ! total grid
      REAL (KIND=R8),ALLOCATABLE,DIMENSION  (:) :: waterLevel_saved_2  ! total grid
      REAL (KIND=R8),ALLOCATABLE,DIMENSION  (:) :: waterLevel_saved_3  ! total grid
!
      REAL (KIND=R8),ALLOCATABLE,DIMENSION  (:) :: NodeArea            ! total grid  used only for testing
!
!***                                              ********************************************
!***                                              ********************************************
      END MODULE  m_Nestor                       !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
