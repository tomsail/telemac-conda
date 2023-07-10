!                   **********************
                    SUBROUTINE USER_SOURCE
!                   **********************
!
     & (S0U,S0V,S0W,S1U,S1V,S1W,
     &  UN3,VN3,WSN3,WN3,
     &  VOLU,VOLUN,T3,NPOIN3,NTRAC,LT,AT,DT,PRIVE,NONHYD,
     &  NPOIN2,NSCE,ISCE,KSCE,QSCE,USCE,VSCE,MAXSCE)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    PREPARES SOURCE TERMS FOR DIFFUSION OF TRACERS.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| DT             |-->| TIME STEP
!| ISCE           |-->| NODE ADRESSES IN 2D MESH FOR SOURCES
!| KSCE           |<->| NUMBER OF PLANE FOR SOURCES
!| LT             |-->| ITERATION NUMBER
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| NONHYD         |-->| LOGICAL FOR NON-HYDROSTATIC OPTION
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN THE MESH
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| NTRAC          |-->| NUMBER OF TRACERS
!| PRIVE          |-->| BLOCK OF ARRAYS FOR USER
!| QSCE           |-->| WATER DISCHARGE OF SOURCES
!| S0U            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES U
!| S0V            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES V
!| S0W            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES W
!| S1U            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES U
!| S1V            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES V
!| S1W            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES W
!| T3             |<->| WORK ARRAY: NOT USED
!| UN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| USCE           |-->| VELOCITY FOR SOURCE
!| VN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| VOLUN          |-->| VOLUME AROUND POINTS AT TIME N
!| VSCE           |-->| VELOCITY FOR SOURCE
!| WN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| WSN3           |-->| SIGMA-TRANSFORMED VERTICAL VELOCITY COMPONENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN3, NTRAC, LT, MAXSCE
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: UN3, VN3, WSN3, WN3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: S0U, S0V, S1U, S1V, S0W, S1W
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T3
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU, VOLUN,PRIVE
!
      DOUBLE PRECISION, INTENT(IN)  :: AT,DT
      LOGICAL, INTENT(IN)           :: NONHYD
!
      INTEGER, INTENT(IN)           :: NPOIN2
      INTEGER, INTENT(IN)           :: NSCE
      INTEGER, INTENT(IN)           :: ISCE(NSCE)
      INTEGER, INTENT(IN)           :: KSCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)  :: QSCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)  :: USCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)  :: VSCE(NSCE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!     BEWARE : BE SURE TO DO S0U = S0U + YOUR SOURCE TERMS
!              BECAUSE S0U HAS ALREADY BEEN INITIALISED IN TRISOU
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
