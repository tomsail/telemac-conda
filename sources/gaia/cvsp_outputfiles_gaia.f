!                       ***********************
                        MODULE CVSP_OUTPUTFILES_GAIA
!                       ***********************
!
!***********************************************************************
! GAIA   V8P1                                   16/05/2017
!***********************************************************************
!
!>@brief
!
!>@history UWE MERKEL, P.A.TASSI (EDF R&D, LNHE)
!!        31/08/2013
!!        V6P3
!
!>@history UWE MERKEL, R. KOPMANN (BAW)
!!        V6P3 / V7P2
!!        12/03/2016 / 2017
!
!       NOTE: THIS MODULE IS ORGANISED IN 10 PARTS
!
!      01) VECTORS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!      02) MATRICES (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!      03) BLOCKS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!      04) INTEGERS
!      05) LOGICAL VALUES
!      06) REALS
!      07) STRINGS
!      08) SLVCFG STRUCTURES
!      09) MESH STRUCTURE
!      10) ALIASES
!
!
!       ALL BIEF_OBJ AND BIEF_MESH STRUCTURES ARE ALLOCATED
!       IN SUBROUTINE POINT_TELEMAC2D

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE BIEF_DEF
        USE DECLARATIONS_GAIA, ONLY: NSICLM

        IMPLICIT NONE

        INTEGER :: OUTPUTCOUNTER             = 0
        !> Number of check points
        INTEGER, PARAMETER :: NUMCP          = 30
        !> Number of check point variables to be written to file t2d_cp
        INTEGER, PARAMETER :: NUMVARCP2RES   = 3
        !> Number of user variables to be written to file t2d_ur
        INTEGER, PARAMETER :: NUMVARUR2RES   = 3
        !> Number of user variables to be written to file t2d_ur3d (3+nsicla)
        INTEGER, PARAMETER :: NUMVARUR3D2RES = 13
        !> 2d-hydraulic to 3d file z,u,v,w,scalaru,tau
        INTEGER, PARAMETER :: NUMVAR2DHYD    = 6
        !> Next cp slot to write into
        INTEGER NEXTCP
        !> Debug number of the deactivated check point variable
        INTEGER CP_ONCNT

        DOUBLE PRECISION USERTIME
        DOUBLE PRECISION USERPRINTCOUNT

        !> Logical for debug printouts
        LOGICAL       :: CP = .FALSE.

!
!-----------------------------------------------------------------------
!
!       1) VECTORS
!
!-----------------------------------------------------------------------
!
        TYPE(BIEF_OBJ), TARGET :: VSP_FRA(NSICLM)
        TYPE(BIEF_OBJ), TARGET :: VSP_D, VSP_D50, VSP_ERROR
        TYPE(BIEF_OBJ), TARGET :: UR2DHYD(NUMVAR2DHYD)
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!     BLOCK OF VARIABLES FOR OPTIONAL CHECKPOINT OUTPUT
      TYPE(BIEF_OBJ), TARGET :: CPBLOC
!     BLOCK OF VARIABLES FOR OPTIONAL USER OUTPUT
      TYPE(BIEF_OBJ), TARGET :: URBLOC
!     BLOCK OF VARIABLES FOR OPTIONAL USER OUTPUT IN 3D
      TYPE(BIEF_OBJ), TARGET :: URBLOC3D
!     BLOCK OF VARIABLES FOR OPTIONAL 2D TO 3D OUTPUT
      TYPE(BIEF_OBJ), TARGET :: URBLOC2DHYD
!
!-----------------------------------------------------------------------
!
!      8)  FILES
!
!-----------------------------------------------------------------------
!
      TYPE(BIEF_FILE) :: CP_FILES(3)   ! FILES FOR OPTIONAL OUTPUT

      CHARACTER(LEN=32) UR3D_FILES_LABELS(NUMVARUR3D2RES)
      LOGICAL      UR3D_FILES_OUTVAR(NUMVARUR3D2RES)

      CHARACTER(LEN=32) UR2DHYD_FILES_LABELS(NUMVAR2DHYD)
      LOGICAL      UR2DHYD_FILES_OUTVAR(NUMVAR2DHYD)
!
!-----------------------------------------------------------------------
!
!       9) MESH STRUCTURE
!
!-----------------------------------------------------------------------
!
!     MESH FOR 3D USEROUTPUT
      TYPE(BIEF_MESH) :: USRMSH
      TYPE(BIEF_MESH) :: USRMSH_2DHYD
      INTEGER USRMSH_NPLAN               ! NUMBER OF PLANES IN T3D_UR
      INTEGER USRMSH_2DHYD_NPLAN         ! NUMBER OF PLANES FOR 2DHYD TO 3D FILE
!
      SAVE
!-----------------------------------------------------------------------
!

      END MODULE CVSP_OUTPUTFILES_GAIA
