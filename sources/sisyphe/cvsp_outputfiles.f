!                       ***********************
                        MODULE CVSP_OUTPUTFILES
!                       ***********************
!
!***********************************************************************
! SISYPHE   V7P2                                   16/05/2017
!***********************************************************************
!
!brief
!
!history UWE MERKEL, P.A.TASSI (EDF R&D, LNHE)
!+        31/08/2013
!+        V6P3
!
!history UWE MERKEL, R. KOPMANN (BAW)
!+        V6P3 / V7P2
!+        12/03/2016 / 2017
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
!|OUTPUTCOUNTER   |<--|
!|NUMCP           |<--| NUMBER OF CHECK POINTS
!|NUMVARCP2RES    |<--| NUMBER OF CHECK POINT VARIABLES TO BE WRITTEN TO FILE T2D_CP
!|NUMVARUR2RES    |<--| NUMBER OF USER VARIABLES TO BE WRITTEN TO FILE T2D_UR
!|NUMVARUR3D2RES  |<--| NUMBER OF USER VARIABLES TO BE WRITTEN TO FILE T2D_UR3D (3+NSICLA)
!|NUMVAR2DHYD     |<--| 2D-HYDRAULIC TO 3D FILE Z,U,V,W,SCALARU,TAU
!|NEXTCP          |<--| NEXT CP SLOT TO WRITE INTO
!|CP_ONCNT        |<--| DEBUG NUMBER OF THE DEACTIVATED CHECK POINT VARIABLE
!|USERTIME        |<--|
!|USERPRINTCOUNT  |<--|
!|CP              |<--| LOGICAL FOR DEBUG PRINTOUTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE BIEF_DEF
        USE DECLARATIONS_SISYPHE, ONLY: NSICLM

        IMPLICIT NONE

        INTEGER :: OUTPUTCOUNTER             = 0
        INTEGER, PARAMETER :: NUMCP          = 30
        INTEGER, PARAMETER :: NUMVARCP2RES   = 3
        INTEGER, PARAMETER :: NUMVARUR2RES   = 3
        INTEGER, PARAMETER :: NUMVARUR3D2RES = 13
        INTEGER, PARAMETER :: NUMVAR2DHYD    = 6
        INTEGER NEXTCP
        INTEGER CP_ONCNT

        DOUBLE PRECISION USERTIME
        DOUBLE PRECISION USERPRINTCOUNT

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

      END MODULE CVSP_OUTPUTFILES
