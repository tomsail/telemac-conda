!                   ********************
                    SUBROUTINE MESH_PROP
!                   ********************
!
     &(HPROP,HN,H,PROLIN,HAULIN,TETA,NSOUSI,ZPROP,
     & IPBOT,NPOIN2,NPLAN,OPTBAN,SIGMAG,OPT_HNEG,
     & MESH3D,VOLU3D,VOLU3DPAR,UNSV3D,MASKEL,IELM3)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES THE MESH FOR THE PROPAGATION STEP.
!
!note     THE BOTTOM OF ZPROP MUST BE DONE BEFORE.
!
!history  J-M HERVOUET (LNHE)
!+        20/05/2010
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (LNHE)
!+        20/05/2010
!+        V6P0
!+   Inverse of VOLU3DPAR differently clipped
!
!history  J-M HERVOUET (LNHE)
!+        30/09/2013
!+        V6P3
!+   Parameter MINIMUM_VOLUME introduced.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| H              |-->| WATER DEPTH
!| HAULIN         |-->| MEAN DEPTH FOR LINEARISATION
!| HN             |-->| WATER DEPTH AT TIME N
!| HPROP          |<->| PROPAGATION DEPTH (DONE IN CVDFTR)
!| IELM3          |-->| TYPE OF ELEMENT
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |<->| 3D MESH
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NSOUSI         |-->| SUB-ITERATIONS NUMBER
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| OPT_HNEG       |-->| OPTION FOR NEGATIVE DEPTHS
!| PROLIN         |-->| CORRESPOND TO KEYWORD "LINEARIZED PROPAGATON"
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!| TETA           |-->| SEMI-IMPLICITATION FOR H
!| UNSV3D         |<->| INVERSE OF VOLUME OF BASIS FUNCTIONS
!| VOLU3D         |<->| INTEGRAL OF TEST FUNCTIONS IN 3D
!| VOLU3DPAR      |<->| VOLU3D FOR EACH DOMAIN IN PARALLEL MODE
!| ZPROP          |<->| Z DURING PROPAGATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY : AGGLOH,MINIMUM_VOLUME
      USE INTERFACE_TELEMAC3D, EX_MESH_PROP => MESH_PROP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NSOUSI,NPOIN2,NPLAN,OPT_HNEG
      INTEGER, INTENT(IN)            :: OPTBAN,IELM3
      LOGICAL, INTENT(IN)            :: PROLIN,SIGMAG
      DOUBLE PRECISION, INTENT(IN)   :: TETA,HAULIN
      TYPE(BIEF_OBJ),   INTENT(IN)   :: HN,H,MASKEL
      TYPE(BIEF_OBJ),   INTENT(INOUT):: IPBOT
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: HPROP,ZPROP,UNSV3D,VOLU3D
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: VOLU3DPAR
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!-----------------------------------------------------------------------
!
      IF(PROLIN) THEN
        CALL OS( 'X=C     ' , X=HPROP , C=HAULIN    )
      ELSEIF(NSOUSI.EQ.1) THEN
        CALL OS( 'X=Y     ' , X=HPROP , Y=HN )
      ELSE
        CALL OS( 'X=CY    ' , X=HPROP , Y=HN , C=1.D0-TETA )
        CALL OS( 'X=X+CY  ' , X=HPROP , Y=H  , C= TETA )
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CLIPS HPROP
!
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.NE.2) THEN
        CALL OS('X=+(Y,C)',X=HPROP,Y=HPROP,C=0.D0)
      ENDIF
!
!     COMPUTES THE NEW MESH
!
      CALL CALCOT(ZPROP%R,HPROP%R)
!
!     COMPUTES THE REAL BOTTOM PLANE
!
      CALL PLANE_BOTTOM(IPBOT%I,ZPROP%R,NPOIN2,NPLAN,SIGMAG,OPTBAN)
!
!     COMPUTES THE INVERSE OF VOLUME OF BASIS FUNCTIONS 3D IN UNSV3D
!
!     ZPROP IS TEMPORARILY PUT IN MESH3D%Z
!
      SAVEZ     =>MESH3D%Z%R
      MESH3D%Z%R=>ZPROP%R
!
!     6.2 NEW COMPUTATION COMPATIBLE WITH CONTINUITY EQUATION
!         LUMPING DONE LIKE FOR CONTINUITY EQUATIONS
!
      CALL VECTOR(VOLU3D, '=', 'MASBAS          ',IELM3,1.D0-AGGLOH,
     &  ZPROP,ZPROP,ZPROP,ZPROP,ZPROP,ZPROP,MESH3D,.FALSE.,MASKEL)
      IF(AGGLOH.GT.1.D-6) THEN
        CALL VECTOR(VOLU3D, '+', 'MASBAS2         ',IELM3,AGGLOH,
     &  ZPROP,ZPROP,ZPROP,ZPROP,ZPROP,ZPROP,MESH3D,.FALSE.,MASKEL)
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=Y     ',X=VOLU3DPAR,Y=VOLU3D)
        CALL PARCOM(VOLU3DPAR,2,MESH3D)
        CALL OS('X=1/Y   ',X=UNSV3D,Y=VOLU3DPAR,
     &          IOPT=2,INFINI=1.D0/MINIMUM_VOLUME,ZERO=MINIMUM_VOLUME)
!               VERSION 6.1
!    &          IOPT=2,INFINI=0.D0,ZERO=MINIMUM_VOLUME)
      ELSE
        CALL OS('X=1/Y   ',X=UNSV3D,Y=VOLU3D,
     &          IOPT=2,INFINI=1.D0/MINIMUM_VOLUME,ZERO=MINIMUM_VOLUME)
!               VERSION 6.1
!    &          IOPT=2,INFINI=0.D0,ZERO=MINIMUM_VOLUME)
      ENDIF
!
!     RESTORES Z
!
      MESH3D%Z%R=>SAVEZ
!
!-----------------------------------------------------------------------
!
      RETURN
      END
