!                   *****************
                    SUBROUTINE FLOT3D
!                   *****************
!
     &(XFLOT,YFLOT,ZFLOT,NFLOT,NFLOT_MAX,X,Y,Z,IKLE,NELEM,NELMAX,NPOIN,
     & NPLAN,TAGFLO,CLSFLO,SHPFLO,SHZFLO,ELTFLO,ETAFLO,MESH3D,LT,NIT,AT)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    DEPRECATED. Use USER_FLOT3D instead. Kept for retro
!+        compatibility
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        26/02/2013
!+        V6P3
!+    First version.
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Call of USER_FLOT3D User Fortran where the modifications are done
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| CLSFLO         |<->| CLASS FOR EACH FLOAT
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| ETAFLO         |<->| LEVELS WHERE ARE THE FLOATS
!| IKLE           |-->| CONNECTIVITY TABLE
!| LT             |-->| CURRENT TIME STEP
!| MESH3D         |<->| 3D MESH STRUCTURE
!| NELEM          |-->| NUMBER OF ELEMENT
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NIT            |-->| NUMBER OF TIME STEPS
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR
!|                |   | ELEMENTS.
!| SHZFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR LEVEL
!| TAGFLO         |<->| TAG FOR EACH FLOAT
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XFLOT          |<->| ABSCISSAE OF FLOATING BODIES
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YFLOT          |<->| ORDINATES OF FLOATING BODIES
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!| ZFLOT          |<->| ELEVATIONS OF FLOATING BODIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_FLOT3D => FLOT3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NIT,NFLOT_MAX,LT,NPLAN
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*)
      INTEGER, INTENT(INOUT)          :: NFLOT
      INTEGER, INTENT(INOUT)          :: TAGFLO(NFLOT_MAX)
      INTEGER, INTENT(INOUT)          :: CLSFLO(NFLOT_MAX)
      INTEGER, INTENT(INOUT)          :: ELTFLO(NFLOT_MAX)
      INTEGER, INTENT(INOUT)          :: ETAFLO(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),Z(NPOIN),AT
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: ZFLOT(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPFLO(3,NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZFLO(NFLOT_MAX)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      ! USER FUNCTION
      CALL USER_FLOT3D
     &(XFLOT,YFLOT,ZFLOT,NFLOT,NFLOT_MAX,X,Y,Z,IKLE,NELEM,NELMAX,NPOIN,
     & NPLAN,TAGFLO,CLSFLO,SHPFLO,SHZFLO,ELTFLO,ETAFLO,MESH3D,LT,NIT,AT)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
