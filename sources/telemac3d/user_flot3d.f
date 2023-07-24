!                   **********************
                    SUBROUTINE USER_FLOT3D
!                   **********************
!
     &(XFLOT,YFLOT,ZFLOT,NFLOT,NFLOT_MAX,X,Y,Z,IKLE,NELEM,NELMAX,NPOIN,
     & NPLAN,TAGFLO,CLSFLO,SHPFLO,SHZFLO,ELTFLO,ETAFLO,MESH3D,LT,NIT,AT)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    This subroutine is called at every time step, and the user can
!+        add or remove particles as in the example given
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        26/02/2013
!+        V6P3
!+    First version.
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Creation from FLOT3D
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
      USE STREAMLINE, ONLY : ADD_PARTICLE,DEL_PARTICLE
      USE INTERFACE_TELEMAC3D, EX_USER_FLOT3D => USER_FLOT3D
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
!     IF(LT.LE.600.AND.(10*(LT/10).EQ.LT.OR.LT.EQ.1)) THEN
!       CALL ADD_PARTICLE(-220.D0,400.D0+LT/3.D0,259.D0+LT/100.D0,
!    &                    LT,1,NFLOT,
!    &                    NFLOT_MAX,XFLOT,YFLOT,ZFLOT,TAGFLO,CLSFLO,
!    &                    SHPFLO,SHZFLO,ELTFLO,ETAFLO,MESH3D,NPLAN,
!    &                    0.D0,0.D0,0.D0,0.D0,0,0)
!     ENDIF
!
!     IF(LT.EQ.600) THEN
!        CALL DEL_PARTICLE(20,NFLOT,NFLOT_MAX,
!    &                     XFLOT,YFLOT,ZFLOT,TAGFLO,CLSFLO,SHPFLO,SHZFLO,
!    &                     ELTFLO,ETAFLO,MESH%TYPELM)
!     ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
