!                   *********************
                    SUBROUTINE T3D_STRESS
!                   *********************
!
     &(SEM3D,OP,T2,T3,BFBORL,BFBORF,BFBORS,NPOIN2,NPOIN3,MESH2D,
     & MESH3D,IELM2H,IELM2V,SV,MSK,MASKBR,MASKEL,IPBOT,SIGMAG,
     & OPTBAN,NPLAN)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   03/06/2014
!***********************************************************************
!
!brief    ADDS TO OR REMOVES FROM SEM3D THE EXPLICIT STRESS
!+               (DEPENDING ON OP).
!
!
!
!history  J.M. HERVOUET (LNHE)
!+        20/07/05
!+        V5P6
!+  First version.
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
!history  J.M. HERVOUET (LNHE)
!+        26/09/2011
!+        V6P2
!+   Replaces subroutine STRESS to avoid a duplication with Tomawac.
!
!history  J.M. HERVOUET (EDF LAB, LNHE)
!+        04/06/2014
!+        V7P0
!+   Tidal flats and crushed planes treated.
!
!history  J. FONTAINE (EDF LAB, LNHE)
!+        19/11/2019
!+        V8P1
!+   Fix inconsistency between treatment of implicit and explicit
!+   boundary conditions (MASVEC was used here instead of MASBAS used
!+   in the matrix.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BFBORF         |---| LOGARITHMIC LAW FOR COMPONENT ON THE BOTTOM:
!|                |---|  NU*DF/DN = AFBORF*U + BFBORF
!| BFBORL         |---| LOGARITHMIC LAW FOR COMPONENT ON THE
!|                |---| LATERAL BOUNDARIES:
!|                |---| NU*DF/DN = AFBORL*U + BFBORL
!| BFBORS         |---| LOGARITHMIC LAW FOR COMPONENT AT THE SURFACE:
!|                |---| NU*DF/DN = AFBORS*U + BFBORS
!| IELM2H         |-->| DISCRETISATION TYPE FOR 2D HORIZONTAL MESH
!| IELM2V         |-->| DISCRETISATION TYPE FOR 2D VERTICAL MESH
!| IPBOT          |-->| IPBOT(I)+1 IS THE FIRST FREE PLANE ABOVE
!|                |   | 2D POINT I.
!| MASKBR         |<->| 3D MASK ON LATERAL BOUNDARIES
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH2D         |-->| 2D MESH
!| MESH3D         |-->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH.
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| OP             |-->| 8 CHARACTERS STRING : 'X=X+Y   ' OR 'X=X-Y   '
!| OPTBAN         |-->| OPTION FOR THE TREATMENT OF TIDAL FLATS
!| SEM3D          |<->| RIGHT HAND SIDE OF EQUATION
!| SIGMAG         |-->| IF YES, GENERALISED SIGMA TRANSFORMATION
!|                |   | IT IS A RISK OF CRUSHED PLANES.
!| SV             |-->| VOID STRUCTURE
!| T2             |-->| 2D WORK BIEF_OBJ STRUCTURES
!| T3             |-->| 3D WORK BIEF_OBJ STRUCTURES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)      :: BFBORL,BFBORF,BFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKBR,MASKEL,SV
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3,T2
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D,MESH3D
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,OPTBAN
      INTEGER, INTENT(IN)             :: IELM2H,IELM2V,NPLAN
      INTEGER, INTENT(IN)             :: IPBOT(NPOIN2)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SEM3D
      LOGICAL, INTENT(IN)             :: MSK,SIGMAG
      CHARACTER(LEN=8), INTENT(IN)    :: OP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I3D
!
!-----------------------------------------------------------------------
!
!     BE CAREFUL! BFBOR* HAVE TO BE ADDED IN THE RHS AS THE
!     SAME WAY THAN AFBOR* IN THE MATRIX (E.G. IN  DIFF3D.F)
!     USE OF MASVEC INSTEAD OF MASBAS COULD BE TESTED
!
!     LATERAL BOUNDARIES
!
      IF(BFBORL%TYPR.NE.'0') THEN
        CALL VECTOR(T3,'=','MASBAS          ',IELM2V,1.D0,SV,
     &              SV,SV,SV,SV,SV,MESH3D,MSK,MASKBR)
        CALL OS('X=XY    ',X=T3,Y=BFBORL)
        CALL OSDB(OP,SEM3D,T3,T3,1.D0,MESH3D)
      ENDIF
!
!     BOTTOM
!
      IF(BFBORF%TYPR.NE.'0') THEN
        CALL VECTOR(T2,'=','MASBAS          ',IELM2H,1.D0,
     &              SV,SV,SV,SV,SV,SV,MESH2D,MSK,MASKEL)
        CALL OV('X=XY    ', X=T2%R, Y=BFBORF%R, DIM1=NPOIN2)
        IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
          IF(OP.EQ.'X=X+Y   ') THEN
            DO I=1,NPOIN2
!             THE TERM IS PUT ON FIRST FREE POINT, AND NOT PUT IN CASE
!             OF TIDAL FLAT
              IF(IPBOT(I).NE.NPLAN-1) THEN
                I3D=IPBOT(I)*NPOIN2+I
                SEM3D%R(I3D)=SEM3D%R(I3D)+T2%R(I)
              ENDIF
            ENDDO
          ELSEIF(OP.EQ.'X=X-Y   ') THEN
            DO I=1,NPOIN2
!             THE TERM IS PUT ON FIRST FREE POINT, AND NOT PUT IN CASE
!             OF TIDAL FLAT
              IF(IPBOT(I).NE.NPLAN-1) THEN
                I3D=IPBOT(I)*NPOIN2+I
                SEM3D%R(I3D)=SEM3D%R(I3D)-T2%R(I)
              ENDIF
            ENDDO
          ELSE
            WRITE(LU,*) 'UNEXPECTED OPERATION : ',OP
            WRITE(LU,*) 'IN T3D_STRESS'
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSE
          CALL OV(OP, X=SEM3D%R(1:NPOIN2) ,Y=T2%R, DIM1=NPOIN2)
        ENDIF
      ENDIF
!
!     FREE SURFACE
!
      IF(BFBORS%TYPR.NE.'0') THEN
        CALL VECTOR(T2,'=','MASBAS          ',IELM2H,1.D0,SV,
     &              SV,SV,SV,SV,SV,MESH2D,MSK,MASKEL)
        CALL OS('X=XY    ',X=T2,Y=BFBORS)
        CALL OV(OP, X=SEM3D%R(NPOIN3-NPOIN2+1:NPOIN3),
     &          Y=T2%R, Z=T2%R, DIM1=NPOIN2)
      ENDIF
!
!=======================================================================
!
      RETURN
      END

