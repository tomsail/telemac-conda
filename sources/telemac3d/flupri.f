!                   *****************
                    SUBROUTINE FLUPRI
!                   *****************
!
     &( VEC,XMUL,U,V,W,X,Y,Z,IKLE,
     &  NELMAX,NELEM2D,NPOIN2,NPOIN3,T1,T2,T3)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FLUX THROUGH THE BOTTOM AND THE FREE SURFACE IN 3D:
!code
!+                          /  ->  ->
!+      VEC = VEC  +  XMUL /   U . N  D(GAMMA)
!+                        /
!+                       /GAMMA
!+
!+      HERE GAMMA IS THE BOTTOM AND THE SURFACE
!+
!+      THE RESULT IS ADDED TO VECTOR VEC !!!!!!
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J.-M. HERVOUET (LNHE)
!+        14/03/06
!+        V5P7
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE           |-->| CONNECTIVITY TABLE (LOCAL TO GLOBAL)
!| NELEM2D        |-->| NUMBER OF ELEMENTS IN 2D
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!|                |   | (ADAPTATIVE MESH CASE)
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| T1             |<->| WORKING ARRAY (METRIC)
!| T2             |<->| WORKING ARRAY (METRIC)
!| T3             |<->| WORKING ARRAY (METRIC)
!| U              |-->| VECTOR COMPONENT PRESENT IN THE FORMULA
!| V              |-->| VECTOR COMPONENT PRESENT IN THE FORMULA
!| VEC            |<->| RESULT VECTOR
!| W              |-->| VECTOR COMPONENT PRESENT IN THE FORMULA
!| X              |-->| COORDINATE
!| XMUL           |-->| MULTIPLYING COEFFICIENT
!| Y              |-->| COORDINATE
!| Z              |-->| COORDINATE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELMAX,NELEM2D,NPOIN2,NPOIN3
      DOUBLE PRECISION, INTENT(INOUT) :: VEC(NPOIN3)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
!
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3),Y(NPOIN3),Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(INOUT) :: T1(NELEM2D)
      DOUBLE PRECISION, INTENT(INOUT) :: T2(NELEM2D)
      DOUBLE PRECISION, INTENT(INOUT) :: T3(NELEM2D)
!
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*),W(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION X2,X3,Y2,Y3,NX,NY,NZ,Z2,Z3,F1,F2,F3,XSUR24,F123
!
      INTEGER I1,I2,I3,IELEM
!
!**********************************************************************
!
      XSUR24 = XMUL/24.D0
!
!     LOOP ON THE 2D ELEMENTS
!
      DO IELEM = 1,NELEM2D
!
!       BOTTOM
!
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
!
        X2 = X(I2) - X(I1)
        X3 = X(I3) - X(I1)
        Y2 = Y(I2) - Y(I1)
        Y3 = Y(I3) - Y(I1)
        Z2 = Z(I2) - Z(I1)
        Z3 = Z(I3) - Z(I1)
!       OUTGOING NORMAL (NOT NORMALISED)
        NX =  (Z2*Y3-Z3*Y2)
        NY =  (X2*Z3-X3*Z2)
        NZ = -(X2*Y3-X3*Y2)
!
        F1 = U(I1)*NX+V(I1)*NY+W(I1)*NZ
        F2 = U(I2)*NX+V(I2)*NY+W(I2)*NZ
        F3 = U(I3)*NX+V(I3)*NY+W(I3)*NZ
!
        F123 = F1 + F2 + F3
!
        T1(IELEM) = XSUR24 * ( F123 + F1 )
        T2(IELEM) = XSUR24 * ( F123 + F2 )
        T3(IELEM) = XSUR24 * ( F123 + F3 )
!
!       ASSEMBLY
!
        VEC(I1) = VEC(I1) + T1(IELEM)
        VEC(I2) = VEC(I2) + T2(IELEM)
        VEC(I3) = VEC(I3) + T3(IELEM)
!
!       FREE SURFACE (IDEM EXCEPT FOR POINTS NUMBERS AND REVERSED NORMAL)
!
        I1 = I1 + NPOIN3-NPOIN2
        I2 = I2 + NPOIN3-NPOIN2
        I3 = I3 + NPOIN3-NPOIN2
!
!       X2 = X(I2) - X(I1)
!       X3 = X(I3) - X(I1)
!       Y2 = Y(I2) - Y(I1)
!       Y3 = Y(I3) - Y(I1)
        Z2 = Z(I2) - Z(I1)
        Z3 = Z(I3) - Z(I1)
!       OUTGOING NORMAL (NOT NORMALISED)
        NX = -(Z2*Y3-Z3*Y2)
        NY = -(X2*Z3-X3*Z2)
        NZ = +(X2*Y3-X3*Y2)
!
        F1 = U(I1)*NX+V(I1)*NY+W(I1)*NZ
        F2 = U(I2)*NX+V(I2)*NY+W(I2)*NZ
        F3 = U(I3)*NX+V(I3)*NY+W(I3)*NZ
!
        F123 = F1 + F2 + F3
!
        T1(IELEM) = XSUR24 * ( F123 + F1 )
        T2(IELEM) = XSUR24 * ( F123 + F2 )
        T3(IELEM) = XSUR24 * ( F123 + F3 )
!
!       ASSEMBLY
!
        VEC(I1) = VEC(I1) + T1(IELEM)
        VEC(I2) = VEC(I2) + T2(IELEM)
        VEC(I3) = VEC(I3) + T3(IELEM)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
