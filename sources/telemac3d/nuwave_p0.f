!                   ********************
                    SUBROUTINE NUWAVE_P0
!                   ********************
!
     &(NUWAVE,DM1,Z,DZ,IKLE,NPOIN2,NPLAN,NELEM,NELMAX,NELEM2,XMUL,IELM3,
     & X,Y,SURFAC)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FOR WAVE EQUATION, COMPUTES THE PSEUDO-VISCOSITY
!+                AS A PIECE-WISE CONSTANT VARIABLE.
!+
!+            THE COMPUTATION IS DONE SO THAT THE SUM ON THE
!+                VERTICAL OF FLUXES DUE TO GRADIENT OF FREE SURFACE
!+                COMPUTED IN VC04PP GIVES THE SAME RESULT
!+               (SEE VCGRADP WITH SPECAD=.TRUE.).
!
!history  J-M HERVOUET (LNHE)
!+        22/06/06
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DM1            |-->| D**-1 (SEE BOOK)
!| DZ             |<->| DELTA(Z) : HEIGHT OF PRISMS
!| IELM3          |-->| TYPE OF ELEMENT
!| IKLE           |-->| CONNECTIVITY TABLE IN 2D MESH
!| NELEM2         |-->| NUMBER OF 2D ELEMENTS
!| NELEM          |-->| NUMBER OF 3D ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF 3D ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NUWAVE         |<->| PSEUDO-VISCOSITY IN WAVE EQUATION
!| SURFAC         |-->| AREA OF TRIANGLES IN UNDERLYING 2D MESH
!| XMUL           |-->| MULTIPLICATIVE CONSTANT
!| X              |-->| ABSCISSAE OF 3D MESH POINTS
!| Y              |-->| ORDINATES OF 3D MESH POINTS
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN2,NPLAN,NELEM,NELMAX
      INTEGER,          INTENT(IN)    :: NELEM2,IELM3
      INTEGER,          INTENT(IN)    :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: NUWAVE(NELEM2),DZ(NPOIN2*NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: DM1(NPOIN2*NPLAN),XMUL
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2*NPLAN),X(*),Y(*)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IETAGE,I1,I2,I3,I4,IELEM,IELEM2D
      DOUBLE PRECISION F1,F2,F3,G1,G2,G3,G4,XSUR12,XSUR24
      DOUBLE PRECISION X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4,VOL
!
!***********************************************************************
!
      XSUR12=XMUL/12.D0
      XSUR24=XMUL/24.D0
!
!     COMPUTES DELTA(Z) FOR ALL THE LEVELS : IN DZ
!
      CALL OV('X=Y-Z   ', X=DZ, Y=Z(1+NPOIN2), Z=Z(1),
     &        DIM1=NPOIN2*(NPLAN-1))
!
!     SUMS AVERAGE DZ*DM1 FOR EACH LEVEL
!     AVERAGE DZ*DM1 = INTEGRAL OF DZ*DM1 ON THE TRIANGLE
!     DIVIDED BY THE TRIANGLE AREA
!
      CALL OV('X=C     ', X=NUWAVE, C=0.D0, DIM1=NELEM2)
!
!-----------------------------------------------------------------------
!
!     PRISMS
!
      IF(IELM3.EQ.41) THEN
!
      DO IETAGE=1,NPLAN-1
        DO IELEM=1,NELEM2
!
        I1=IKLE(IELEM,1)+(IETAGE-1)*NPOIN2
        I2=IKLE(IELEM,2)+(IETAGE-1)*NPOIN2
        I3=IKLE(IELEM,3)+(IETAGE-1)*NPOIN2
!       FUNCTION F : DZ
!       FUNCTION G : AVERAGE DM1 FOR EACH VERTICAL
        F1=DZ(I1)
        F2=DZ(I2)
        F3=DZ(I3)
        G1=0.5D0*(DM1(I1)+DM1(I1+NPOIN2))
        G2=0.5D0*(DM1(I2)+DM1(I2+NPOIN2))
        G3=0.5D0*(DM1(I3)+DM1(I3+NPOIN2))
        NUWAVE(IELEM)=NUWAVE(IELEM)+
     &   (F3*(G1+G2)+2*(F1*G1+F2*G2+F3*G3)+F2*(G1+G3)+F1*(G2+G3))*XSUR12
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     PRISMS CUT INTO TETRAHEDRONS
!
      ELSEIF(IELM3.EQ.51) THEN
!
      DO IELEM=1,NELEM
!
!       RETRIEVING THE CORRESPONDING TRIANGLE
!
        IELEM2D=MOD(IELEM-1,NELEM2)+1
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
!
        G1=DM1(I1)
        G2=DM1(I2)
        G3=DM1(I3)
        G4=DM1(I4)
!
        X2 = X(I2)-X(I1)
        X3 = X(I3)-X(I1)
        X4 = X(I4)-X(I1)
!
        Y2 = Y(I2)-Y(I1)
        Y3 = Y(I3)-Y(I1)
        Y4 = Y(I4)-Y(I1)
!
        Z2 = Z(I2)-Z(I1)
        Z3 = Z(I3)-Z(I1)
        Z4 = Z(I4)-Z(I1)
!       IN FACT VOL=VOLUME/4
        VOL=(X2*(Y3*Z4-Y4*Z3)+Y2*(X4*Z3-X3*Z4)+Z2*(X3*Y4-X4*Y3))*XSUR24
        NUWAVE(IELEM2D)=NUWAVE(IELEM2D)+VOL*(G1+G2+G3+G4)
!
      ENDDO
!
      CALL OV('X=Y/Z   ', X=NUWAVE, Y=NUWAVE, Z=SURFAC, DIM1=NELEM2)
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) ' '
        WRITE(LU,*) 'NUWAVE_P0: ELEMENT NOT IMPLEMENTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
