!                   **************************
                    SUBROUTINE USER_T3D_CORFON
!                   **************************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER MODIFIES THE BOTTOM TOPOGRAPHY.
!+
!
!history  J.M. JANIN  (LNH)
!+        25/11/97
!+        V5P1
!+
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Creation from T3D_CORFON
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LISFON         |-->| NUMBER OF SMOOTHINGS REQUIRED
!| MASKEL         |-->| MASK OF ELEMENTS
!| MATR2D         |<->| WORK MATRIX IN 2DH
!| MESH2D         |<->| 2D MESH
!| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| PRIVE          |<->| BLOCK OF PRIVATE ARRAYS FOR USER
!| S              |-->| VOID STRUCTURE
!| ST1            |<->| STRUCTURE OF T1
!| ST2            |<->| STRUCTURE OF T2
!| SZF            |<->| STRUCTURE OF ZF
!| T1             |<->| WORK ARRAY
!| T2             |<->| WORK ARRAY
!| X              |-->| MESH COORDINATE
!| Y              |-->| MESH COORDINATE
!| ZF             |<->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2, LISFON
      LOGICAL, INTENT(IN) :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SZF, ST1, ST2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, T1, T2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X,Y
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: PRIVE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MATR2D
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: S
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION A0,A1,A2,A3,A4,A5,A6,A7
!
!-----------------------------------------------------------------------
!
      A1=3.69D0
      A2=5.985D0
      A3=12.D0
      A4=14.D0
      A5=17.015D0
      A6=20.785D0
!
      A0=A1-1.D0
      A7=A6+1.D0
!
! BAR DEFINITION + SMALL TRANSITIONS
!
      DO  I=1,NPOIN2
!
        ZF(I)=-0.43D0
        IF (X(I).GE.A0.AND.X(I).LE.A1) THEN
          ZF(I)= -0.43D0+0.03D0*(X(I)-A0)/(A1-A0)
        ENDIF
!
        IF (X(I).GE.A1.AND.X(I).LE.A2) THEN
          ZF(I)=-0.4D0
        ENDIF
        IF (X(I).GE.A2.AND.X(I).LE.A3) THEN
          ZF(I)=-0.4D0+0.3D0*(X(I)-A2)/(A3-A2)
        ENDIF
        IF (X(I).GE.A3.AND.X(I).LE.A4) THEN
          ZF(I)=-0.1D0
        ENDIF
        IF (X(I).GE.A4.AND.X(I).LE.A5) THEN
          ZF(I)=-0.1D0-0.3D0*(X(I)-A4)/(A5-A4)
        ENDIF
        IF (X(I).GE.A5.AND.X(I).LE.A6) THEN
          ZF(I)=-0.4D0
        ENDIF
        IF(X(I).GE.A6.AND.X(I).LE.A7) THEN
          ZF(I)=-0.4D0-0.03D0*(X(I)-A6)/(A7-A6)
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
