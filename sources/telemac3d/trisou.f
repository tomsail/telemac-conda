!                   *****************
                    SUBROUTINE TRISOU
!                   *****************
!
     & (CV1, CV2, SCV1, SCV2, UN3, VN3, X, Y, Z, ZS,
     &  DELTAR,MESH3,FCOR,CORIOL,NTRAC,AT,SURFAC,
     &  T1,ST1, W1, W2, W3, GRAV, NPOIN3, NELEM3, NPOIN2,
     &  NELEM2, NPLAN, NETAGE, IKLE3, LV, MSK, MASKEL, INCHYD,
     &  SVOLU,SVIDE,IELM3,SMASKEL,NREJEU,ISCE,KSCE,QSCE,USCE,VSCE,
     &  GRADZSX,GRADZSY,MESH2D, ST2,T2,ST3,T3,
     &  LONGIT, YASEM3D,SCHCVI,DENLAW,FXH,FYH,
     &  COUROU,NPTH,T3D_FILES,T3DBI1)
!
!***********************************************************************
! TELEMAC3D   V6P3                                 21/08/2010
!***********************************************************************
!
!brief    SOURCE TERMS FOR U & V MOMENTUM EQUATIONS.
!
!history  CGD/SOGREAH
!+
!+
!+   CORIOLIS FORCE ADDED
!
!history  AG (LNHE)
!+
!+
!+   BUOYANCY TERMS COMPUTED IN PHYSICAL SPACE
!
!history  JMH
!+        19/12/2008
!+
!+   WAVE DRIVEN CURRENTS ADDED. SEE IF(COUROU)
!
!history  J-M HERVOUET (LNHE)
!+        29/06/2009
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
!history  R. KOPMANN (EDF R&D, LNHE)
!+        16/04/2013
!+        V6P3
!+   Adding the file format in calls to FIND_IN_SEL.
!
!history  J-M HERVOUET (LNHE)
!+        14/06/2013
!+        V6P3
!+   In last loop on sources a test IF(ISCE(I).GT.0) THEN has been added
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/07/2015
!+        V7P1
!+   A division by SVOLU was done in case of sources, it had to be the
!+   assembled volumes in parallel.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME OF TIME STEP
!| CORIOL         |-->| LOGICAL IF CORIOLIS FORCE OR NOT
!| COUROU         |-->| LOGICAL FOR WAVE DRIVEN CURRENTS
!| CV1            |<->| SOURCE TERMS ON U
!| CV2            |<->| SOURCE TERMS ON V
!| DELTAR         |<->| (RHO-RHO0)/RHO0
!| DENLAW         |-->| CHOICE OF DENSITY LAW (SEE ABOVE)
!| FCOR           |-->| CORIOLIS COEFFICIENT
!| FXH            |<->| WAVE STRESSES FROM ARTEMIS OR TOMAWAC
!| FYH            |<->| WAVE STRESSES FROM ARTEMIS OR TOMAWAC
!| GRADZSX        |<->| FREE SURFACE GRADIENT
!| GRADZSY        |<->| FREE SURFACE GRADIENT
!| GRAV           |-->| GRAVITY ACCELERATION
!| IELM3          |-->| CORRESPONDENCE BETWEEN LOCAL AND 3D GLOBAL
!|                |---| NUMBER
!| INCHYD         |-->| IF YES, HYDROSTATIC INCONSISTENCY FILTER
!| ISCE           |-->| NODE ADRESSES IN 2D MESH FOR SOURCES
!| KSCE           |-->| NUMBER OF PLANE FOR SOURCES
!| LONGIT         |-->| LONGITUDE OF THE ORIGIN POINT: NOT USED
!| LV             |-->| VECTOR LENGTH FOR VECTORISATION
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH2D         |<->| 2D MESH
!| MESH3          |---| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| NPTH           |-->| RECORD NUMBER IN THE WAVE DRIVEN CURRENTS FILE
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| NREJEU         |---| NUMBER OF VELOCITY INFORMATION FOR SOURCE
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| QSCE           |---| WATER DISCHARGE OF SOURCES
!| SCHCVI         |-->| ADVECTION SCHEME ON VELOCITY
!| SCV1           |<->| ASSOCIATED STRUCTURES
!| SCV2           |<->| ASSOCIATED STRUCTURES
!| SMASKEL        |-->| ASSOCIATED STRUCTURES
!| ST1            |<->| ASSOCIATED STRUCTURE
!| ST2            |<->| ASSOCIATED STRUCTURE
!| ST3            |<->| ASSOCIATED STRUCTURE
!| SURFAC         |-->| AREA OF TRIANGLES
!| SVIDE          |<->| VOID VECTOR STRUCTURE
!| SVOLU          |<->| ASSOCIATED STRUCTURE: NOT USED
!| T1             |<->| WORK ARRAY BY POINTS
!| T2             |<->| WORK ARRAY
!| T3             |<->| WORK ARRAY
!| T3D_FILES      |-->| DATA STRUCTURE WITH DATA ON FILES
!| T3DBI1         |-->| BINARY DATA FILE 1
!| UN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP N
!| USCE           |-->| VELOCITIES OF THE SOURCES ALONG X
!| VN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP N
!| VSCE           |-->|VELOCITIES OF THE SOURCES ALONG Y
!| W1             |<->| WORK ARRY BY 3D ELEMENTS
!| W2             |<->| WORK ARRY BY 3D ELEMENTS
!| W3             |<->| WORK ARRY BY 3D ELEMENTS
!| X              |-->| 3D MESH COORDINATES
!| Y              |-->| 3D MESH COORDINATES
!| YASEM3D        |<->| IF TRUE, RIGHT HAND SIDE HAS BEEN PARTLY
!|                |   | COMPUTED BEFORE CALLING DIFF3D
!| Z              |-->| 3D MESH COORDINATES
!| ZS             |<->| COTE PAR RAPPORT A LA SURFACE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_TELEMAC3D, EX_TRISOU => TRISOU
      USE DECLARATIONS_TELEMAC3D, ONLY : SPHERI, MAREE, MARDAT, MARTIM,
     &     T2_01, T2_02, T3_05, T3_06, T3_07, T3_08,
     &     DEJALU_TRISOU, WIPDX, WIPDY, USTOKES, VSTOKES, WSTOKES,
     &     USTX, VSTY, WST1, WST2, UN, VN, S3D_SEDI
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3,NELEM3,NPOIN2,NELEM2
      INTEGER, INTENT(IN) :: NPLAN,NETAGE,NTRAC,NPTH,T3DBI1
      INTEGER, INTENT(IN) :: LV,NREJEU,IELM3,SCHCVI,DENLAW
!
      INTEGER, INTENT(IN) :: IKLE3(NELEM3,*)
!
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(*)
      DOUBLE PRECISION, INTENT(IN)    :: LONGIT
!
      DOUBLE PRECISION, INTENT(INOUT) :: CV1(NPOIN3),CV2(NPOIN3)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: SCV1,SCV2,FXH,FYH
!
      DOUBLE PRECISION, INTENT(IN)    :: UN3(NPOIN3), VN3(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: ZS(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELEM3,*), W2(NELEM3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELEM3,*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T1(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: T2(NPOIN3), T3(NPOIN3)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ST1, ST2, ST3
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DELTAR
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH3
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: GRADZSX
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: GRADZSY
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: SMASKEL
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: SVIDE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: SVOLU
!
!                                 * = NSCE
      INTEGER, INTENT(IN) :: ISCE(*),KSCE(*)
      DOUBLE PRECISION, INTENT(IN) :: QSCE(*),USCE(*),VSCE(*)
!
      DOUBLE PRECISION, INTENT(IN) :: GRAV, AT, FCOR
      LOGICAL, INTENT(IN) :: CORIOL, MSK, INCHYD,COUROU
      LOGICAL, INTENT(INOUT) :: YASEM3D
      TYPE(BIEF_FILE), INTENT(IN) :: T3D_FILES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM3,IPLAN,IETAGE,IZ,IZM,IZS,I3D
      DOUBLE PRECISION A,OMEGA,PI,CORI
!
      INTEGER I,OPTFLO
      CHARACTER(LEN=15) FORMUL
!
      CHARACTER(LEN=8) :: FFORMAT
      INTEGER :: FILE_ID, IERR
!
      INTEGER I1,I2,I3,I4,I5,I6
      DOUBLE PRECISION DX1,DX2,DX3,DY1,DY2,DY3
      DOUBLE PRECISION DZ1,DZ2,DZ3,DR1,DR2,DR3
      DOUBLE PRECISION SZ1,SZ2,SZ3,SR1,SR2,SR3
      DOUBLE PRECISION DZSUDX,DZSUDY,DRSUDX,DRSUDY
      DOUBLE PRECISION DZ123,ATH
!
!     FOR WAVE DRIVEN CURRENTS
!
      CHARACTER(LEN=16) NOMX,NOMY
      LOGICAL OKX,OKY
      LOGICAL ACTIVE_TEMP_SAL, ACTIVE_SED
!
!***********************************************************************
!
! INITIALISES
!
      SCV1%TYPR='0'
      SCV2%TYPR='0'
!
!-----------------------------------------------------------------------
!  BUOYANCY SOURCE TERMS (ONLY WITH DENLAW NOT 0, EVEN FOR SEDIMENT !!)
!-----------------------------------------------------------------------
!
      YASEM3D = .FALSE.
      ACTIVE_TEMP_SAL = DENLAW.NE.0.AND.DENLAW.NE.5.AND.NTRAC.GT.0
      ACTIVE_SED = (INCLUS(COUPLING,'GAIA').OR.S3D_SEDI)
     &               .AND.NTRAC.GT.0.AND.DENLAW.NE.5

!
      IF(ACTIVE_TEMP_SAL.OR.ACTIVE_SED) THEN
!
      SCV1%TYPR = 'Q'
      SCV2%TYPR = 'Q'
      CALL OS( 'X=0     ' , X=SCV1 )
      CALL OS( 'X=0     ' , X=SCV2 )
!
!     VOLUME OF TEST FUNCTIONS
!
      CALL VECTOR(ST1, '=', 'MASBAS          ',IELM3, 1.D0,
     &            SVIDE, SVIDE,
     &            SVIDE, SVIDE, SVIDE, SVIDE, MESH3,.FALSE.,SMASKEL)
      IF(NCSIZE.GT.1) CALL PARCOM(ST1,2,MESH3)
!
!     1 : BUOYANCY IN REAL MESH
!     2 : BUOYANCY IN TRANSFORMED MESH
!
!     ENABLES TREATMENT WITH TETRAHEDRONS
!
      OPTFLO = 1
!
      IF(OPTFLO.EQ.1) THEN
!
      YASEM3D = .FALSE.
!
! - G DENSITY GRADIENTS
!     WITH TREATMENT OF HYDROSTATIC INCONSISTENCIES IF NEEDED
!     THIS IS AVAILABLE ONLY FOR PRISMS !!!!!!!!!!!!
!
      FORMUL = 'GRADF          '
      IF(IELM3.EQ.41.AND.INCHYD) FORMUL(6:6) = '2'
!
!     ===============================================
!     BETTER FILTERING OF HYDROSTATIC INCONSISTENCIES
!     ===============================================
!
!     3 OR 4 IMPLIES THAT 2 IS ALSO APPLIED
!
!     RECOMMENDED : FILTER 4
!
!     FILTER 3
!     IF(INCHYD) FORMUL(6:6)='3'
!     FILTER 4
!     IF(INCHYD) FORMUL(6:6)='4'
!
!
      CALL VECTOR(ST2, '=',FORMUL//'X',IELM3,-GRAV,DELTAR,SVIDE,
     &            SVIDE,SVIDE,SVIDE,SVIDE,MESH3,MSK,SMASKEL)
!
      CALL VECTOR(ST3, '=',FORMUL//'Y',IELM3,-GRAV,DELTAR,SVIDE,
     &            SVIDE,SVIDE,SVIDE,SVIDE,MESH3,MSK,SMASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(ST2,2,MESH3)
        CALL PARCOM(ST3,2,MESH3)
      ENDIF
!
! NODAL VALUE
!
      CALL OVD('X=Y/Z   ',T2,T2,T1,0.D0,NPOIN3,2,0.D0,1.D-9)
      CALL OVD('X=Y/Z   ',T3,T3,T1,0.D0,NPOIN3,2,0.D0,1.D-9)
!
! SIMPSON INTEGRATION
!
      DO IPLAN = NPLAN, 2, -1
        DO I = 1, NPOIN2
          IZ  = (IPLAN-1)*NPOIN2+I
          IZM = (IPLAN-2)*NPOIN2+I
          A = 0.5D0 * (Z(IZ)-Z(IZM))
          CV1(IZM) = CV1(IZ) + (T2(IZ)+T2(IZM)) * A
          CV2(IZM) = CV2(IZ) + (T3(IZ)+T3(IZM)) * A
        ENDDO
      ENDDO
!
! TERMS WITH FREE SURFACE GRADIENT (NODAL VALUES)
!
      DO IPLAN = 1, NPLAN-1
!
        DO I = 1, NPOIN2
          IZ  = (IPLAN-1)*NPOIN2+I
          IZS = (NPLAN-1)*NPOIN2+I
          A = GRAV*(DELTAR%R(IZ)-DELTAR%R(IZS))
!
          CV1(IZ) = CV1(IZ) + A * GRADZSX%R(I)
          CV2(IZ) = CV2(IZ) + A * GRADZSY%R(I)
!
        ENDDO
!
      ENDDO
!
      ELSEIF(OPTFLO.EQ.2) THEN
!
      YASEM3D = .FALSE.
!
! TRANSFORMED MESH
!
! COMPUTES ZS: OPPOSITE OF WATER DEPTH AT CONSIDERED POINT
!
        I2 = NPOIN3 - NPOIN2 + 1
        I4 = NPOIN3
        DO IPLAN = 1,NPLAN
          I1 = NPOIN2*(IPLAN-1) + 1
          I3 = NPOIN2* IPLAN
          CALL OV('X=Y-Z   ', X=ZS(I1:I3), Y=Z(I1:I3), Z=Z(I2:I4),
     &            DIM1=NPOIN2)
        END DO
!
        BYELEMENT: DO IELEM3 = 1 , NELEM3
!
          I1 = IKLE3(IELEM3,1)
          I2 = IKLE3(IELEM3,2)
          I3 = IKLE3(IELEM3,3)
          I4 = IKLE3(IELEM3,4)
          I5 = IKLE3(IELEM3,5)
          I6 = IKLE3(IELEM3,6)
!
          DX1 = X(I3) - X(I2)
          DX2 = X(I1) - X(I3)
          DX3 = X(I2) - X(I1)
          DY1 = Y(I2) - Y(I3)
          DY2 = Y(I3) - Y(I1)
          DY3 = Y(I1) - Y(I2)
!
          DR1 = DELTAR%R(I4) - DELTAR%R(I1)
          DR2 = DELTAR%R(I5) - DELTAR%R(I2)
          DR3 = DELTAR%R(I6) - DELTAR%R(I3)
          DZ1 = ZS(I4) - ZS(I1)
          DZ2 = ZS(I5) - ZS(I2)
          DZ3 = ZS(I6) - ZS(I3)
          DZ123 = DZ1 + DZ2 + DZ3
!
          SZ1 = ZS(I4) + ZS(I1)
          SZ2 = ZS(I5) + ZS(I2)
          SZ3 = ZS(I6) + ZS(I3)
          SR1 = DELTAR%R(I4) + DELTAR%R(I1)
          SR2 = DELTAR%R(I5) + DELTAR%R(I2)
          SR3 = DELTAR%R(I6) + DELTAR%R(I3)
!
          IF(MAX(ZS(I1),ZS(I2),ZS(I3)).GT.
     &       MIN(ZS(I4),ZS(I5),ZS(I6)).AND.INCHYD) THEN
            DR1 = 0.D0
            DR2 = 0.D0
            DR3 = 0.D0
            SR1 = 0.D0
            SR2 = 0.D0
            SR3 = 0.D0
          ENDIF
!
          DZSUDX = SZ1 * DY1 + SZ2 * DY2 + SZ3 * DY3
          DZSUDY = SZ1 * DX1 + SZ2 * DX2 + SZ3 * DX3
          DRSUDX = SR1 * DY1 + SR2 * DY2 + SR3 * DY3
          DRSUDY = SR1 * DX1 + SR2 * DX2 + SR3 * DX3
!
          W1(IELEM3,1) = DR1 * DZSUDX - DZ1 * DRSUDX
          W1(IELEM3,2) = DR2 * DZSUDX - DZ2 * DRSUDX
          W1(IELEM3,3) = DR3 * DZSUDX - DZ3 * DRSUDX
          W2(IELEM3,1) = DR1 * DZSUDY - DZ1 * DRSUDY
          W2(IELEM3,2) = DR2 * DZSUDY - DZ2 * DRSUDY
          W2(IELEM3,3) = DR3 * DZSUDY - DZ3 * DRSUDY
          W3(IELEM3,1) = DZ1 + DZ123
          W3(IELEM3,2) = DZ2 + DZ123
          W3(IELEM3,3) = DZ3 + DZ123
!
        END DO BYELEMENT
!
        IF (NETAGE.NE.1) THEN
!
          BYETAGE: DO IETAGE = NETAGE-1 , 1 , -1
            I2 = NELEM2*IETAGE + 1
            I1 = I2 - NELEM2
            CALL OV('X=X+Y   ', X=W1(I1,1), Y=W1(I2,1), DIM1=NELEM2)
            CALL OV('X=X+Y   ', X=W1(I1,2), Y=W1(I2,2), DIM1=NELEM2)
            CALL OV('X=X+Y   ', X=W1(I1,3), Y=W1(I2,3), DIM1=NELEM2)
            CALL OV('X=X+Y   ', X=W2(I1,1), Y=W2(I2,1), DIM1=NELEM2)
            CALL OV('X=X+Y   ', X=W2(I1,2), Y=W2(I2,2), DIM1=NELEM2)
            CALL OV('X=X+Y   ', X=W2(I1,3), Y=W2(I2,3), DIM1=NELEM2)
          END DO BYETAGE
!
          I2 = NELEM2 + 1
          I1 = NELEM3 - NELEM2
          CALL OV('X=Y     ', X=W1(1,4), Y=W1(I2,1), DIM1=I1)
          CALL OV('X=Y     ', X=W1(1,5), Y=W1(I2,2), DIM1=I1)
          CALL OV('X=Y     ', X=W1(1,6), Y=W1(I2,3), DIM1=I1)
          CALL OV('X=Y     ', X=W2(1,4), Y=W2(I2,1), DIM1=I1)
          CALL OV('X=Y     ', X=W2(1,5), Y=W2(I2,2), DIM1=I1)
          CALL OV('X=Y     ', X=W2(1,6), Y=W2(I2,3), DIM1=I1)
!
        ENDIF
!
        I1 = NELEM3 - NELEM2 + 1
        CALL OV('X=C     ', X=W1(I1,4), C=0.D0, DIM1=NELEM2)
        CALL OV('X=C     ', X=W1(I1,5), C=0.D0, DIM1=NELEM2)
        CALL OV('X=C     ', X=W1(I1,6), C=0.D0, DIM1=NELEM2)
        CALL OV('X=C     ', X=W2(I1,4), C=0.D0, DIM1=NELEM2)
        CALL OV('X=C     ', X=W2(I1,5), C=0.D0, DIM1=NELEM2)
        CALL OV('X=C     ', X=W2(I1,6), C=0.D0, DIM1=NELEM2)
!
        CALL OV('X=XY    ', X=W1(1,1), Y=W3, DIM1=3*NELEM3)
        CALL OV('X=XY    ', X=W1(1,4), Y=W3, DIM1=3*NELEM3)
        CALL OV('X=XY    ', X=W2(1,1), Y=W3, DIM1=3*NELEM3)
        CALL OV('X=XY    ', X=W2(1,4), Y=W3, DIM1=3*NELEM3)
!
        DO IETAGE = 1 , NETAGE
          I1 = NELEM2*(IETAGE-1) + 1
          CALL OV('X=XY    ', X=W3(I1,1), Y=SURFAC, DIM1=NELEM2)
          CALL OV('X=XY    ', X=W3(I1,2), Y=SURFAC, DIM1=NELEM2)
          CALL OV('X=XY    ', X=W3(I1,3), Y=SURFAC, DIM1=NELEM2)
        ENDDO
        CALL OV('X=Y     ', X=W3(1,4), Y=W3, DIM1=3*NELEM3)
!
        CALL ASSVEC(CV1,IKLE3,NPOIN3,NELEM3,NELEM3,W1,.FALSE.,
     &              LV,MSK,MASKEL,6)
        CALL ASSVEC(CV2,IKLE3,NPOIN3,NELEM3,NELEM3,W2,.FALSE.,
     &              LV,MSK,MASKEL,6)
        CALL ASSVEC(T1,IKLE3,NPOIN3,NELEM3,NELEM3,W3,.TRUE. ,
     &              LV,MSK,MASKEL,6)
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(SCV1,2,MESH3)
          CALL PARCOM(SCV2,2,MESH3)
          CALL PARCOM(ST1,2,MESH3)
        ENDIF
!
        CALL OVD('X=CY/Z  ',CV1,CV1,T1,0.25D0*GRAV,NPOIN3,2,0.D0,1D-9)
        CALL OVD('X=CY/Z  ',CV2,CV2,T1,0.25D0*GRAV,NPOIN3,2,0.D0,1D-9)
!
      ENDIF
!
!     IF(NTRAC.GT.0)
      ENDIF
!
!-----------------------------------------------------------------------
!  CORIOLIS FORCE
!-----------------------------------------------------------------------
!
      IF(CORIOL) THEN
!
        IF(SCV1%TYPR.EQ.'0') THEN
          CALL OS( 'X=0     ' , X=SCV1 )
          CALL OS( 'X=0     ' , X=SCV2 )
          SCV1%TYPR='Q'
          SCV2%TYPR='Q'
        ENDIF
!
!       HERE THE VERTICAL VELOCITY IS NEGLECTED
!
        IF(SPHERI) THEN
!
          PI = 4.D0 * ATAN( 1.D0 )
          OMEGA=2.D0*PI/86164.D0
!
          DO IPLAN=1,NPLAN
            DO I=1,NPOIN2
              CORI=2.D0*OMEGA*MESH2D%SINLAT%R(I)
              I3=(IPLAN-1)*NPOIN2+I
              CV1(I3)=CV1(I3)+VN3(I3)*CORI
              CV2(I3)=CV2(I3)-UN3(I3)*CORI
            ENDDO
          ENDDO
!
!         TAKES THE TIDE GENERATING FORCE INTO ACCOUNT
!
          IF(MAREE) THEN
            CALL CPSTVC(MESH2D%X,T2_01)
            CALL CPSTVC(MESH2D%Y,T2_02)
            CALL OS('X=0     ',X=T2_01)
            CALL OS('X=0     ',X=T2_02)
            CALL MARAST(MARDAT,MARTIM,LONGIT,NPOIN2,AT,
     &                  T2_01%R,T2_02%R,MESH2D%X%R,
     &                  MESH2D%SINLAT%R,MESH2D%COSLAT%R,GRAV)
            DO IPLAN=1,NPLAN
              DO I=1,NPOIN2
                I3=(IPLAN-1)*NPOIN2+I
                CV1(I3)=CV1(I3)+T2_01%R(I)
                CV2(I3)=CV2(I3)+T2_02%R(I)
              ENDDO
            ENDDO
          ENDIF
!
        ELSE
!
          CALL OV('X=X+CY   ', X=CV1, Y=VN3, C=FCOR, DIM1=NPOIN3)
          CALL OV('X=X+CY   ', X=CV2, Y=UN3, C=-FCOR, DIM1=NPOIN3)
!
        ENDIF
!
      ENDIF
!
!***********************************************************************
!
!     * WITH WAVE DRIVEN CURRENTS
!       -------------------------
!
!       FORCING TERMS FROM A TOMAWAC RESULTS FILE
!
!       BEWARE :    1. MESHES MUST BE THE SAME
!       ---------
!                   2. TAKES THE LAST TIMESTEP FROM TOMAWAC FILE
!
      IF(COUROU) THEN
!
        IF(.NOT.DEJALU_TRISOU.AND..NOT.INCLUS(COUPLING,'TOMAWAC')) THEN
!
!
!         T3DBI1 : BINARY DATA FILE 1
          NOMX='FORCE FX        '
          NOMY='FORCE FY        '
          FFORMAT = T3D_FILES(T3DBI1)%FMT
          FILE_ID = T3D_FILES(T3DBI1)%LU
          CALL FIND_VARIABLE(FFORMAT, FILE_ID, NOMX, FXH%R, NPOIN2,
     &                   IERR,RECORD=NPTH,TIME_RECORD=ATH)
          OKX = IERR.EQ.0
          CALL FIND_VARIABLE(FFORMAT, FILE_ID, NOMY, FYH%R, NPOIN2,
     &                   IERR,RECORD=NPTH,TIME_RECORD=ATH)
          OKY = IERR.EQ.0
!
          IF(.NOT.OKX.OR..NOT.OKY) THEN
            WRITE(LU,6)
 6          FORMAT(1X,'TRISOU: FORCE FX OR FY NOT FOUND',/,1X,
     &                '        IN THE WAVE RESULTS FILE')
            CALL PLANTE(1)
            STOP
          ENDIF
!         WRITES OUT TO LISTING
          WRITE(LU,116) ATH
116       FORMAT(1X,/,1X,'TRISOU: WAVE DRIVEN CURRENTS MODELLING',/,
     &                1X,'         READING FILE AT TIME ',F10.3,/)
          DEJALU_TRISOU = .TRUE.
!
        ENDIF
!
        IF(.NOT.INCLUS(COUPLING,'TOMAWACT3D')) THEN
!
!         ADDS TO SOURCE TERMS
!
          IF(SCV1%TYPR.EQ.'0') THEN
            DO I = 1,NPOIN2
              DO IPLAN = 1,NPLAN
                I3D = (IPLAN-1)*NPOIN2+I
!               CV1(I3D) = 1.5D0*FXH%R(I)  (SOGREAH-PECHON-TEISSON VERSION)
                CV1(I3D) = FXH%R(I)
                CV2(I3D) = FYH%R(I)
              ENDDO
            ENDDO
            SCV1%TYPR = 'Q'
            SCV2%TYPR = 'Q'
          ELSE
            DO I = 1,NPOIN2
              DO IPLAN = 1,NPLAN
                I3D = (IPLAN-1)*NPOIN2+I
!               CV1(I3D) = CV1(I3D)+1.5D0*FXH%R(I)  (SOGREAH-PECHON-TEISSON VERSION)
                CV1(I3D) = CV1(I3D)+FXH%R(I)
                CV2(I3D) = CV2(I3D)+FYH%R(I)
              ENDDO
            ENDDO
          ENDIF
        ELSE
!       VORTEX FORCE FORMALISM FOR WAVE AND CURRENTS INTERACTIONS START HERE
!         CORIOLIS FORCE COMBINED WITH STOKES DRIFT
          IF(CORIOL) THEN
            CALL OV('X=X+CY   ', X=CV1, Y=VSTOKES%R,C=FCOR,DIM1=NPOIN3)
            CALL OV('X=X+CY   ', X=CV2, Y=USTOKES%R,C=-FCOR,DIM1=NPOIN3)
          ENDIF

!       DERIVATIVE CALCULATION OF US AND VS TO GET WS

!         DERIVATIVE IN X OF USTOKES
          CALL VECTOR(USTX,'=','GRADF          X',IELM3,1.D0,USTOKES,
     &              SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3,.FALSE.,SMASKEL)

!         DERIVATIVE IN Y OF VSTOKES
          CALL VECTOR(VSTY,'=','GRADF          Y',IELM3,1.D0,VSTOKES,
     &              SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3,.FALSE.,SMASKEL)

!         HORIZONTAL AND VERTICAL GRADIENT CALCULATION OF U AND V
          CALL VECTOR(T3_05,'=','GRADF          Y',IELM3,1.D0,UN,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3,MSK,SMASKEL)
          CALL VECTOR(T3_06,'=','GRADF          X',IELM3,1.D0,VN,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3,MSK,SMASKEL)
          CALL VECTOR(T3_07,'=','GRADF          Z',IELM3,1.D0,UN,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3,MSK,SMASKEL)
          CALL VECTOR(T3_08,'=','GRADF          Z',IELM3,1.D0,VN,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3,MSK,SMASKEL)
!
          IF(NCSIZE.GT.1) THEN
            CALL PARCOM(T3_05 ,2,MESH3)
            CALL PARCOM(T3_06 ,2,MESH3)
            CALL PARCOM(T3_07 ,2,MESH3)
            CALL PARCOM(T3_08 ,2,MESH3)
            CALL PARCOM(USTX ,2,MESH3)
            CALL PARCOM(VSTY ,2,MESH3)
          ENDIF
!
          CALL VECTOR(ST1, '=', 'MASBAS          ',IELM3, 1.D0,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3,.FALSE.,
     &                SMASKEL)
!
          IF(NCSIZE.GT.1) CALL PARCOM(ST1,2,MESH3)
!
          DO IPLAN=1,NPLAN
            DO I=1,NPOIN2
              I3D=(IPLAN-1)*NPOIN2+I
              IF(ST1%R(I3D).GT.1.D-6) THEN
                ST1%R(I3D)   = 1.D0/ST1%R(I3D)
                T3_05%R(I3D) = T3_05%R(I3D)*ST1%R(I3D)
                T3_06%R(I3D) = T3_06%R(I3D)*ST1%R(I3D)
                T3_07%R(I3D) = T3_07%R(I3D)*ST1%R(I3D)
                T3_08%R(I3D) = T3_08%R(I3D)*ST1%R(I3D)
                USTX%R(I3D)  = USTX%R(I3D)*ST1%R(I3D)
                VSTY%R(I3D)  = VSTY%R(I3D)*ST1%R(I3D)
              ELSE
                T3_05%R(I3D) = 0.D0
                T3_06%R(I3D) = 0.D0
                T3_07%R(I3D) = 0.D0
                T3_08%R(I3D) = 0.D0
                USTX%R(I3D)  = 0.D0
                VSTY%R(I3D)  = 0.D0
              ENDIF
            ENDDO
          ENDDO

!         CALCULATION OF WSTOKES
!
          DO I=1,NPOIN2
            DO IPLAN=1,NPLAN
              I3D = (IPLAN-1)*NPOIN2+I
              WST2%R(I3D) = (USTX%R(I3D)+VSTY%R(I3D))
     &                     *(Z(I+NPOIN2*(IPLAN-1))-Z(I))
            ENDDO
          ENDDO

          DO I=1,NPOIN2
            DO IPLAN=1,NPLAN
              I3D = (IPLAN-1)*NPOIN2+I
              WSTOKES%R(I3D) = WST1%R(I)-WST2%R(I3D)
            ENDDO
          ENDDO

!         ADDING THE SOURCE TERMS-VORTEX FORCE ASSOCIATED WITH STOKES DRIFT
          IF(SCV1%TYPR.EQ.'0') THEN
            DO IPLAN=1,NPLAN
              DO I=1,NPOIN2
                I3D=(IPLAN-1)*NPOIN2+I
                CV1(I3D) = VSTOKES%R(I3D)*(T3_06%R(I3D)-T3_05%R(I3D))
     &                   - WSTOKES%R(I3D)*T3_07%R(I3D) - WIPDX%R(I)
                CV2(I3D) = -USTOKES%R(I3D)*(T3_06%R(I3D)-T3_05%R(I3D))
     &                   - WSTOKES%R(I3D)*T3_08%R(I3D) - WIPDY%R(I)
              ENDDO
            ENDDO
            SCV1%TYPR = 'Q'
            SCV2%TYPR = 'Q'
          ELSE
            DO IPLAN=1,NPLAN
              DO I=1,NPOIN2
                I3D=(IPLAN-1)*NPOIN2+I
                CV1(I3D) = CV1(I3D)
     &                   + VSTOKES%R(I3D)*(T3_06%R(I3D)-T3_05%R(I3D))
     &                   - WSTOKES%R(I3D)*T3_07%R(I3D) - WIPDX%R(I)
                CV2(I3D) = CV2(I3D)
     &                   - USTOKES%R(I3D)*(T3_06%R(I3D)-T3_05%R(I3D))
     &                   - WSTOKES%R(I3D)*T3_08%R(I3D) - WIPDY%R(I)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
! TAKES THE VELOCITY OF SOURCES INTO ACCOUNT
!
! NOTE : IF USCE AND VSCE ARE NOT GIVEN, CONSIDERS THAT
!        USCE=UN3 AND VSCE=VN3
!
      IF(NREJEU.GT.0.AND.SCHCVI.NE.ADV_NSC.AND.SCHCVI.NE.ADV_PSI
     &              .AND.SCHCVI.NE.ADV_LPO.AND.SCHCVI.NE.ADV_NSC_TF
     &              .AND.SCHCVI.NE.ADV_LPO_TF) THEN
!
        IF(SCV1%TYPR.EQ.'0') THEN
          CALL OS( 'X=0     ' , X=SCV1 )
          CALL OS( 'X=0     ' , X=SCV2 )
          SCV1%TYPR='Q'
          SCV2%TYPR='Q'
        ENDIF
!
!       ASSEMBLING THE VOLUMES IN PARALLEL
!
        CALL OS('X=Y     ',X=ST1,Y=SVOLU)
        IF(NCSIZE.GT.1) CALL PARCOM(ST1,2,MESH3)
!
!       WITH DISTRIBUTIVE SCHEMES AND FINITE VOLUME SCHEMES
!       THIS IS DONE DIRECTLY INTO SUBROUTINE MURD3D, AND NOT WITH CV1
!
        DO I=1,NREJEU
          IF(ISCE(I).GT.0) THEN
            I3D = (KSCE(I)-1)*NPOIN2+ISCE(I)
            A = MAX(ST1%R(I3D),1.D-6)
            CV1(I3D) = CV1(I3D)+(USCE(I)-UN3(I3D))*QSCE(I)/A
            CV2(I3D) = CV2(I3D)+(VSCE(I)-VN3(I3D))*QSCE(I)/A
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
