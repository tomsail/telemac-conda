!                   *****************************
                    SUBROUTINE BEDLOAD_SECCURRENT
!                   *****************************
!
     &(IELMU,CALFA,SALFA)
!
!***********************************************************************
! SISYPHE   V7P2                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE NEW TAU FROM SECONDARY CURRENTS.
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
!history  R. KOPMANN, U.MERKEL
!+         20/03/2011
!+         V6P1
!+
!history  R KOPMANN (BAW)
!+        10/05/2016
!+        V7P2
!+ CALFA,SALFA dependent of grain classes
!
!history  R KOPMANN (BAW)
!+        14/03/2017
!+        V7P2
!+   Reading the bend radii from GEOMETRY FILE IF SECONDARY CURRENT FILE = YES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELMU          |-->| TYPE OF ELEMENT FOR VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IELMU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION C, ALPHAL
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CALFA, SALFA
!

!
!FORMULATION FROM ENGELUND WITH GEOMETRIC RADIUS: TAN(TETA) = 7*(H/R)
!

! RADIUS OF CURVATURE MUST BE CALCULATED FROM FREE SURFACE SLOPE
! USE OF ALPHA (0.75 FOR VERY ROUGH BOTTOMS, 1 FOR SMOOTH ONES)

!     RK MODIFICATION FOR SECONDARY CURRENTS
!
!     COMPUTES THE GRADIENT OF THE FREE SURFACE IN X AND Y DIRECTION
!
      CALL VECTOR(T5,'=','GRADF          X',IELMU,
     &            1.D0,Z,S,S,S,S,S,MESH,MSK,MASKEL)
      CALL VECTOR(T6,'=','GRADF          Y',IELMU,
     &            1.D0,Z,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     FOR PARALLEL COMPUTING
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM (T5, 2, MESH)
        CALL PARCOM (T6, 2, MESH)
      ENDIF
!
      CALL OS('X=XY    ',X=T5,Y=UNSV2D)
      CALL OS('X=XY    ',X=T6,Y=UNSV2D)
!
!     COMPUTES THE X- AND Y-COMPONENTS OF THE SECONDARY CURRENT
!     ACCORDING TO ENGELUNG. TAU_X_SEC = C*QV, TAU_Y_SEC = C*QU
!

! BEWARE: THE VARIABLE ALPHA IS MORE THAN THE ALPHA FROM THE THEORY
      ALPHAL = ALPHA
      IF(ALPHA.GT.0.D0) THEN
        ALPHAL = 7.D0 / ALPHAL * XMVE *GRAV
      ELSE
        ALPHAL = 0.D0
      ENDIF
! CASE OF RADII FROM FILE
      IF(HAVESECFILE) THEN
! ALPHA IS CALIBRATION FACTOR
        ALPHAL = 7.D0/ALPHA
! CALCULATING
        CALL OS('X=-Y     ', X=T1, Y=RADSEC) ! -RADIUS
        CALL OS('X=Y/Z    ', X=T8, Y=HN, Z=T1) ! HN/-RADIUS
        CALL OS('X=CY     ', X=T8, Y=T8, C=ALPHAL) ! ALPHAL*HN/-RADIUS
        CALL OS('X=ATN(Y) ', X=T8, Y=T8) ! ARCTAN(7*HN/-RADIUS)
      ELSE
! CALCULATING THE RADII FROM WATER LEVEL SLOPE
!
      CALL OS('X=YZ    ', X=T1, Y=T6, Z=U2D) ! DZSDY*U2D
      CALL OS('X=YZ    ', X=T2, Y=T5, Z=V2D) ! DZSDX*V2D
      CALL OS('X=X-Y   ', X=T1, Y=T2) ! U2D*DZSDY - V2D*DZSDX
!
      CALL OS('X=YZ    ', X=T2, Y=U2D, Z=U2D) ! U2D**2
      CALL OS('X=YZ    ', X=T3, Y=V2D, Z=V2D) ! V2D**2
      CALL OS('X=X+Y   ', X=T2, Y=T3) ! U2D**2+V2D**2
!
      CALL OS('X=Y/Z   ', X=T1, Y=T1, Z=T2, C=C,
     &                    IOPT=2 , INFINI=0.D0, ZERO=1.D-12) !(U2D*DZSDY - V2D*DZSDX)/(U2D**2+V2D**2)
!
      CALL OS('X=CX    ', X=T1, C=ALPHAL) ! T1 * 7/ALPHAL*XMVE*GRAV
      CALL OS('X=XY    ', X=T1, Y=HN) ! T1*HN
!
!     FOR ALL ROUGHNESS LAWS
!
      CALL OS('X=CXY   ', X=T1 , Y=CF , C=0.5D0 )!T1*CF/2
!
!     TAU_X_SEK = -C*V2D : T5
!     TAU_Y_SEK =  C*U2D : T6
!
      CALL OS('X=YZ    ', X=T5 , Y=T1    , Z=V2D) ! C*V2D
      CALL OS('X=YZ    ', X=T6 , Y=T1    , Z=U2D) ! C*U2D
      CALL OS('X=-Y    ', X=T6 , Y=T6    , Z=QV ) ! -C*U2D
!
!     SQRT(TAU_X_SEK**2+TAU_Y_SEK**2) : T3
!
      CALL OS('X=YZ    ', X=T2, Y=T5, Z=T5) ! T2 = (C*V2D)**2
      CALL OS('X=YZ    ', X=T3, Y=T6, Z=T6) ! T3 = (C*U2D)**2
      CALL OS('X=X+Y   ', X=T2, Y=T3) ! T2 = (C*V2D)**2+(C*U2D)**2
      CALL OS('X=SQR(Y)', X=T3, Y=T2) ! T3 = SQRT((C*U2D)**2+(C*V2D)**2
!
!     TAU_X_GES = TOB*EFFPNT*CALFA + TAU_X_SEK : T1
!     TAU_Y_GES = TOB*EFFPNT*SALFA + TAU_Y_SEK : T2
!
      ENDIF

      CALL OS('X=YZ    ', X=T1, Y=TOB, Z=COEFPN) ! TOB*EFFPNT
      CALL OS('X=YZ    ', X=T2, Y=T1 , Z=SALFA) ! TOB*EFFPNT*SALFA
      CALL OS('X=YZ    ', X=T1, Y=T1 , Z=CALFA) ! TOB*EFFPNT*CALFA
!
      IF(HAVESECFILE) THEN
!       CALCULATION TAU_X_COR: TAU_X_COR=TAU_X*COS(TETA)-Y*SIN(TETA)
        CALL OS('X=COS(Y)', X=T3, Y=T8) !COS(TETA)
        CALL OS('X=SIN(Y)', X=T4, Y=T8) !SIN(TETA)
        CALL OS('X=YZ    ', X=T5, Y=T3, Z=T1) !TAU_X*COS(TETA)
        CALL OS('X=YZ    ', X=T6, Y=T4, Z=T2) !TAU_Y*SIN(TETA)
        CALL OS('X=Y-Z   ', X=T5, Y=T5, Z=T6) ! TAU_X_COR=TAU_X*COS(TETA)-TAU_Y*SIN(TETA)
!       CALCULATION TAU_Y_COR: TAU_Y_COR=TAU_Y*SIN(TETA)+TAU_Y*COS(TETA)
        CALL OS('X=YZ    ', X=T6, Y=T4, Z=T1) !TAU_X*SIN(TETA)
        CALL OS('X=YZ    ', X=T7, Y=T3, Z=T2) !TAU_Y*COS(TETA)
        CALL OS('X=X+Y   ', X=T6, Y=T7) ! TAU_Y_COR=TAU_Y*COS(TETA)+TAU_Y*SIN(TETA)
        CALL OS('X=Y     ', X=T1, Y=T5)
        CALL OS('X=Y     ', X=T2, Y=T6)
      ELSE
        CALL OS('X=X+Y   ', X=T1, Y=T5) ! TAU_X_GES = TOB*CALFA+TAU_X_SEK
        CALL OS('X=X+Y   ', X=T2, Y=T6) ! TAU_Y_GES = TOB*SALFA+TAU_Y_SEK
!
      ENDIF
!
!     TAU_GES=SQRT(TAU_X_GES**2+TAU_Y_GES**2)
!
      CALL OS('X=YZ    ', X=T3, Y=T1, Z=T1) ! TAU_X_GES**2
      CALL OS('X=YZ    ', X=T4, Y=T2, Z=T2) ! TAU_Y_GES**2
      CALL OS('X=X+Y   ', X=T4, Y=T3) ! TAU_X_GES**2+TAU_Y_GES**2
      CALL OS('X=SQR(Y)', X=T4, Y=T4) ! SQRT(TAU_X_GES**2+TAU_Y_GES**2)
!
!     NEW ANGLE
!     CALFA_NEW = COS(TAU_X_GES/TAU_GES)
!     SALFA_NEW = SIN(TAU_Y_GES/TAU_GES)
!
      CALL OS('X=Y/Z   ', X=T1, Y=T1, Z=T4, C=C ,
     &        IOPT=2, INFINI=0.D0, ZERO=1.D-12) !TAU_X_GES/TAU_GES
      CALL OS('X=Y/Z   ', X=T2, Y=T2, Z=T4, C=C ,
     &        IOPT=2, INFINI=0.D0, ZERO=1.D-12) !TAU_Y_GES/TAU_GES
!
!     TAKEN FROM EFFPNT UEBER
!     TO MAKE SURE THAT TAU_X_GES/TAU_GES IS IN RANGE [-1,1]
!
      DO I=1,NPOIN
        IF(T1%R(I).LT.-1.D0.OR.T1%R(I).GT.1.D0.OR.
     &     T2%R(I).LT.-1.D0.OR.T2%R(I).GT.1.D0) THEN
          WRITE(LU,*) 'NOT ACCEPTABLE BORDER CROSSING',I
        ENDIF
        T1%R(I) = MIN(T1%R(I),1.D0)
        T1%R(I) = MAX(T1%R(I),-1.D0)
        T2%R(I) = MIN(T2%R(I),1.D0)
        T2%R(I) = MAX(T2%R(I),-1.D0)
      ENDDO
!
!     COEFPN_NEW = TAU_GES / TOB
!
      CALL OS('X=Y/Z   ' ,X=COEFPN, Y=T4 , Z=TOB,
     &                    IOPT=2 , INFINI=0.D0, ZERO=1.D-12) !COEFPN=TAU_GES/TOB
      CALL OS('X=Y     ' ,X=CALFA ,Y=T1) ! (TAU_X_GES/TAU_GES)
      CALL OS('X=Y     ' ,X=SALFA ,Y=T2) ! (TAU_Y_GES/TAU_GES)
!
!     FROM EFFPNT
!     FOR BOUNDARY NODES WITH IMPOSED FLOW :
!     QS IS NOT MODIFIED WHEN USER-DEFINED
!
      DO I = 1 , NPTFR
        IF(LIQBOR%I(I).EQ.5) THEN
          COEFPN%R(MESH%NBOR%I(I)) = 1.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
