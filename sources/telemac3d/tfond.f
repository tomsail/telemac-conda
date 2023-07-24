!                   ****************
                    SUBROUTINE TFOND
!                   ****************
!
     &(AUBOR,CF,U2D,V2D,U3D,V3D,W3D,KARMAN,LISRUF,PROPNU,Z,NPOIN,KFROT,
     & RUGOF,UETCAR,NONHYD,OPTBAN,HN,GRAV,IPBOT,NPLAN)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES AUBOR, COEFFICIENT FOR THE LOG LAW
!+                AT THE BOTTOM.
!
!history  V. BOYER UMIST
!+        **/01/2001
!+
!+   REICHARD LAW
!
!history  J-M HERVOUET (LNHE)
!+        21/06/2010
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
!+        29/12/2011
!+        V6P2
!+   Treatment of tidal flats in case of Nikuradse law.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AUBOR          |<->| FRICTION COEFFICIENT FOR THE LOG LAW ON WALLS
!| CF             |-->| FRICTION COEFFICIENT FOR K-EPSILON
!| GRAV           |-->| GRAVITY ACCELERATION
!| HN             |-->| WATER DEPTH AT TIME N
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| KARMAN         |-->| KARMAN CONSTANT
!| KFROT          |-->| LAW OF BOTTOM FRICTION
!| LISRUF         |-->| TURBULENCE MODEL FOR BOTTOM
!|                |   | 1: SMOOTH  2: ROUGH
!| NONHYD         |-->| LOGICAL FOR NON-HYDROSTATIC OPTION
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
!| NPOIN          |-->| NUMBER OF POINTS
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| PROPNU         |-->| MOLECULAR VISCOSITY COEFFICIENT
!| RUGOF          |-->| FRICTION COEFFICIENT
!| U2D            |-->| 2D (VERTICALLY INTEGRATED) VELOCITY COMPONENT
!| U3D            |-->| 3D VELOCITY COMPONENT
!| UETCAR         |<->| (FRICTION VELOCITY)**2
!| V2D            |-->| 2D (VERTICALLY INTEGRATED) VELOCITY COMPONENT
!| V3D            |-->| 3D VELOCITY COMPONENT
!| W3D            |-->| 3D VELOCITY COMPONENT
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LISRUF,NPOIN,KFROT,OPTBAN,NPLAN
      INTEGER, INTENT(IN) :: IPBOT(NPOIN)
!
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,PROPNU,GRAV
      DOUBLE PRECISION, INTENT(INOUT) :: AUBOR(NPOIN),UETCAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN)
!
!     VECTORS
!
      DOUBLE PRECISION, INTENT(IN) :: CF(*),U2D(*),V2D(*)
      DOUBLE PRECISION, INTENT(IN) :: RUGOF(*),U3D(*),V3D(*),W3D(*),Z(*)
!
      LOGICAL, INTENT(IN) :: NONHYD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,IT,I1
!
      DOUBLE PRECISION UTANG,DIST,UETUTA,YPLUS,VNORM,AUX
!
      INTRINSIC SQRT,LOG
!
!-----------------------------------------------------------------------
!
!     COMPUTES UETOIL ** 2 FOR THE SOLID BOUNDARIES
!     ----------------------------------------
!
!     SMOOTH FRICTION REGIME :
!
!     ********************
      IF(LISRUF.EQ.1) THEN
!     ********************
!
        DO N=1,NPOIN
!
!         APPLIES LOG LAW AT THE FIRST PLANE TO DETERMINE U*/UTANG
!         WHICH IS CALLED UETUTA
!
!         TAKES INTO ACCOUNT CRUSHED PLANES
!         I1 FIRST POINT WITH WATER ABOVE (EXCEPT CASE OF TIDAL FLATS
!         WHERE THE LEVEL UNDER THE FREE SURFACE IS TAKEN)
          I1=(MIN(NPLAN-1,IPBOT(N)+1)-1)*NPOIN+N
!
          UTANG=SQRT(U3D(I1+NPOIN)**2+V3D(I1+NPOIN)**2)
!
          DIST  = Z(NPOIN+I1)-Z(I1)
!
!         ITERATIONS TO GET UETUTA
          UETUTA = 6.D-2
          DO IT=1,10
!           REICHARD LAW
!           YPLUS>30 IN THE LOGARITHMIC LAYER, OTHERWISE POSSIBLE
!           DIVISION BY ZERO, IF YPLUS=0
            YPLUS = MAX(DIST*UETUTA*UTANG/PROPNU,30.D0)
            UETUTA = 1.D0/(LOG(1.D0+KARMAN*YPLUS)/KARMAN+7.8D0*
     &      (1.D0-EXP(-YPLUS/11.D0)-YPLUS/11.D0*EXP(-0.33D0*YPLUS)))
          ENDDO
!
          UETCAR(N) = (UETUTA*UTANG)**2
!
        ENDDO
!
!     ***************************************
      ELSEIF(LISRUF.EQ.2.OR.LISRUF.EQ.3) THEN
!     ***************************************
!
!       ROUGH FRICTION REGIME
!
        IF( KFROT.EQ.0.OR.KFROT.EQ.2.OR.
     &      KFROT.EQ.3.OR.KFROT.EQ.4     ) THEN
!
!         NEW AUBOR TO GET THE SAME CALIBRATION THAN IN 2D
!         LAWS BASED ON STRICKLER OR CHEZY
          DO N=1,NPOIN
            UETCAR(N)=(U2D(N)**2+V2D(N)**2)*0.5D0*CF(N)
          ENDDO
!
        ELSEIF(KFROT.EQ.5) THEN
!
!         APPLIES LOG LAW AT THE FIRST PLANE TO DETERMINE U*
!         THE BOTTOM
!
          DO N=1,NPOIN
!           TAKES INTO ACCOUNT CRUSHED PLANES
!           I1 FIRST POINT WITH WATER ABOVE (EXCEPT CASE OF TIDAL FLATS
!           WHERE THE LEVEL UNDER THE FREE SURFACE IS TAKEN)
            I1=(MIN(NPLAN-1,IPBOT(N)+1)-1)*NPOIN+N
            DIST  = Z(NPOIN+I1)-Z(I1)
            AUX=MAX(1.001D0,30.D0*DIST/RUGOF(N))
            UETCAR(N)=(KARMAN/LOG(AUX))**2
     &               * (U3D(I1+NPOIN)**2+V3D(I1+NPOIN)**2)
          ENDDO
!
        ELSE
!
          WRITE(LU,403) KFROT
403       FORMAT(1X,'TFOND : UNKNOWN LAW OF FRICTION : ',1I6)
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!     ****
      ELSE
!     ****
!
        WRITE(LU,401) LISRUF
401     FORMAT(1X,'TFOND : UNKNOWN TURBULENCE MODEL : ',1I6)
        CALL PLANTE(1)
        STOP
!
!     *****
      ENDIF
!     *****
!
!
!-----------------------------------------------------------------------
!
!     FINAL COMPUTATION OF AUBOR = - (U*)**2 / U(BOTTOM)
!
!-----------------------------------------------------------------------
!
!     ON TIDAL FLATS A MINIMUM VELOCITY IS IMPOSED (U*)**2=GH
!     TO OPPOSE SOME FRICTION TO FREE SURFACE GRADIENTS
!
      IF(OPTBAN.EQ.1) THEN
        DO N=1,NPOIN
          IF(HN(N).LT.1.D-3) THEN
            UETCAR(N)=MAX(GRAV*MAX(HN(N),1.D-7),UETCAR(N))
          ENDIF
        ENDDO
      ENDIF
!
      IF(NONHYD) THEN
!
!       NOTE: ON TIDAL FLATS THE UPPER LAYER IS TAKEN FOR VNORM
!             MAYBE NOT USEFUL IF ALL VALUES EQUAL UP TO IPBOT+1
!
        DO N=1,NPOIN
          I1=IPBOT(N)*NPOIN+N
          VNORM=SQRT(U3D(I1)**2+V3D(I1)**2+W3D(I1)**2)
          AUBOR(N)= - UETCAR(N)/MAX(1.D-4,VNORM)
        ENDDO
!
      ELSE
!
        DO N=1,NPOIN
          I1=IPBOT(N)*NPOIN+N
          VNORM=SQRT(U3D(I1)**2+V3D(I1)**2)
          AUBOR(N)= - UETCAR(N)/MAX(1.D-4,VNORM)
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
