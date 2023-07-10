!                   *****************
                    SUBROUTINE CALCFW
!                   *****************
!
     &(I,H,K,HMU,
     & NPOIN,OMEGA,GRAV,
     & VISCO,DIAM90,DIAM50,MVSED,MVEAU,
     & REGIDO,RICOEF,
     & ENTREG,ENTRUG,FFW)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE BOTTOM FRICTION COEFFICIENT FW
!+                FOR SANDY BOTTOMS.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH) ; D. PAUGAM ( PLACEMENT)
!+        02/06/1999
!+        V5P1
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
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIAM50         |-->| MAXIMUM GRAIN DIAMETER WHICH DEFINES 50% OF THE
!|                |   | TOTAL WEIGTH OF SEDIMENT
!| DIAM90         |-->| MAXIMUM GRAIN DIAMETER WHICH DEFINES 90% OF THE
!|                |   | TOTAL WEIGTH OF SEDIMENT
!| ENTREG         |-->| LOGICAL USED TO IMPOSE THE HYDRAULIC REGIME
!|                |   | (FOR FRICTION CALCULATION)
!| ENTRUG         |-->| LOGICAL USED TO RESTRICT THE TOTAL ROUGHNESS TO
!|                |   | THE SKIN ROUGHNESS (FOR FRICTION CALCULATION)
!| FFW            |<--| FRICTION FACTOR
!| GRAV           |-->| GRAVITY
!| H              |-->| WATER HEIGHT
!| HMU            |-->| WAVE HEIGHT
!| I              |-->| CURRENT POINT NUMBER
!| K              |-->| WAVE NUMBER
!| MVEAU          |-->| FLUID SPECIFIC WEIGTH
!| MVSED          |-->| SEDIMENT SPECIFIC WEIGTH
!| NPOIN          |-->| NUMBER OF POINT
!| OMEGA          |-->| WAVE  PULSATION
!| REGIDO         |-->| TYPE OF HYDRAULIC REGIME
!| RICOEF         |-->| RIPPLES COEFFICIENT
!| VISCO          |-->| KINEMATIC VISCOSITY OF THE FLUID
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_ARTEMIS, EX_CALCFW => CALCFW
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(IN) :: H(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: K(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: HMU(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: GRAV
      DOUBLE PRECISION, INTENT(IN) :: OMEGA
      DOUBLE PRECISION, INTENT(IN) :: VISCO
      DOUBLE PRECISION, INTENT(IN) :: DIAM90
      DOUBLE PRECISION, INTENT(IN) :: DIAM50
      DOUBLE PRECISION, INTENT(IN) :: MVSED
      DOUBLE PRECISION, INTENT(IN) :: MVEAU
      DOUBLE PRECISION, INTENT(IN) :: RICOEF
      INTEGER, INTENT(IN) :: REGIDO
      LOGICAL, INTENT(IN) :: ENTREG
      LOGICAL, INTENT(IN) :: ENTRUG
      DOUBLE PRECISION, INTENT(INOUT) :: FFW
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER REGIME
!
      DOUBLE PRECISION KSRUGO, AEX, UEX
      DOUBLE PRECISION RAP1, RAP2
      DOUBLE PRECISION LL1
      DOUBLE PRECISION RMAX1, RMIN1, RMAX2, RMIN2
      DOUBLE PRECISION SP, PSI, KS1, RH, RS, KS2
      DOUBLE PRECISION DIAMAD, TETACR, TAUCR
!
!
!
      INTRINSIC EXP
!
!
!  COMPUTES FW OVER THE WHOLE DOMAIN
!--------------------------------------------------------
      RMAX1 = 0.D0
      RMIN1 = 1.D7
      RMAX2 = 0.D0
      RMIN2 = 1.D7
!
      KSRUGO = 3.D0 * DIAM90
!
! COMPUTES AEX AND UEX AT EACH GRID NODE
! THUS THE HYDRAULIC REGIME WILL ALSO BE GIVEN AT EACH
! GRID NODE
!
!
      AEX = HMU(I)/(2.D0*SINH(K(I)*H(I)))
      UEX = OMEGA * AEX
      RAP1 = (UEX*AEX)/VISCO
      RAP2 = AEX/KSRUGO
!
!   COMPARES AGAINST THE MAXIMUM REYNOLDS NUMBER
!   AND THE EXCURSION RATIO ON ROUGHNESS
!
      IF (RAP1 .GT. RMAX1) THEN
        RMAX1 = RAP1
      ENDIF
!
      IF (RAP1 .LT. RMIN1) THEN
        RMIN1 = RAP1
      ENDIF
!
      IF (RAP2 .GT. RMAX2) THEN
        RMAX2 = RAP2
      ENDIF
!
      IF (RAP2 .LT. RMIN2) THEN
        RMIN2 = RAP2
      ENDIF
!
!-----------------------------------------------------------------
! DETERMINES THE HYDRAULIC REGIME
!-----------------------------------------------------------------
!
      IF (ENTREG) THEN
        REGIME = REGIDO
      ELSE
!
!     INITIALIZES THE REGIME (SETS TO 0) BEFORE EACH NEW ITERATION
!
        REGIME = 0
        LL1 = 0.D0
!
        IF (RAP1 .LE. 10250.D0) THEN
          LL1 = 0.0322D0*RAP1+3.33D0
          IF (RAP2 .GE. LL1) THEN
            REGIME = 1
!           WRITE(LU,*) 'THE HYDRAULIC REGIME IS LAMINAR'
          ENDIF
        ENDIF
!
        IF (RAP1 .GE. 3.D4) THEN
          LL1 = 0.009792D0*RAP1+208.33D0
          IF (RAP2 .GE. LL1) THEN
            REGIME = 2
!           WRITE(LU,*) 'THE HYDRAULIC REGIME IS SMOOTH TURBULENT'
          ENDIF
        ENDIF
!
        IF (RAP1 .GE. 5.D3 .AND. RAP1 .LE. 2.D4) THEN
          LL1 = 0.026D0*RAP1-12.D0
          IF (RAP2 .LE. LL1) THEN
            REGIME = 3
!           WRITE(LU,*) 'THE HYDRAULIC REGIME IS ROUGH TURBULENT'
          ENDIF
        ENDIF
!
        IF (RAP1 .GT. 2.D4) THEN
          LL1 = 0.00099D0*RAP1+30.30D0
          IF (RAP2 .LE. LL1) THEN
            REGIME = 3
!           WRITE(LU,*) 'THE HYDRAULIC REGIME IS ROUGH TURBULENT'
          ENDIF
        ENDIF
!
        IF (REGIME .EQ. 0) THEN
          REGIME = 3
!         WRITE(LU,*) 'THE HYDRAULIC REGIME IS ROUGH TURBULENT'
        ENDIF
!
      ENDIF
!
!
!--------------------------------------------------------------
! COMPUTES FW: THE COEFFICIENT OF FRICTION
!--------------------------------------------------------------
!
      IF (REGIME .EQ. 1) THEN
        FFW = 2.D0*(((AEX*UEX)/VISCO)**(-0.5D0))
      ENDIF
!
      IF (REGIME .EQ. 2) THEN
        FFW = 0.09*(((AEX*UEX)/VISCO)**(-0.2D0))
      ENDIF
!
      IF (REGIME .EQ. 3) THEN
!
        IF (ENTRUG) THEN
!
          KSRUGO = 3.D0 * DIAM90
!
        ELSE
!
          SP = MVSED/MVEAU
          PSI = (UEX**2.D0)/((SP-1.D0)*GRAV*DIAM50)
!
          IF (PSI .LT. 250.D0) THEN
            KS1 = 3.D0*DIAM90
!
            IF (PSI .LT. 10.D0) THEN
              RH = 0.22D0*AEX
              RS = 0.18D0
            ELSE
              RH = AEX*(2.4D-13)*((250.D0-PSI)**5.D0)
              RS = (2.D-7)*((250.D0-PSI)**2.5D0)
            ENDIF
!
            KS2 = 20.D0*RICOEF*RH*RS
!
!  RICOEF = 1     RIPPLES ONLY
!  RICOEF = 0.7D0 RIPPLES AND SAND WAVES
!
            ELSE
              KS1 = 3.D0*(0.04D0*PSI-9.D0)*DIAM90
              KS2 = 0.D0
              IF (KS1 .LT. 0.01D0) THEN
                KS1 = 3.D0*DIAM90
              ENDIF
            ENDIF
!
            KSRUGO = KS1+KS2
            KS1 = (AEX/KSRUGO)
!
          ENDIF
!
          FFW = EXP(-6.D0+5.2D0*((AEX/KSRUGO)**(-0.19D0)))
          IF (FFW .GT. 0.3D0) THEN
            FFW = 0.3D0
          ENDIF
        ENDIF
!
        IF (REGIME .EQ. 4) THEN
!
!     TRANSIENT STATE NOT TAKEN INTO ACCOUNT
!
          IF (ENTRUG) THEN
!
            KSRUGO = 3.D0 * DIAM90
!
          ELSE
!
            DIAMAD = (((MVSED/MVEAU)-1.D0)*GRAV)/(VISCO**2.D0)
            DIAMAD = DIAMAD**(1/3D0)
            DIAMAD = DIAMAD * DIAM50
!
            TETACR = 0.14D0*(DIAMAD**(-0.64D0))
!
            TAUCR = TETACR*(MVSED-MVEAU)*GRAV*DIAM50
!
            KSRUGO = (3.D0*DIAM90)+((3.3D0*VISCO)/
     &               ((TAUCR/MVEAU)**0.5D0))
!
          ENDIF
!
          FFW = EXP(-6.D0+5.2D0*((AEX/KSRUGO)**(-0.19D0)))
          IF (FFW .GT. 0.3D0) THEN
            FFW = 0.3D0
          ENDIF
!
        ENDIF
!
      RETURN
      END
