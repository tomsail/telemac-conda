!                   ***************************
                    SUBROUTINE BEDLOAD_INTERACT
!                   ***************************
!
     &(UCMOY,TOBW,TOB,ALPHAW,FW,CF,UW,NPOIN,XMVE,FCW)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE FRICTION COEFFICIENT UNDER
!+                WAVE AND CURRENT COMBINED ACTION.
!
!history  C. VILLARET (LNHE)
!+        01/10/2003
!+        V5P7
!+
!
!history  C. VILLARET (LNHE)
!+        23/09/2010
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALPHAW         |-->| ANGLE OF WAVES WITH OX
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| FCW            |<->| WAVE-CURRENT FRICTION FACTOR
!| FW             |-->| WAVE FRICTION FACTOR
!| NPOIN          |-->| NUMBER OF POINTS
!| TOB            |<->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOBW           |-->| WAVE INDUCED SHEAR STRESS
!| UCMOY          |-->| MEAN CURRENT
!| UW             |-->| ORBITAL WAVE VELOCITY
!| XMVE           |-->| FLUID DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_BEDLOAD_INTERACT => BEDLOAD_INTERACT
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      !
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      !
      TYPE(BIEF_OBJ),   INTENT(IN)  :: UCMOY, TOBW, TOB, ALPHAW
      TYPE(BIEF_OBJ),   INTENT(IN)  :: FW, CF, UW
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: XMVE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FCW
      !
      ! 3/ LOCAL VARIABLES
      ! ------------------
      !
      INTEGER                     :: I
      DOUBLE PRECISION            :: TX, LOGF
      DOUBLE PRECISION            :: CSAL,CSAL1, CSAL3
      DOUBLE PRECISION            :: AX, MX, NX, BX, PX, QX
      DOUBLE PRECISION            :: UCW2, TAUCW,ZERO
!
      INTRINSIC MAX
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ZERO = 1.D-6
!
      DO I = 1, NPOIN
!
        TX = TOB%R(I) / MAX((TOB%R(I) + TOBW%R(I)),ZERO)
!
        LOGF  = LOG10(2.D0*MAX(FW%R(I),ZERO)/MAX(CF%R(I),ZERO))
        CSAL  = ABS(COS(ALPHAW%R(I)))
        CSAL1 = CSAL**0.82D0
        CSAL3 = CSAL**2.70D0
!
        AX = -0.07D0 + 1.87D0*CSAL1 + (-0.34D0 - 0.12D0*CSAL1)*LOGF
        MX =  0.72D0 - 0.33D0*CSAL1 + ( 0.08D0 + 0.34D0*CSAL1)*LOGF
        NX =  0.78D0 - 0.23D0*CSAL1 + ( 0.12D0 - 0.12D0*CSAL1)*LOGF
!
        BX =  0.27D0 + 0.51D0*CSAL3 + (-0.10D0 - 0.24D0*CSAL3)*LOGF
        PX = -0.75D0 + 0.13D0*CSAL3 + ( 0.12D0 + 0.02D0*CSAL3)*LOGF
        QX =  0.89D0 + 0.40D0*CSAL3 + ( 0.50D0 - 0.28D0*CSAL3)*LOGF
!
        IF(TX.LE.ZERO) THEN
          TAUCW = TOBW%R(I)
        ELSEIF(TX.LT.1.D0) THEN
          TAUCW = (1.D0 + BX * TX**PX * (1.D0 - TX)**QX)*TOB%R(I)*TX
     &          + (1.D0 + AX * TX**MX * (1.D0 - TX)**NX)*TOBW%R(I)
        ELSE
          TAUCW = TOB%R(I)
        ENDIF
!
        UCW2 = (UCMOY%R(I)**2 + 0.5D0 * UW%R(I)**2) * XMVE
        FCW%R(I) = TAUCW / MAX(UCW2,1.D-10)
!
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
