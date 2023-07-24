!                   ***********************
                    SUBROUTINE TOBW_SISYPHE
!                   ***********************
!
     &(TOBW ,CF, FW, UW,TW,HN,NPOIN,XMVE)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE WAVE FRICTION STRESS. THE FRICTION
!+                COEFFICIENT IS COMPUTED USING SWART FORMULATION (1976).
!
!history  C. VILLARET (LNHE)
!+        01/10/2003
!+        V5P4
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
!+   Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| FW             |-->| QUADRATIC FRICTION COEFFICIENT (WAVE)
!| HN             |-->| WATER DEPTH AT TIME N
!| NPOIN          |-->| NUMBER OF POINTS
!| TOBW           |-->| TOTAL BED SHEAR STRESS
!| TW             |-->| WAVE PERIOD
!| UW             |-->| ORBITAL VELOCITY (WAVE)
!| XMVE           |-->| FLUID DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
!
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: UW(NPOIN),TW(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XMVE
      DOUBLE PRECISION, INTENT(INOUT) :: TOBW(NPOIN),FW(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION KS,AUX
      DOUBLE PRECISION PI,AW
      DOUBLE PRECISION, PARAMETER :: KARMAN = 0.4D0
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
!
!-----------------------------------------------------------------------
!
      DO  I=1,NPOIN
!       KS : NIKURADSE COEFFICIENT (TOTAL FRICTION)
        AUX=1.D0+KARMAN*SQRT(2.D0/MAX(CF(I),1.D-10))
        KS=30.D0*MAX(HN(I),1.D-8)*EXP(-AUX)
        AW= UW(I)*TW(I) / (2.D0*PI)
        IF(AW/KS.GT.1.59D0) THEN
          FW(I)=EXP( -6.D0 + 5.2D0 * (AW/KS)**(-0.19D0) )
        ELSE
          FW(I)=0.3D0
        ENDIF
        TOBW(I)=0.5D0 * XMVE * FW(I) * UW(I)*UW(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE TOBW_SISYPHE
