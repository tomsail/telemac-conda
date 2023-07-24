!                   ********************
                    SUBROUTINE ZREF_GAIA
!                   ********************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief After computing fluer, set compute_susp.eq.true for coupling
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!>@todo: Prepares the advection eq:
!!     Takes details of the continuity equation into account
!!     in charac ielmt is intent(inout)
!!
!!     ielmt=ielm
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 1.  COMPUTES THE REFERENCE ELEVATION  -->  ZREF
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     THREE OPTIONS : ICQ=1: FREDSOE REFERENCE CONC. ZREF = 2.D50
!                     ICQ=2: BIJKER METHOD ZREF = MAX(KSP,KS)
!                     ICQ=3: VAN RIJN ZREF= 0.5 KS
!                     ICQ=4: SOULSBY/VAN RIJN ZREF= 0.5 KS
!
!     Each relation proposed to estimate the near-bed concentration
!     under equilibrium conditions has an associated reference height
!     called zref. For further details see Sedimentation Engineering
!     (edited by Garcia, M.)
!
      IF(ICQ.EQ.1) THEN
        CALL OS('X=Y     ', X=ZREF, Y=KSP)
      ELSEIF(ICQ.EQ.2) THEN
        CALL OS('X=Y     ', X=ZREF, Y=KSR)
      ELSEIF(ICQ.EQ.3) THEN
        CALL OS('X=CY    ', X=ZREF, Y=KS,C=0.5D0)
      ELSEIF(ICQ.EQ.4) THEN
        CALL OS('X=CY    ', X=ZREF, Y=KS,C=0.5D0)
      ELSEIF(ICQ.EQ.0) THEN
!     CALL OS('X=CY    ', X=ZREF, Y=KS,C=0.5D0)
!     The value of ZREF must be provided by the user according to the
!     near-bed concentration formula
        WRITE(LU,201)
201     FORMAT(1X,'SUSPENSION_MAIN_GAIA:',/,1X,
     &            'DEFINE ZREF FOR USER FORMULA')
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,203) ICQ
203     FORMAT(1X,'SUSPENSION_MAIN_GAIA:',/,1X,
     &            'REFERENCE CONCENTRATION FORMULA',/,1X,
     &            'UNEXPECTED VALUE:',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      RETURN
      END
