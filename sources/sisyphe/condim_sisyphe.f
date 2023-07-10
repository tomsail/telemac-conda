!                   *************************
                    SUBROUTINE CONDIM_SISYPHE
!                   *************************
!
     & (U      , V       , QU    , QV   , H    , ZF , Z ,
     &  ESOMT  , THETAWR ,  Q    , HWR  , TWR  ,
     &  X      , Y       , NPOIN , AT   , PMAREE)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    KEPT FOR RETRO COMPATIBILITY OF USER FORTRAN USE
!+        USER_CONDOM_SISYPHE NOW
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/95
!+        V5P3
!+
!
!history
!+        **/05/2006
!+
!+   THE VARIABLES U AND V, H, MUST BE DEFINED; THE OTHER ONES ARE OPTIONAL
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
!| AT             |-->| TIME
!| ESOMT          |<->| TOTAL BED EVOLUTION
!| H              |<->| WATER DEPTH
!| HWR            |<->| WAVE HEIGHT
!| NPOIN          |-->| NUMBER OF 2D POINTS
!| PMAREE         |-->| TIDAL PERIOD
!| Q              |<->| FLOW RATE
!| THETAWR        |<->| WAVE DIRECTION (WRT OY AXIS)
!| TWR            |<->| WAVE PERIOD
!| X,Y            |-->| COORDINATES
!| Z              |<->| FREE SURFACE
!| ZF             |<->| BED ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)::NPOIN
!
      DOUBLE PRECISION, INTENT(IN):: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN):: AT , PMAREE
! SEDIMENT
      DOUBLE PRECISION, INTENT(INOUT) ::  ZF(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::  ESOMT(NPOIN)
! HYDRODYNAMICS
      DOUBLE PRECISION, INTENT(INOUT):: Z(NPOIN) , H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::QU(NPOIN), QV(NPOIN), Q(NPOIN)
! WAVES
      DOUBLE PRECISION, INTENT (INOUT):: HWR(NPOIN) , TWR(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT):: THETAWR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
      CALL USER_CONDIM_SISYPHE
     & (U      , V       , QU    , QV   , H    , ZF , Z ,
     &  ESOMT  , THETAWR ,  Q    , HWR  , TWR  ,
     &  X      , Y       , NPOIN , AT   , PMAREE)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CONDIM_SISYPHE
