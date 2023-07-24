!                   *****************
                    SUBROUTINE CSTKEP
!                   *****************
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief    SETS CONSTANTS OF K-EPSILON AND K-OMEGA MODELS.
!
!history  VINCENT BOYER
!+        01/02/01
!+
!+
!
!history  OLIVER GOETHEL
!+        18/03/04
!+
!+
!
!history  J-M HERVOUET(LNH)
!+        14/12/09
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        164/12/2012
!+        V6P3
!+   New parameters for monitoring k-epsilon, all arguments suppressed.
!
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  C.-T. PHAM (EDF, LNHE)
!+        01/03/2017
!+        V7P2
!+   Allowing k-epsilon model on a direction and not on the other.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC3D, ONLY : CMU,C1,C2,SIGMAK,SIGMAE,
     &                                   VIRT,KMIN,KMAX,
     &                                   EMIN,EMAX,ALPHA,
     &                                   BETA,BETAS,OMSTAR,ITURBV,
     &                                   ITURBH,CLIPK,CLIPE,WSIK,YAP,
     &                                   PERNORM2,PERPROD,RIMIN,RIMAX,
     &                                   OPTPROD,LIMKF,LIMEF,LIMKS,LIMES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!
!=======================================================================
!
!     ALL MODELS
!
!=======================================================================
!
!     VON KARMAN CONSTANT
!
!     UP TO VERSION 6.0, 0.41  FROM NOW ON : 0.40
!     FROM 7.0 USE KEYWORDS
!      KARMAN = 0.40D0
!
!     SCHMIDT NUMBER (not used)
!
!      SCHMIT = 1.D0
!
!     PRANDTL NUMBER (BETWEEN 0.8 AND 0.9 FOR TEMPERATURE)
!
!     Prandtl number should be 1 by default for sediment
!      PRANDTL = 1.D0
!
!     K-EPSILON OR K-OMEGA MODEL
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
        SIGMAK = 1.D0
        SIGMAE = 1.3D0
      ELSEIF(ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
        SIGMAK = 2.D0
        SIGMAE = 2.D0
      ENDIF
!
!=======================================================================
!
!     K-EPSILON MODEL
!
!=======================================================================
!
      CMU    = 0.09D0
      C1     = 1.44D0
      C2     = 1.92D0
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITIONS AT BOTTOM AND FREE SURFACE
!
!-----------------------------------------------------------------------
!
!     K : K
!     E : EPSILON
!     F : BOTTOM
!     S : FREE SURFACE
!     1 : NEUMANN
!     2 : DIRICHLET (VALUES COMPUTED IN KEPCL3)
!
      LIMKF = 1
      LIMEF = 1
      LIMKS = 1
      LIMES = 1
!
!-----------------------------------------------------------------------
!
!     PARAMETERS USED IN SUBROUTINE SOUKEP
!
!-----------------------------------------------------------------------
!
!     LIMITATION OF K AND EPSILON WITH PHYSICAL CRITERIA
!
      CLIPK    = .TRUE.
      CLIPE    = .TRUE.
      PERNORM2 = 0.5D0
      PERPROD  = 0.1D0
!
!     MIN AND MAX OF RICHARDSON NUMBER
!
      RIMIN=0.D0
      RIMAX=100.D0
!
!     OPTION FOR PRODUCTION
!     1: LINEAR (NOT STANDARD)
!     2: QUADRATIC (STANDARD)
!
      OPTPROD=2
!
!     WIND STRESS IN K
!
      WSIK=.TRUE.
!
!     YAP CORRECTION
!
      YAP=.FALSE.
!
!=======================================================================
!
!     K-OMEGA MODEL
!
!=======================================================================
!
      ALPHA  = 5.D0/9.D0
      BETA   = 3.D0/40.D0
      BETAS  = 0.09D0
!
!     TO COMPUTE THE FREE SURFACE VALUE OF OMEGA
!
      OMSTAR  = 100.D0
!
!     VIRTUAL ORIGIN FOR EPSILON
!
      VIRT = 0.07D0
!
!     MINIMA AND MAXIMA FOR CLIPPING
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
        KMIN = 1.D-10
        EMIN = 1.D-16
        KMAX = 1.D4
        EMAX = 1.D10
      ELSEIF(ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
        KMIN = 1.D-8
        EMIN = 1.D-3
        KMAX = 1.D-1
        EMAX = 1.D4
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

