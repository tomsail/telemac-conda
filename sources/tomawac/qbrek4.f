!                   *****************
                    SUBROUTINE QBREK4
!                   *****************
!
     &( TSTOT , F     , FCAR  , VARIAN, NF    , NDIRE , NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE DEPTH-INDUCED
!+                BREAKING SOURCE TERM BASED ON IZUMIYA ET HORIKAWA (1984).
!
!note     THIS SOURCE TERM IS LINEAR IN F(FREQ,TETA), AND THE LINEAR
!+          COEFFICIENT DOES NOT VARY WITH TIME.
!
!reference  IZUMIYA T., HORIKAWA K. (1984) :
!+                     "WAVE ENERGY EQUATION APPLICABLE IN AND OUTSIDE
!+                      THE SURF ZONE". COASTAL ENGINEERING IN JAPAN, VOL 17, PP 119-137.
!
!history  F. BECQ; M. BENOIT (EDF/DER/LNH)
!+        26/03/96
!+        V1P1
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
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| DIRECTIONAL SPECTRUM
!| FCAR           |-->| CHARACTERISTIC FREQUENCY
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| VARIAN         |-->| SPECTRUM VARIANCE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : BETAIH, EM2SIH, GRAVIT, DEPTH,
     &                BETABR
      USE INTERFACE_TOMAWAC, EX_QBREK4 => QBREK4
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)   ::          NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)   :: VARIAN(NPOIN2),FCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , IFF   , IP
      DOUBLE PRECISION COEF  , XKCAR , DEUKD , GG1   , GG2
!
!
      COEF   = -SQRT(GRAVIT)*BETAIH
!
!.....COMPUTES THE LINEAR COEFFICIENT BETABR : QBREK4 = BETABR * F
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""
      DO IP = 1,NPOIN2
        CALL WNSCOU( XKCAR, FCAR(IP), DEPTH(IP) )
        DEUKD=2.D0*XKCAR*DEPTH(IP)
        IF (DEUKD.GT.7.D2) THEN
          GG1 = 0.D0
          GG2 = 0.5D0
        ELSE
          GG1 = DEUKD/SINH(DEUKD)
          GG2 = 0.5D0*(1.D0+GG1)
        ENDIF
        BETABR(IP) = COEF/DEPTH(IP)**1.5*SQRT(VARIAN(IP)*GG1)
     &               *SQRT(MAX(0.D0,GG2*VARIAN(IP)
     &               /(DEPTH(IP)*DEPTH(IP))-EM2SIH))
        DO IFF = 1,NF
          DO JP = 1,NDIRE
            TSTOT(IP,JP,IFF) = TSTOT(IP,JP,IFF)+BETABR(IP)*F(IP,JP,IFF)
          ENDDO                 ! JP
        ENDDO                   ! IFF
      ENDDO                     ! IP
!
      RETURN
      END
