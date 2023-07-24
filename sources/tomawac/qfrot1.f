!                   *****************
                    SUBROUTINE QFROT1
!                   *****************
!
     &( TSTOT , TSDER , F     , XK    , NF    , NDIRE , NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE BOTTOM FRICTION
!+                SOURCE TERM BASED ON HASSELMANN ET AL.'S FORMULATION
!+                (1973), MODIFIED BY BOUWS ET KOMEN (1983).
!
!note     THIS SOURCE TERM IS LINEAR IN F(FREQ,TETA), AND THE LINEAR
!+          COEFFICIENT DOES NOT VARY WITH TIME.
!note   CFROT1 (USED IN WAM CYCLE 4) EQUALS 0.038 M2.S-3.
!
!reference  HASSELMANN ET AL. (1973) :
!+                     "MEASUREMENTS OF WIND-WAVE GROWTH AND SWELL
!+                      DECAY DURING THE JOINT NORTH SEA WAVE PROJECT
!+                     (JONSWAP)". DEUTSCHEN HYDROGRAPHISVHEN ZEITSCHRIFT, REIHE A(8), NUM 12.
!reference BOUWS E., KOMEN G.J. (1983) :
!+                     "ON THE BALANCE BETWEEN GROWTH AND DISSIPATION
!+                      IN AN EXTREME DEPTH-LIMITED WIND-SEA IN THE
!+                      SOUTHERN NORTH-SEA". JPO, VOL 13.
!
!history  P. THELLIER; M. BENOIT (EDF/DER/LNH)
!+        03/04/95
!+        V1P0
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
!| CFROT1         |-->| BOTTOM FRICTION COEFFICIENT
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| DIRECTIONAL SPECTRUM
!| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
!| NDIRE          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : GRAVIT, CFROT1, DEPTH
      USE INTERFACE_TOMAWAC, EX_QFROT1 => QFROT1
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    ::  NF , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION COEF , DEUKD, BETA
!
!
      COEF=-2.D0*CFROT1/GRAVIT
!
!.....LOOP OVER DISCRETISED FREQUENCIES
!     """"""""""""""""""""""""""""""""""""""""""""
      DO JF=1,NF
!
!.......COMPUTES THE LINEAR COEFFICIENT BETA : QFROT1 = BETA * F
!       """""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP=1,NPOIN2
          DEUKD = MIN(2.D0*DEPTH(IP)*XK(IP,JF),7.D2)
          BETA = COEF*XK(IP,JF)/SINH(DEUKD)
          DO JP=1,NDIRE
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+BETA*F(IP,JP,JF)
            TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+BETA
          ENDDO
        ENDDO
      ENDDO
!
      RETURN
      END
