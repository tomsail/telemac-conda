!                   *****************
                    SUBROUTINE KMOYEN
!                   *****************
!
     &(XKMOY, XK, F, NF, NDIRE, NPOIN2, AUX1, AUX2, AUX3)
!
!***********************************************************************
! TOMAWAC   V6P3                                   20/06/2011
!***********************************************************************
!
!brief    COMPUTES THE AVERAGE WAVE NUMBER FOR ALL THE NODES
!+                IN THE 2D MESH.
!
!note     THE HIGH-FREQUENCY PART OF THE SPECTRUM IS ONLY CONSIDERED
!+          IF THE TAIL FACTOR (TAILF) IS STRICTLY GREATER THAN 1.
!
!history  P. THELLIER; M. BENOIT (EDF/DER/LNH)
!+        04/04/95
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
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        18/01/2013
!+        V6P3
!+   ARITHMETIC AVERAGE WHEN ENERGY WEIGHTED AVERAGE IS NOT POSSIBLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AUX1           |<->| WORK TABLE
!| AUX2           |<->| WORK TABLE
!| AUX3           |<->| WORK TABLE
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |---| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TAILF          |-->| SPECTRUM QUEUE FACTOR
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKMOY          |<--| AVERAGE WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, GRAVIT, FREQ, DFREQ, TAILF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_KMOYEN => KMOYEN
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: AUX1(NPOIN2),AUX2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: AUX3(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: XKMOY(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  IDIRE , JF    , IP
      DOUBLE PRECISION COEFF , SEUIL , CTE1  , CTE2  , AUX4
!
!-----------------------------------------------------------------------
!
      SEUIL = 1.D-20
      COEFF = SQRT(GRAVIT)/DEUPI
!
      DO IP = 1,NPOIN2
        AUX1(IP) = 0.D0
        AUX2(IP) = 0.D0
      ENDDO
!
!     SUMS UP THE CONTRIBUTIONS FOR THE DISCRETISED PART OF THE SPECTRUM
!
      DO JF = 1, NF
!
        AUX4=DFREQ(JF)
!
        DO IP=1,NPOIN2
          AUX3(IP) = 0.D0
        ENDDO
        DO IDIRE = 1, NDIRE
          DO IP=1, NPOIN2
            AUX3(IP) = AUX3(IP) + F(IP,IDIRE,JF)
          ENDDO
        ENDDO
!
        DO IP = 1,NPOIN2
          AUX1(IP)=AUX1(IP)+AUX3(IP)*AUX4
          AUX2(IP)=AUX2(IP)+AUX3(IP)/SQRT(XK(IP,JF))*AUX4
        ENDDO
!
      ENDDO
!
!     (OPTIONALLY) TAKES INTO ACCOUNT THE HIGH-FREQUENCY PART
!
      IF(TAILF.GT.1.D0) THEN
        CTE1=FREQ(NF)/(TAILF-1.D0)
        CTE2=COEFF/TAILF
        DO IP=1,NPOIN2
          AUX1(IP) = AUX1(IP) + AUX3(IP)*CTE1
          AUX2(IP) = AUX2(IP) + AUX3(IP)*CTE2
        ENDDO
      ENDIF
!
!     COMPUTES THE AVERAGE WAVE NUMBER
!
      DO IP=1,NPOIN2
        IF(AUX2(IP).LT.SEUIL) THEN
!         ARITHMETIC AVERAGE WHEN ENERGY WEIGHTED
!         AVERAGE IS NOT POSSIBLE
          XKMOY(IP)=XK(IP,1)
          DO JF=2,NF
            XKMOY(IP)=XKMOY(IP)+XK(IP,JF)
          ENDDO
          XKMOY(IP)=XKMOY(IP)/NF
        ELSE
          XKMOY(IP) = (AUX1(IP)/AUX2(IP))**2
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
