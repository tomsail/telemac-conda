!                   *****************
                    SUBROUTINE QMOUT1
!                   *****************
!
     &( TSTOT , TSDER , F     , XK    , ENRJ  , FMOY  , XKMOY ,
     &  NF  , NDIRE , NPOIN2, TAUX1   )
!
!***********************************************************************
! TOMAWAC   V6P3                                   23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE WHITECAPPING
!+                SOURCE TERM BASED ON KOMEN ET AL. (1984).
!
!note     CMOUT1 (USED IN WAM-CYCLE 4) EQUALS 4.5.
!note   CMOUT2 (USED IN WAM-CYCLE 4) EQUALS 0.5.
!
!reference  KOMEN G.J., HASSELMANN S., HASSELMANN K. (1984) :
!+                     "ON THE EXISTENCE OF A FULLY DEVELOPED WINDSEA
!+                      SPECTRUM". JPO, VOL 14, PP 1271-1285.
!
!history  P. THELLIER; M. BENOIT (EDF/DER/LNH)
!+        06/04/95
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
!| ENRJ           |-->| SPECTRUM VARIANCE
!| F              |-->| DIRECTIONAL SPECTRUM
!| FMOY           |-->| MEAN SPECTRAL FRQUENCY FMOY
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| TAUX1          |<--| WORK TABLE
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKMOY          |-->| AVERAGE WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT, CMOUT1,CMOUT2,
     &                                 PROINF, FREQ, BETAWC

! Variables in TOMAWAC MODULE
! CMOUT1         WHITE CAPPING DISSIPATION COEFFICIENT
! CMOUT2         WHITE CAPPING WEIGHTING COEFFICIENT
!
      USE INTERFACE_TOMAWAC, EX_QMOUT1 => QMOUT1
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XKMOY(NPOIN2),ENRJ(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX1(NPOIN2),FMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION AUX   , C1    , C2
!
      C1 = - CMOUT1*DEUPI**9/GRAVIT**4
      C2 = - CMOUT1*DEUPI
!
      IF (PROINF) THEN
!
!       INFINITE WATER DEPTH (USES F).
!
!       WORKING ARRAY (THIS TERM ONLY DEPENDS ON THE POINT IN SPACE)
!
        DO IP=1,NPOIN2
          TAUX1(IP) = C1 * ENRJ(IP)**2 * FMOY(IP)**9
        ENDDO
!
!       LOOP OVER DISCRETISED FREQUENCIES
!
        DO JF=1,NF
!
!         COMPUTES THE BETA COEFFICIENT : QMOUT1 = BETA * F
!
          DO IP=1,NPOIN2
            AUX = (FREQ(JF)/FMOY(IP))**2
            BETAWC(IP)=TAUX1(IP)*AUX*(1.D0-CMOUT2+CMOUT2*AUX)
            DO JP=1,NDIRE
              TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+BETAWC(IP)*F(IP,JP,JF)
              TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+BETAWC(IP)
            ENDDO
          ENDDO
!
!         TAKES THE SOURCE TERM INTO ACCOUNT
!
!
        ENDDO
!
      ELSE
!
!       FINITE WATER DEPTH (USES K).
!
!       WORKING ARRAY (THIS TERM ONLY DEPENDS ON THE POINT IN SPACE)
!
        DO IP=1,NPOIN2
          TAUX1(IP) = C2 * ENRJ(IP)**2 * FMOY(IP) * XKMOY(IP)**4
        ENDDO
!
!       LOOP OVER THE DISCRETISED FREQUENCIES
!
        DO JF=1,NF
!
!         COMPUTES THE BETA COEFFICIENT : QMOUT1 = BETA * F
!
          DO IP=1,NPOIN2
            AUX = XK(IP,JF) / XKMOY(IP)
            BETAWC(IP)=TAUX1(IP)*AUX*(1.D0-CMOUT2+CMOUT2*AUX)
            DO JP=1,NDIRE
              TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+BETAWC(IP)*F(IP,JP,JF)
              TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+BETAWC(IP)
            ENDDO
          ENDDO
        ENDDO

!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
