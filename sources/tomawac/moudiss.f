!                   *****************
                    SUBROUTINE MOUDISS
!                   *****************
     &  (FWX, FWY, NPOIN2, XK, NDIRE, FS,NF)
!  SURFACE STRESS DUE TO WIND INPUT ENERGY AND WHITECAPPING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FS             |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FWX            |<--| SURFACE STRESS DUE TO WIND ALONG X
!| FWY            |<--| SURFACE STRESS DUE TO WIND ALONG Y
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                    *****************
      USE DECLARATIONS_TOMAWAC, ONLY : FREQ, DFREQ, SINTET, COSTET,
     &DEUPI, BETAWC
      USE INTERFACE_TOMAWAC, EX_MOUDISS => MOUDISS
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FWX(NPOIN2), FWY(NPOIN2)
!
      DOUBLE PRECISION DTETAR, SIGMA, AUX1
      INTEGER IP, JF, JP
      DTETAR=DEUPI/DBLE(NDIRE)

      DO JF=1,NF
        SIGMA=DEUPI*FREQ(JF)
        AUX1=DFREQ(JF)*DTETAR
        DO IP=1,NPOIN2
          DO JP=1,NDIRE
            FWX(IP)=FWX(IP)+((XK(IP,JF)/SIGMA)*SINTET(JP)
     &             *BETAWC(IP)*FS(IP,JP,JF))*AUX1
            FWY(IP)=FWY(IP)+((XK(IP,JF)/SIGMA)*COSTET(JP)
     &             *BETAWC(IP)*FS(IP,JP,JF))*AUX1
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END
