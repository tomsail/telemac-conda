!                   *****************
                    SUBROUTINE WINDISS2
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
     &   TETA, DEUPI, ROAIR, ROEAU, USDPI,
     &   TWOLD, USOLD
      USE INTERFACE_TOMAWAC, EX_WINDISS2 => WINDISS2
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FWX(NPOIN2), FWY(NPOIN2)
!
      DOUBLE PRECISION DTETAR,  C1, CONST,  AUX1, COEPHAS
      DOUBLE PRECISION USO, SIGMA, BETAWIN, DIREC,SURDEUPIFREQ
      INTEGER IP, JF, JP
      DTETAR=DEUPI/DBLE(NDIRE)
      DO IP=1,NPOIN2
        FWX(IP) = 0.D0
        FWY(IP) = 0.D0
      ENDDO
      C1 = 0.25D0 * (ROAIR/ROEAU) * DEUPI

!.....LOOP ON THE DISCRETISED FREQUENCIES
!     """"""""""""""""""""""""""""""""""""""""""""
      DO JF=1,NF

        CONST=C1*FREQ(JF)
        SURDEUPIFREQ=USDPI/FREQ(JF)
        AUX1=DFREQ(JF)*DTETAR
        SIGMA=DEUPI*FREQ(JF)
!
!.......LOOP ON THE DISCRETISED DIRECTIONS
!       """"""""""""""""""""""""""""""""""""""""""""
        DO JP=1,NDIRE
          DIREC=TETA(JP)
          DO IP=1,NPOIN2
!.......COMPUTES THE FREQUENCIES (OMEGA AND UETOILE/CPHASE)
            USO=28.D0*USOLD(IP)*COS(DIREC-TWOLD(IP))
            COEPHAS = XK(IP,JF)*SURDEUPIFREQ
            BETAWIN = MAX(USO*COEPHAS-1.D0,0.D0)*CONST

            FWX(IP)=FWX(IP)+((XK(IP,JF)/SIGMA)*SINTET(JP)
     &           *BETAWIN*FS(IP,JP,JF))*AUX1
            FWY(IP)=FWY(IP)+((XK(IP,JF)/SIGMA)*COSTET(JP)
     &           *BETAWIN*FS(IP,JP,JF))*AUX1
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END
