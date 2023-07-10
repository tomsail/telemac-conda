!                   *****************
                    SUBROUTINE WINDISS3
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

      USE DECLARATIONS_TOMAWAC, ONLY : FREQ, DFREQ, SINTET, COSTET,
     &   TETA, DEUPI, TWOLD, USOLD,   COEFWD, COEFWE, COEFWF, COEFWH
!
      USE INTERFACE_TOMAWAC, EX_WINDISS3 => WINDISS3
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FWX(NPOIN2), FWY(NPOIN2)
!
      DOUBLE PRECISION DTETAR,  AUX1, CPHAS, SIGMA
      DOUBLE PRECISION BETAWIN, DIREC, INTER, DIR
      INTEGER IP, JF, JP
      DTETAR=DEUPI/DBLE(NDIRE)
      DO IP=1,NPOIN2
        FWX(IP) = 0.D0
        FWY(IP) = 0.D0
      ENDDO

!.....LOOP ON THE DISCRETISED FREQUENCIES
!     """"""""""""""""""""""""""""""""""""""""""""
      DO JF=1,NF

        SIGMA=DEUPI*FREQ(JF)
        AUX1=DFREQ(JF)*DTETAR
!
!.......LOOP ON THE DISCRETISED DIRECTIONS
!       """"""""""""""""""""""""""""""""""""""""""""
        DO JP=1,NDIRE
          DIREC=TETA(JP)
          DO IP=1,NPOIN2
!.......COMPUTES THE FREQUENCIES (OMEGA AND UETOILE/CPHASE)
            CPHAS = SIGMA / XK(IP,JF)
            INTER=USOLD(IP)/CPHAS
            DIR=COS(DIREC-TWOLD(IP))
            BETAWIN = (COEFWD*INTER**2*DIR+COEFWE*INTER*DIR
     &      +COEFWF*DIR+COEFWH)*SIGMA

            FWX(IP)=FWX(IP)+((XK(IP,JF)/SIGMA)*SINTET(JP)
     &           *BETAWIN*FS(IP,JP,JF))*AUX1
            FWY(IP)=FWY(IP)+((XK(IP,JF)/SIGMA)*COSTET(JP)
     &           *BETAWIN*FS(IP,JP,JF))*AUX1
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END
