!                   *****************
                    SUBROUTINE WINDISS1
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
     &   TETA, BETAM, DECAL, XKAPPA, DEUPI, GRAVIT, ROAIR, ROEAU,
     &   TWOLD, USOLD, Z0OLD
!
      USE INTERFACE_TOMAWAC, EX_WINDISS1 => WINDISS1
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FWX(NPOIN2), FWY(NPOIN2)
!
      DOUBLE PRECISION DTETAR, ZLOGMU, C1, CONST, SIGMA, AUX1, CPHAS
      DOUBLE PRECISION XX, USO, OMEGA, BETAWIN
      INTEGER IP, JF, JP
      DTETAR=DEUPI/DBLE(NDIRE)
      DO IP=1,NPOIN2
        FWX(IP) = 0.D0
        FWY(IP) = 0.D0
      ENDDO
      !
      C1 = DEUPI * (ROAIR/ROEAU) * (BETAM/XKAPPA**2)
!.....LOOP ON THE DISCRETISED FREQUENCIES
!     """"""""""""""""""""""""""""""""""""""""""""
      DO JF=1,NF

        CONST=C1*FREQ(JF)
        SIGMA=DEUPI*FREQ(JF)
        AUX1=DFREQ(JF)*DTETAR
!
!.......LOOP ON THE DISCRETISED DIRECTIONS
!       """"""""""""""""""""""""""""""""""""""""""""
        DO JP=1,NDIRE
          DO IP=1,NPOIN2
!.......COMPUTES THE FREQUENCIES (OMEGA AND UETOILE/CPHASE)
            CPHAS = DEUPI * FREQ(JF) / XK(IP,JF)
            OMEGA = GRAVIT * Z0OLD(IP) / CPHAS**2
            USO  = USOLD(IP) / CPHAS + DECAL
            XX = USO *  COS(TETA(JP)-TWOLD(IP))
            ZLOGMU = LOG(OMEGA) + XKAPPA/XX
            IF(ZLOGMU.LT.0.D0) THEN
              BETAWIN = CONST*OMEGA*EXP(XKAPPA/XX)*
     &             ZLOGMU**4*XX**2
              FWX(IP)=FWX(IP)+((XK(IP,JF)/SIGMA)*SINTET(JP)
     &             *BETAWIN*FS(IP,JP,JF))*AUX1
              FWY(IP)=FWY(IP)+((XK(IP,JF)/SIGMA)*COSTET(JP)
     &             *BETAWIN*FS(IP,JP,JF))*AUX1
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END
