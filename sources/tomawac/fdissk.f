!                       *****************
                        SUBROUTINE FDISSK
!                       *****************
     &  (FDK,  NPOIN2, NDIRE,FS,ZTEL,NZ,HSMJT,FZNORM,NF)
! WAVE-ENHANCED MIXING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FS             |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FDK            |<--|
!| FZNORM               WORK ARRAY
!| HSMJT                WORK ARRAY
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NZ             |-->| NUMBER OF PLAN IN TELEMAC3D
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| ZTEL           |-->| ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT,DFREQ,DEPTH,BETABR
      USE INTERFACE_TOMAWAC, EX_FDISSK => FDISSK

      IMPLICIT NONE

!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)  :: NZ
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE, NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: HSMJT(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FDK(NPOIN2,NZ)
      DOUBLE PRECISION, INTENT(IN) :: ZTEL(NPOIN2,NZ)
      DOUBLE PRECISION, INTENT(INOUT) :: FZNORM(NPOIN2)
!     """""""""""""""""
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP,  INZ
      DOUBLE PRECISION DTETAR, AUX1,AUXZ
!
      IF (NDIRE.EQ.0) THEN
        WRITE(LU,*) 'FDISSK : NDIRE DIR eq 0 '
        CALL PLANTE(1)
      ENDIF
      DTETAR=DEUPI/DBLE(NDIRE)

      DO IP=1,NPOIN2
        FZNORM(IP) = 0.D0
        DO INZ=1,NZ
          FDK(IP,INZ) = 0.D0
        ENDDO
      ENDDO

      DO IP=1,NPOIN2
        DO INZ=1,NZ
          AUXZ=(ZTEL(IP,NZ)-ZTEL(IP,1))
!TYPE II
          IF (HSMJT(IP) .NE.0) THEN
            FZNORM(IP)=FZNORM(IP)+(1.D0-TANH(SQRT(2.D0)
     & /(1.2D0*HSMJT(IP))*((DEPTH(IP))-ZTEL(IP,INZ)))
     & **2.D0)*AUXZ
          ENDIF

!TYPE I
!           FZNORM(IP,NZ)=FZNORM(IP,NZ)+(COSH(SQRT(2.D0)
!      & /(1.2D0*HSMJT(IP))*(ZTEL(IP,INZ)-ZFJ(IP))))*AUXZ

        ENDDO
      ENDDO
!
      DO JP=1,NDIRE
        DO JF=1,NF
          AUX1=DFREQ(JF)*DTETAR
          DO IP=1,NPOIN2
            IF (FZNORM(IP).NE.0) THEN
              DO INZ=1,NZ
!   type II shape
                FDK(IP,INZ)=FDK(IP,INZ)+(0.03D0
     & *(1.D0-TANH(SQRT(2.D0)/(1.2D0*HSMJT(IP))
     & *((DEPTH(IP))-ZTEL(IP,INZ)))**2.D0)/FZNORM(IP)
     & *DEPTH(IP)*HSMJT(IP)/SQRT(2.D0)*(FS(IP,JP,JF)*ABS(BETABR(IP))
     & *GRAVIT)**(1./3.))*AUX1
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDDO

! !
! ! type I shape

!
!             FDK(IP,INZ)=FDK(IP,INZ)+(0.03D0
!      & *(COSH(SQRT(2.D0)/(1.2D0*HSMJT(IP))
!      & *(ZTEL(IP,INZ)-ZFJ(IP))))/(FZNORM(IP,NZ))*DEPTH1(IP)
!      & *HSMJT(IP)/SQRT(2.D0)*(FS(IP,JP,JF)*ABS(BETA(IP))
!      & *GRAVIT)**(1./3.))*AUX1

      RETURN
      END
