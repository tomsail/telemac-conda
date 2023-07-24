!#######################################################################
                        SUBROUTINE FDISS3D
     &  (FDX, FDY, NPOIN2, XK, NDIRE, FS,NF)
!  SURFACE STRESS DUE TO DEPTH INDUCED WAVE BREAKING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FS             |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FDX            |<--| SURFACE STRESS ALONG X
!| FDY            |<--| SURFACE STRESS ALONG Y 
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY: FREQ, DFREQ, SINTET, COSTET, 
     &                                BETABR, DEUPI 
      USE INTERFACE_TOMAWAC, EX_FDISS3D => FDISS3D
    
      IMPLICIT NONE
      
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FDX(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FDY(NPOIN2)  

!     """""""""""""""""
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION SIGMA
      DOUBLE PRECISION DTETAR, AUX1
!
      DTETAR=DEUPI/DBLE(NDIRE)
      DO IP=1,NPOIN2
        FDX(IP) = 0.D0
        FDY(IP) = 0.D0
      ENDDO
      DO JF=1,NF
        SIGMA=DEUPI*FREQ(JF)
        AUX1=DFREQ(JF)*DTETAR        
        DO JP=1,NDIRE
          DO IP=1,NPOIN2
            FDX(IP)=FDX(IP)+((XK(IP,JF)/SIGMA)*SINTET(JP)
     & *BETABR(IP)*FS(IP,JP,JF))*AUX1
            FDY(IP)=FDY(IP)+((XK(IP,JF)/SIGMA)*COSTET(JP)
     & *BETABR(IP)*FS(IP,JP,JF))*AUX1
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END


