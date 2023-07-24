!#######################################################################
                        SUBROUTINE UVSTOKES
     &  (UST, VST, WST, FS, NPOIN2, XK, ZFJ, NDIRE, ZTEL, NZ, NF)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  calculation of the three components of the stokes drift
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FS             |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NZ             |-->| NUMBER OF PLAN IN TELEMAC3D
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| UST            |<--| STOKES COMPONENT ALONG X
!| VST            |<--| STOKES COMPONENT ALONG Y
!| WST            |<--| STOKES COMPONENT ALONG Z
!| ZFJ            |-->| BOTTOM ELEVATION
!| ZTEL           |-->| ELEVATION IN TELEMAC3D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : FREQ,COSTET,DFREQ,DZX,DZY,SINTET,
     &                                 DEPTH, DEUPI
      USE INTERFACE_TOMAWAC, EX_UVSTOKES => UVSTOKES
      
!NDIRE - number of direction discretization      
      IMPLICIT NONE
      
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)  :: NZ,NF
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: ZFJ(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: UST(NPOIN2,NZ)
      DOUBLE PRECISION, INTENT(INOUT) :: VST(NPOIN2,NZ)
      DOUBLE PRECISION, INTENT(INOUT) :: WST(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: ZTEL(NPOIN2,NZ)
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP, INZ
      DOUBLE PRECISION SIGMA, DTETAR, AUX1
!
      DTETAR=DEUPI/DBLE(NDIRE)

!begin mjt- initialize variables      
      DO IP=1,NPOIN2
        DO INZ=1,NZ
          UST(IP,INZ) = 0.D0
          VST(IP,INZ) = 0.D0
        ENDDO      
      ENDDO
      DO JP=1,NDIRE
        DO JF=1,NF
          SIGMA=DEUPI*FREQ(JF)
          AUX1=DFREQ(JF)*DTETAR
          DO IP=1,NPOIN2
            DO INZ=1, NZ
              UST(IP,INZ)=UST(IP,INZ)+(SIGMA*XK(IP,JF)*SINTET(JP)
     & *FS(IP,JP,JF)*(COSH(2.D0*XK(IP,JF)*ZTEL(IP,INZ)
     & +2.D0*XK(IP,JF)*(-ZFJ(IP)))
     & /SINH(XK(IP,JF)*DEPTH(IP))**2.D0))*AUX1

              VST(IP,INZ)=VST(IP,INZ)+(SIGMA*XK(IP,JF)
     & *COSTET(JP)*FS(IP,JP,JF)*(COSH(2.D0*XK(IP,JF)*ZTEL(IP,INZ)
     & +2.D0*XK(IP,JF)*(-ZFJ(IP))) 
     & /SINH(XK(IP,JF)*DEPTH(IP))**2.D0))*AUX1
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      DO IP=1,NPOIN2
        WST(IP)=-UST(IP,1)*DZX(IP)-VST(IP,1)*DZY(IP)
      ENDDO
      
      RETURN
      END
