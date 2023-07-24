!#######################################################################
                        SUBROUTINE FBOTT3D
     &  (FBX, FBY, FS, NPOIN2, XK, NDIRE, NF)
!
!     WAVE DISSIPATION DUE TO BOTTOM FRICTION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FBX            |<--| WAVE DISSIPATION ALONG X
!| FBY            |<--| WAVE DISSIPATION ALONG Y 
!| FS             |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, CFROT1, GRAVIT, COSTET,
     &  DFREQ, FREQ, SINTET, DEPTH
      USE INTERFACE_TOMAWAC, EX_FBOTT3D => FBOTT3D
    
      IMPLICIT NONE
      
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    :: NPOIN2, NDIRE,NF
      DOUBLE PRECISION, INTENT(IN)    :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT)    :: FBX(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT)    :: FBY(NPOIN2)  

!     """""""""""""""""
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION SIGMA,BETAMJ
      DOUBLE PRECISION DTETAR, AUX1

      DOUBLE PRECISION COEF , DEUKD
!
      DTETAR=DEUPI/DBLE(NDIRE)
      DO IP=1,NPOIN2
        FBX(IP) = 0.D0
        FBY(IP) = 0.D0
      ENDDO
      COEF=-2.D0*CFROT1/GRAVIT
!
      DO JF=1,NF
        SIGMA=DEUPI*FREQ(JF)
        AUX1=DFREQ(JF)*DTETAR        
        DO JP=1,NDIRE
          DO IP=1,NPOIN2
            DEUKD = MIN(2.D0*DEPTH(IP)*XK(IP,JF),7.D2)
            BETAMJ = COEF*XK(IP,JF)/SINH(DEUKD)
            FBX(IP)=FBX(IP)+((XK(IP,JF)/SIGMA)*SINTET(JP)
     &            *BETAMJ*FS(IP,JP,JF))*AUX1
            FBY(IP)=FBY(IP)+((XK(IP,JF)/SIGMA)*COSTET(JP)
     &            *BETAMJ*FS(IP,JP,JF))*AUX1
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END
