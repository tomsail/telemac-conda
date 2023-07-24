!#######################################################################
      SUBROUTINE WIPJ (  WIP, FS, NPOIN2, XK, WIPDX, WIPDY,NDIRE,NF)
!     Calculating the wave induced pressure and gradients
!     WIP = Wave Induced Pressure      
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FS             |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| WIP            |<--| WAVE INDUCED PRESSURE 
!| WIPDX          |<--| DERIVATIVE OF WIP ALONG X 
!| WIPDY          |<--| DERIVATIVE OF WIP ALONG Y 
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : ST1,ST3,ST4, MESH, GRAVIT, DFREQ
     &     ,IELM2,T1,T3,T4, DEPTH, DEUPI
      USE INTERFACE_TOMAWAC, EX_WIPJ => WIPJ

      IMPLICIT NONE
      
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""

      INTEGER, INTENT(IN)    :: NPOIN2,NDIRE,NF
      DOUBLE PRECISION, INTENT(IN)    :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: WIP(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: WIPDX(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: WIPDY(NPOIN2)
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION DTETAR, COEF, C
      
      DTETAR=DEUPI/DBLE(NDIRE)
      DO IP=1,NPOIN2
        WIP(IP) = 0.D0
      ENDDO
         
      DO JF=1,NF
        COEF=GRAVIT*DFREQ(JF)*DTETAR
        DO JP=1,NDIRE  
          DO IP=1,NPOIN2
            WIP(IP)=WIP(IP)+((XK(IP,JF)*FS(IP,JP,JF))
     &  /(SINH(2.D0*(XK(IP,JF)*DEPTH(IP)))))*COEF
          ENDDO
        ENDDO
      ENDDO

!.....DERIVATIVE IN X
      CALL OV('X=Y     ',T4,WIP,T3,C,NPOIN2)
    
      CALL VECTOR
     & (ST1,'=','GRADF          X',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,ST3,ASSPAR=.TRUE.)
            
      CALL VECTOR
     & (ST3,'=','GRADF          X',IELM2,1.D0,MESH%X,
     &  ST1,ST1,ST1,ST1,ST1,MESH,.FALSE.,ST1,ASSPAR=.TRUE.)

!      IF(NCSIZE.GE.1) THEN
!        CALL PARCOM(ST1,2,MESH)
!        CALL PARCOM(ST3,2,MESH)
!      ENDIF
      CALL OV('X=Y/Z   ',WIPDX,T1,T3,C,NPOIN2)
     
!.....DERIVATIVE IN Y
      CALL OV('X=Y     ',T4,WIP,T3,C,NPOIN2)
      CALL VECTOR
     & (ST1,'=','GRADF          Y',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,ST3,ASSPAR=.TRUE.)
!
      CALL VECTOR
     & (ST3,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     &  ST1,ST1,ST1,ST1,ST1,MESH,.FALSE.,ST1,ASSPAR=.TRUE.)

!AQUI FAZEMOS A MÉDIA dos grads no volume.

      CALL OV('X=Y/Z   ',WIPDY,T1,T3,C,NPOIN2)
      
!AQUI TEMOS AS DERIVADAS EM X E Y DO WAVE INDUCED PRESSURE

      RETURN
      END

