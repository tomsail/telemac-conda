!                   *****************
                    SUBROUTINE INIPHY
!                   *****************
!
     &( XK    , CG    , B     , NPOIN2, NF   )
!
!***********************************************************************
! TOMAWAC   V6P1                                   20/06/2011
!***********************************************************************
!
!brief    COMPUTES THE WAVE PARAMETERS THAT ARE TIME-INDEPENDENT
!+               (WAVE NUMBER, GROUP VELOCITY,...).
!
!note     ALL THE DIRECTIONS ARE IN RADIAN AND IN THE RANGE [0 ; 2PI].
!
!history  M. BENOIT (EDF/DER/LNH)
!+        07/02/95
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
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| B              |<--| JACOBIAN TO TRANSFORM N(KX,KY) INTO F(FR,TETA)
!| CG             |<--| DISCRETIZED GROUP VELOCITY
!| COSPHI         |-->| COSINE OF THE LATITUDES OF THE POINTS 2D
!| DEPTH          |-->| WATER DEPTH
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION
!| XK             |<--| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT,R2, DEPTH, FREQ,
     &                                 COSF, PROINF, SPHE
!
      USE INTERFACE_TOMAWAC, EX_INIPHY => INIPHY
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)             :: NF    , NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: B(NPOIN2,NF)  , XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CG(NPOIN2,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER          IP    , JF
      DOUBLE PRECISION DEUPI2,DPDSUG,AUX2,AUX1,AUX3,DEUKD
!
      DEUPI2=DEUPI**2
      DPDSUG=DEUPI2/GRAVIT
!
      IF (PROINF) THEN
!                               +----------------------+
!.............................. ! INFINITE WATER DEPTH !
!                               +----------------------+
        DO JF=1,NF
          AUX1=DPDSUG*(FREQ(JF))**2
          AUX3=0.5D0*GRAVIT/(DEUPI*FREQ(JF))
          DO IP=1,NPOIN2
            XK(IP,JF)=AUX1
            CG(IP,JF)=AUX3
          ENDDO ! IP
        ENDDO ! JF
      ELSE
!                               +--------------------+
!.............................. ! FINITE WATER DEPTH !
!                               +--------------------+
        DO JF=1,NF
          AUX2=DEUPI*FREQ(JF)
          DO IP=1,NPOIN2
            CALL WNSCOU(AUX1,FREQ(JF),DEPTH(IP))
            DEUKD=2.D0*AUX1*DEPTH(IP)
            IF (DEUKD.GT.7.D2) THEN
              AUX3=0.5D0*AUX2/AUX1
            ELSE
              AUX3=0.5D0*(1.D0+DEUKD/SINH(DEUKD))*AUX2/AUX1
            ENDIF
            XK(IP,JF)=AUX1
            CG(IP,JF)=AUX3
          ENDDO ! IP
        ENDDO ! JF
      ENDIF
!
!
!.....COMPUTES B TO GO FROM (KX, KY) TO (FR, TETA)
!     ===================================================
      IF (.NOT.SPHE) THEN
!                               +-----------------------------+
!.............................. ! CARTESIAN COORDINATE SYSTEM !
!                               +-----------------------------+
        DO JF=1,NF
          AUX1=DEUPI2*FREQ(JF)
          DO IP=1,NPOIN2
            B(IP,JF)= CG(IP,JF)/(AUX1*XK(IP,JF))
          ENDDO ! IP
        ENDDO ! JF
!
      ELSE
!                               +-----------------------------+
!.............................. ! SPHERICAL COORDINATE SYSTEM !
!                               +-----------------------------+
        DO JF=1,NF
          AUX1=DEUPI2*FREQ(JF)*R2
          DO IP=1,NPOIN2
            B(IP,JF)= CG(IP,JF)/(AUX1*XK(IP,JF)*COSF(IP))
          ENDDO ! IP
        ENDDO ! JF
      ENDIF
!
      RETURN
      END
