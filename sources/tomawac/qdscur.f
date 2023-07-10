!                       *****************
                        SUBROUTINE QDSCUR
!                       *****************
!
     &( TSTOT , TSDER , F     , CF    , XK    , USOLD , USNEW ,
     &  NF    , NDIRE , NPOIN2, F_INT , BETOTO, BETOTN)
!
!**********************************************************************
! TOMAWAC   V7P0                                 30/07/2014
!**********************************************************************
!
!brief   COMPUTES THE CONTRIBUTION OF THE WAVE BLOCKING SINK TERM USING
!+          THE  PARAMETRISATION OF VAN DER WESTHUYSEN (2012).
!
!reference    VAN DER WESTHUYSEN (2012):  SPECTRAL
!+              MODELLING OF WAVES DISSIPATION ON NEGATIVE
!+                   CURRENT GRADIENTS
!
!history  E. GAGNAIRE-RENOU (EDF/LNHE)
!+        09/2014
!+        V7P0
!+        NEW SUBROUTINE CREATED / IMPLEMENTED
!+
!history  WA BREUGEM. (IMDC)
!+        06/2022
!+        CHANGED LOOP ORDER TO PREVENT CACHE LOSSES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETOTN         |<->| WORK TABLE
!| BETOTO         |<->| WORK TABLE
!| F_INT          |<->| WORK TABLE
!| CF             |-->| ADVECTION FIELD ALONG FREQUENCY
!| F              |-->| DIRECTIONAL SPECTRUM
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |-->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USOLD          |<->| FRICTION VELOCITY AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  REMARKS:
!  ********
!
!  - THE CONSTANT CDSCUR (C"dis,break) UTILISED IN WESTHUYSEN (2012)
!                                    IS EQUAL TO 0.65 BY DEFAULT
!    CDSCUR       COEFFICIENT OF DISSIPATION BY STRONG CURRENT
!  - THE CONSTANT CMOUT4 (Br) UTILISED IN WESTHUYSEN (2007)
!                                    IS EQUAL TO 1.75*10^(-3)
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT,CMOUT4, CDSCUR,
     &                       CIMPLI, FREQ, DEPTH, PROINF,
     &                       TRA40,TRA41
! FROM TOMAWAC MODULE
! CIMPLI         IMPLICITATION COEFFICIENT FOR SOURCE TERM INTEG.
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_QDSCUR => QDSCUR
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: USNEW(NPOIN2),USOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: F_INT(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: BETOTO(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: BETOTN(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN2,NDIRE,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JP,IFF,IP
      DOUBLE PRECISION P0O,P0N,W,SURDEUPIFREQ,B,DTETAR
      DOUBLE PRECISION CPHAS,CG1,SQBSCMOUT4,DEUKD,KD,SURCMOUT4
      DOUBLE PRECISION CC
      
      DOUBLE PRECISION, POINTER :: AA(:), BB(:)
!
!-----------------------------------------------------------------------
!
      AA =>TRA40
      BB =>TRA41
      DTETAR=1.D0/DBLE(NDIRE)
      W=25.D0
      SURCMOUT4=1.D0/CMOUT4
!
!     LOOP ON THE DISCRETISED FREQUENCIES
!
      DO IFF=1,NF
!
        SURDEUPIFREQ=1.D0/(DEUPI*FREQ(IFF))
!
        DO IP=1,NPOIN2
          F_INT(IP)=F(IP,1,IFF)
        ENDDO
        DO JP=2,NDIRE
          DO IP=1,NPOIN2
            F_INT(IP)=F_INT(IP)+F(IP,JP,IFF)
          ENDDO
        ENDDO
        DO IP=1,NPOIN2
          F_INT(IP)=F_INT(IP)*DTETAR
        ENDDO
!
        IF(PROINF) THEN
!
          DO IP=1,NPOIN2
            CPHAS=XK(IP,IFF)*SURDEUPIFREQ
            P0O=3.D0+TANH(W*(USOLD(IP)*CPHAS-0.1D0))
            P0N=3.D0+TANH(W*(USNEW(IP)*CPHAS-0.1D0))
            CG1=0.5D0*GRAVIT*SURDEUPIFREQ
            B=CG1*F_INT(IP)*XK(IP,IFF)**3
            SQBSCMOUT4=SQRT(B*SURCMOUT4)
            AA(IP)=-CDSCUR*SQBSCMOUT4**(P0O/2)
            BB(IP)=-CDSCUR*SQBSCMOUT4**(P0N/2)
          ENDDO
          DO JP=1,NDIRE
            DO IP=1,NPOIN2
              CC=MAX(CF(IP,JP,IFF)/FREQ(IFF),0.D0)
              BETOTO(IP,JP)=AA(IP)*CC
              BETOTN(IP,JP)=BB(IP)*CC
            ENDDO
          ENDDO
!
        ELSE
!
          DO IP=1,NPOIN2
            CPHAS=XK(IP,IFF)*SURDEUPIFREQ
            P0O=3.D0+TANH(W*(USOLD(IP)*CPHAS-0.1D0))
            P0N=3.D0+TANH(W*(USNEW(IP)*CPHAS-0.1D0))
            KD=MIN(XK(IP,IFF)*DEPTH(IP),350.D0)
            DEUKD=KD+KD
            CG1=( 0.5D0+XK(IP,IFF)*DEPTH(IP)/SINH(DEUKD) )/CPHAS
            B=CG1*F_INT(IP)*XK(IP,IFF)**3
            SQBSCMOUT4=SQRT(B*SURCMOUT4)
            AA(IP)=-CDSCUR*SQBSCMOUT4**(P0O/2)
            BB(IP)=-CDSCUR*SQBSCMOUT4**(P0N/2)
          ENDDO
          !ABR
          DO JP=1,NDIRE
            DO IP=1,NPOIN2
              CC=MAX(CF(IP,JP,IFF)/FREQ(IFF),0.D0)
              BETOTO(IP,JP)=AA(IP)*CC
              BETOTN(IP,JP)=BB(IP)*CC
            ENDDO
          ENDDO
        ENDIF
!
!       TAKES THE SOURCE TERM INTO ACCOUNT
!
        DO JP=1,NDIRE
          DO IP=1,NPOIN2
            TSTOT(IP,JP,IFF)=TSTOT(IP,JP,IFF)
     &      +(BETOTO(IP,JP)+CIMPLI*(BETOTN(IP,JP)-BETOTO(IP,JP)))
     &      *F(IP,JP,IFF)
            TSDER(IP,JP,IFF)=TSDER(IP,JP,IFF)+BETOTN(IP,JP)
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
