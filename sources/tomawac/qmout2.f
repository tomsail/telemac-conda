!                       *****************
                        SUBROUTINE QMOUT2
!                       *****************
!
     &( TSTOT, TSDER, F    , XK    , ENRJ , FMOY , XKMOY , USOLD,  
     &  USNEW, NF   , NDIRE, NPOIN2, TAUX1, F_INT, BETOTO, BETOTN)
!
!**********************************************************************
! TOMAWAC   V6P3                                  23/06/2011
!**********************************************************************
!
!brief   COMPUTES THE CONTRIBUTION OF THE WHITECAPPING SINK TERM USING
!+          THE  PARAMETRISATION of VAN DER WESTHUYSEN (2007).
!
!reference    VAN DER WESTHUYSEN (2007): ADVANCES IN THE SPECTRAL
!+              MODELLING OF WIND WAVES IN THE NEARSHORE, PHD THESID,
!+              DELFT UNIVERSITY OF TECHNOLOGY
!
!history  E. GAGNAIRE-RENOU (EDF/LNHE)
!+        09/2010
!+        V6P0
!+
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF/LNHE)
!+        23/12/2012
!+        V6P3
!+   A first optimisation + limitation of SINH argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETA           |<->| WORK TABLE
!| BETOTN         |<->| WORK TABLE
!| BETOTO         |<->| WORK TABLE
!| ENRJ           |-->| SPECTRUM VARIANCE
!| F              |-->| DIRECTIONAL SPECTRUM
!| FMOY           |-->| MEAN SPECTRAL FRQUENCY FMOY
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKMOY          |-->| AVERAGE WAVE NUMBER
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TAUX1          |<->| WORK TABLE
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |-->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USOLD          |<->| FRICTION VELOCITY AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!  APPELS :    - PROGRAMME(S) APPELANT  : SEMIMP
!  ********    - PROGRAMME(S) APPELE(S) :    -
!
!  REMARKS:
!  ********
!
!  - THE CONSTANT CMOUT3 (Cdis,break) UTILISED IN WESTHUYSEN (2007)
!                                    IS EQUAL TO 5.0*10^(-5)
!  - THE CONSTANT CMOUT4 (Br) UTILISED IN WESTHUYSEN (2007)
!                                    IS EQUAL TO 1.75*10^(-3)
!  - THE CONSTANT CMOUT5 (Cdis,non-break) UTILISED IN WESTHUYSEN
!                                    (2007) IS EQUAL TO 3.29
!  - THE CONSTANT CMOUT6 (Delta) UTILISED IN WESTHUYSEN (2007)
!                                    IS EQUAL TO 0
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT, CMOUT3,CMOUT4, 
     &           CMOUT5, CMOUT6, CIMPLI, PROINF, DEPTH, FREQ, BETAWC
! Variables in TOMAWAC MODULE
! CMOUT3         WESTHUYSEN WHITE CAPPING DISSIPATION COEFFICIENT
! CMOUT4         WESTHUYSEN SATURATION THRES. FOR THE DISSIPATION
! CMOUT5         WESTHUYSEN WHITE CAPPING DISSIPATION COEFFICIENT
! CMOUT6         WESTHUYSEN WHITE CAPPING WEIGHTING COEFFICIENT
! CIMPLI         IMPLICITATION COEFFICIENT FOR SOURCE TERM INTEG.
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_QMOUT2 => QMOUT2
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: USNEW(NPOIN2),USOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: FMOY(NPOIN2),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: ENRJ(NPOIN2),XKMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F_INT(NPOIN2),TAUX1(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: BETOTO(NPOIN2),BETOTN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JP,IFF,IP
      DOUBLE PRECISION PO,AUX,C1,C2,C3,P0O,P0N,W,SURDEUPIFREQ,B,DTETAR
      DOUBLE PRECISION BETAO,BETAN,CPHAS,CG1,SQBSCMOUT4,BETA,DEUKD,KD
      DOUBLE PRECISION SURCMOUT4
!
!-----------------------------------------------------------------------
!
!     DTETAR = DEUPI/DBLE(NDIRE)
!     F_INT WAS DIVIDED BY DEUPI AFTER IN FORMULAS, DIVISION REMOVED
      DTETAR = 1.D0/DBLE(NDIRE)
      C1     = - CMOUT5*DEUPI**9/GRAVIT**4
      C2     = - CMOUT5*DEUPI
      W = 25.D0
      SURCMOUT4 = 1.D0/CMOUT4
!
      IF(PROINF) THEN
!       DEEP WATER CASE, ARRAY DEPENDING ONLY ON THE SPATIAL MESH NODE
        DO IP = 1,NPOIN2
          TAUX1(IP) = C1 * ENRJ(IP)**2 * FMOY(IP)**9
        ENDDO
      ELSE
!       FINITE DEPTH CASE
        DO IP=1,NPOIN2
          TAUX1(IP) = C2 * ENRJ(IP)**2 * FMOY(IP) * XKMOY(IP)**4
        ENDDO
      ENDIF
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
          DO IP = 1,NPOIN2
!
            CPHAS = XK(IP,IFF)*SURDEUPIFREQ
            P0O=3.D0+TANH(W*(USOLD(IP)*CPHAS-0.1D0))
            P0N=3.D0+TANH(W*(USNEW(IP)*CPHAS-0.1D0))
            CG1 = 0.5D0*GRAVIT*SURDEUPIFREQ
            B   = CG1*F_INT(IP)*XK(IP,IFF)**3
            SQBSCMOUT4=SQRT(B*SURCMOUT4)
!           COMPUTES THE BREAK/NON-BREAK TRANSITION
            PO = 0.5D0*(1.D0+TANH(10.D0*(SQBSCMOUT4-1.D0)))
!           COMPUTES THE BREAK BETA
            C3=-CMOUT3*SQRT(GRAVIT*XK(IP,IFF))
            BETAO=C3*SQBSCMOUT4**P0O
            BETAN=C3*SQBSCMOUT4**P0N
!           COMPUTES THE NON-BREAK BETA
            AUX = (FREQ(IFF)/FMOY(IP))**2
            BETA=TAUX1(IP)*AUX*(1.D0-CMOUT6+CMOUT6*AUX)
!           COMPUTES THE TOTAL BETA
            BETOTO(IP)=BETA+PO*(BETAO-BETA)
            BETOTN(IP)=BETA+PO*(BETAN-BETA)
!
          ENDDO
!
        ELSE
!
          DO IP = 1,NPOIN2
!
            CPHAS = XK(IP,IFF)*SURDEUPIFREQ
            KD=MIN(XK(IP,IFF)*DEPTH(IP),350.D0)
            DEUKD=KD+KD
            CG1=( 0.5D0+XK(IP,IFF)*DEPTH(IP)/SINH(DEUKD) )/CPHAS
            B = CG1*F_INT(IP)*XK(IP,IFF)**3
            SQBSCMOUT4=SQRT(B*SURCMOUT4)
!           COMPUTES THE BREAK BETA
            C3=-CMOUT3*SQRT(GRAVIT*XK(IP,IFF))
            AUX=TANH(KD)
            P0O=3.D0+TANH(W*(USOLD(IP)*CPHAS-0.1D0))
            P0N=3.D0+TANH(W*(USNEW(IP)*CPHAS-0.1D0))
            BETAO=C3*SQBSCMOUT4**P0O*AUX**((2.D0-P0O)*0.25D0)
            BETAN=C3*SQBSCMOUT4**P0N*AUX**((2.D0-P0N)*0.25D0)
!           COMPUTES THE NON-BREAK BETA
            AUX = XK(IP,IFF) / XKMOY(IP)
!           COMPUTES THE TOTAL BETA
            BETA=TAUX1(IP)*AUX*(1.D0-CMOUT6+CMOUT6*AUX)
!           COMPUTES THE BREAK/NON-BREAK TRANSITION
            PO = 0.5D0*(1.D0+TANH(10.D0*(SQBSCMOUT4-1.D0)))
            BETOTO(IP)=BETA+PO*(BETAO-BETA)
            BETOTN(IP)=BETA+PO*(BETAN-BETA)
!
          ENDDO
!
        ENDIF

!     CALCULATE WHITE CAPPING RATIO
        DO IP = 1,NPOIN2
          BETAWC(IP)=+BETOTO(IP)+CIMPLI*(BETOTN(IP)-BETOTO(IP))
        ENDDO
!
!       TAKES THE SOURCE TERM INTO ACCOUNT
!
        DO JP = 1,NDIRE
          DO IP = 1,NPOIN2
            TSTOT(IP,JP,IFF)=TSTOT(IP,JP,IFF) + BETAWC(IP)*F(IP,JP,IFF)
            TSDER(IP,JP,IFF)=TSDER(IP,JP,IFF) + BETOTN(IP)
          ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
