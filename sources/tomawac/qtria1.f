!                   *****************
                    SUBROUTINE QTRIA1
!                   *****************
!
     &( F     , XK    , NF    , NDIRE , NPOIN2, TSTOT , FTOT  , FMOY  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   27/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE NON-LINEAR
!+                INTERACTIONS SOURCE TERM (FREQUENCY TRIADS).
!
!history  EDF/DER/LNH
!+        26/12/96
!+        V1P1
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
!+        27/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALFLTA         |-->| COEFFICIENT ALPHA OF LTA TRIAD INTERACTION MODEL
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| DIRECTIONAL SPECTRUM
!| FMOY           |-->| MEAN FREQUENCIES F-10
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| FTOT           |-->| SPECTRUM VARIANCE
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| RFMLTA         |-->| COEFFICIENT OF LTA TRIAD INTERACTION MODEL
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : GRAVIT,DEUPI, ALFLTA, RFMLTA
     &                , RAISF, FREQ, DEPTH
!
      USE INTERFACE_TOMAWAC, EX_QTRIA1 => QTRIA1
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    ::  NF, NDIRE, NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: FTOT(NPOIN2) , FMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
!
!.....VARIABLES FROM MODULE TOMAWAC
!     """""""""""""""""""""""""""""
!| ALFLTA         |-->| COEFFICIENT ALPHA OF LTA TRIAD INTERACTION MODEL
!| RFMLTA         |-->| COEFFICIENT OF LTA TRIAD INTERACTION MODEL
!| RAISF          |-->| FREQUENTIAL RATIO
!
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER           IIND  , IFMA  , IFF   , IPL   , IPO
      DOUBLE PRECISION  CPH   , CGR   , BIF   , RPS2  , RP    ,
     &                  FPS2  , CPHS2 , XKPS2 , URS   , RIND  , FMAX  ,
     &                  F2P   , CPH2P , XK2P  , CGR2P , E2P   , EPS2  ,
     &                  OMP   , OM2P  , COEF  , D     , DEUKD ,
     &                  SPLUS , SMOIN ,FP    , XKP
!
!.....FUNCTION / FORMULATION
!     """"""""""""""""
!
      COEF = GRAVIT*SQRT(2.D0)
!
      DO IPO=1,NPOIN2
!
        D=DEPTH(IPO)
!
!.......COMPUTES THE URSELL NUMBER AT THE CONSIDERED POINT IN SPACE
!       """""""""""""""""""""""""""""""""""""""""""""""""""""
        URS = COEF*SQRT(FTOT(IPO))/(DEUPI*D*FMOY(IPO))**2
!
!.......COMPUTES THE CONTRIBUTION OF TRIADS ONLY IF URSELL > 0.1
!       """""""""""""""""""""""""""""""""""""""""
        IF (URS.GT.0.1D0) THEN
!
!.........COMPUTES THE SINE OF THE BIPHASE
!         """""""""""""""""""""""""""""
          IF (URS.GT.10.D0) THEN
            BIF=1.D0
          ELSE
            BIF=SIN(ABS(DEUPI/4.D0*(-1.D0+TANH(0.2D0/URS))))
          ENDIF
!
!.........COMPUTES THE MAXIMUM FREQUENTIAL INDEX
!         """"""""""""""""""""""""""""""""""""""
          FMAX = RFMLTA*MAX(FMOY(IPO),FREQ(1))
          RIND = 1.D0 + LOG(FMAX/FREQ(1))/LOG(RAISF)
          IFMA = MIN(INT(RIND),NF)
!
          DO IFF=1,IFMA
            FP  = FREQ(IFF)
            OMP = DEUPI*FP
            XKP = XK(IPO,IFF)
            CPH = OMP/XKP
!
!
!...........COMPUTES THE CONTIBUTION S+
!           """""""""""""""""""""""""""
            FPS2 = FP/2.D0
            RIND = 1.D0 + LOG(FPS2/FREQ(1))/LOG(RAISF)
            IIND = INT(RIND)
            RIND = RIND-DBLE(IIND)
!
            IF (IIND.GT.0) THEN
              DEUKD=2.D0*XKP*D
              IF(DEUKD.LE.7.D2) THEN
                CGR = CPH*(0.5D0+XKP*D/SINH(2.D0*XKP*D))
              ELSE
                CGR = 0.5D0*CPH
              ENDIF
              CALL WNSCOU(XKPS2,FPS2,D)
              CPHS2 = DEUPI*FPS2/XKPS2
!>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
              RPS2 = CPH*CGR*(
     &               XKPS2**2*(GRAVIT*D+2.D0*CPHS2**2)/(XKP*D)/
     &   (GRAVIT*D+(2.D0/15.D0)*GRAVIT*D**3*XKP**2-0.4D0*(OMP*D)**2)
     &               )**2
!              RPS2 = CPH*CGR*RPP(XKPS2,CPHS2,XKP,OMP,D)**2
!<JR @ ADJOINTWARE
!
              DO IPL=1,NDIRE
                EPS2=(1.D0-RIND)*F(IPO,IPL,IIND)+RIND*F(IPO,IPL,IIND+1)
                SPLUS = ALFLTA*RPS2*BIF*(EPS2-2.D0*F(IPO,IPL,IFF))*EPS2
                IF(SPLUS.LT.0.D0) SPLUS = 0.D0
                TSTOT(IPO,IPL,IFF) = TSTOT(IPO,IPL,IFF) + SPLUS
              ENDDO
            ENDIF
!
!
!...........COMPUTES THE CONTIBUTION S-
!           """""""""""""""""""""""""""
            F2P = 2.D0*FP
            RIND = 1.D0 + LOG(F2P/FREQ(1))/LOG(RAISF)
            IIND = INT(RIND)
            RIND = RIND-DBLE(IIND)
            IF (IIND.LT.IFMA) THEN
              OM2P  = DEUPI*F2P
              CALL WNSCOU(XK2P,F2P,D)
              CPH2P = OM2P/XK2P
              DEUKD=2.D0*XK2P*D
              IF(DEUKD.LE.700D2) THEN
                CGR2P = CPH2P*(0.5D0+XK2P*D/SINH(2.D0*XK2P*D))
              ELSE
                CGR2P = CPH2P*0.5D0
              ENDIF
!>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
              RP    = CPH2P*CGR2P*(
     &                XKP**2*(GRAVIT*D+2.D0*CPH**2)/(XK2P*D)/
     &   (GRAVIT*D+(2.D0/15.D0)*GRAVIT*D**3*XK2P**2-0.4D0*(OM2P*D)**2)
     &                )**2
!              RP    = CPH2P*CGR2P*RPP(XKP,CPH,XK2P,OM2P,D)**2
!<JR @ ADJOINTWARE
!
              DO IPL=1,NDIRE
                E2P = (1.D0-RIND)*F(IPO,IPL,IIND)+RIND*F(IPO,IPL,IIND+1)
                SMOIN = 2.D0*ALFLTA*RP*BIF*F(IPO,IPL,IFF)
     &                *(F(IPO,IPL,IFF)-2.D0*E2P)
                IF(SMOIN.LT.0.D0) SMOIN = 0.D0
                TSTOT(IPO,IPL,IFF) = TSTOT(IPO,IPL,IFF) - SMOIN
              ENDDO
            ENDIF
!
          ENDDO
        ENDIF
      ENDDO
!
      RETURN
      END
