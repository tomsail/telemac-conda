!                   *****************
                    SUBROUTINE QTRIA2
!                   *****************
!
     &( F     , XK    ,  NF    , NDIRE , NPOIN2, TSTOT )
!
!***********************************************************************
! TOMAWAC   V6P1                                   27/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE NON-LINEAR
!+                INTERACTIONS SOURCE TERM (FREQUENCY TRIADS).
!+<BR>           (INSPIRED FROM THE BOUSSINESQ EQUATIONS)
!
!history  EDF/DER/LNH
!+        11/06/98
!+        V5P0
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
!| F              |-->| DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT, FREQ  , DFREQ ,
     &                          DEPTH , TETA  , SINTET, COSTET ,  RAISF,
     &                          QINDI,BDISPB,BDSSPB, KSPB, NBD
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_QTRIA2 => QTRIA2
      IMPLICIT NONE
!
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    ::  NF, NDIRE, NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
!.....VARIABLES FROM MODULE TOMAWAC
!     """""""""""""""""""""""""""""
! QINDI           CONFIGURATION INDEX
! NBD             NUMBER OF TRIAD CONFIGURATIONS
! BDISPB          LOWER DIRECTIONAL BOUND. OF SPB TRIAD MODEL
! BDSSPB          UPPER DIRECTIONAL BOUND. OF SPB TRIAD MODEL
! KSPB            COEFFICIENT K OF SPB TRIAD INTERACTION MODEL
! RAISF           FREQUENTIAL RATIO
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IFF, JFF, IPL, JPL , IPO
      INTEGER  IFR, IPP, IPM
      DOUBLE PRECISION  DTETA, FR1 , AP2  , XK1   , XK3 , DEP
      DOUBLE PRECISION  TETA2, XK2 , K2NL , NRJ2
      DOUBLE PRECISION  FREQ0, FREQ1, FREQ2, FREQ3, LRAISF, RAISM1
      DOUBLE PRECISION  VR1 , VR2 , VR3 , TK1 , TK2 , TK3
      DOUBLE PRECISION  BK1 , BK3 , DEP2
      DOUBLE PRECISION  FILT , BISP, DEUPI2
      DOUBLE PRECISION  VAR1 , XC1 , XC2 , XC3
      DOUBLE PRECISION  BMS , BMSP
!
      INTEGER IP1, IP3
!
!
!.....EXTERNAL FUNCTIONS
!     """""""""""""""""""
!      DOUBLE PRECISION KERBOU
!      EXTERNAL         KERBOU
!
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
!
      DTETA = TETA(2)-TETA(1)
      BMS  = 1.D0/15.D0
      BMSP = BMS + 1.D0/3.D0
      DEUPI2 = DEUPI**2
      FREQ0  = FREQ(1)
      LRAISF = LOG(RAISF)
      RAISM1 = RAISF-1.D0
!
      DO IFF = 1,NF
        FREQ3 = FREQ(IFF)
        DO JFF = 1,IFF-1
          FREQ1 = FREQ(JFF)
          FREQ2 = FREQ3-FREQ1
          IF(FREQ2.LE.FREQ0) THEN
            CYCLE
          ENDIF
          FR1 = 1.D0 + LOG(FREQ2/FREQ0)/LRAISF
          IFR = INT(FR1)
          FR1 = FR1 - DBLE(IFR)
          FR1 = (RAISF**FR1-1.D0)/RAISM1
          DO IP3 = 1,NBD
            IPL = QINDI(IP3)
            DO IP1 = 1,NBD
              JPL = QINDI(IP1)
              DO IPO = 1,NPOIN2
!               COMPUTES K2
!               --------------------
                DEP = DEPTH(IPO)
                XK1 = XK(IPO,JFF)
                XK3 = XK(IPO,IFF)
                K2NL   = SQRT((XK3*COSTET(IPL)-XK1*COSTET(JPL))**2
     &                   +(XK3*SINTET(IPL)-XK1*SINTET(JPL))**2)
                XK2    = (1.D0-FR1)*XK(IPO,IFR) + FR1*XK(IPO,IFR+1)
!
                TETA2=ATAN2(XK3*SINTET(IPL)-XK1*SINTET(JPL)
     &                      ,XK3*COSTET(IPL)-XK1*COSTET(JPL))
                IF(TETA2.LT.0.D0) TETA2 = DEUPI + TETA2
!
                IF(TETA2.LT.BDISPB .OR. TETA2.GT.BDSSPB) THEN
!               INTERACTIONS BETWEEN COMPONENTS WHICH DIRECTIONS ARE NOT
!               WITHIN THE ANGULAR SECTOR DEFINED BY THE USER (VARIABLES
!               BDISPB AND BDSSPB) ARE NOT TAKEN INTO ACCOUNT
                  CYCLE
                ENDIF
!
                AP2    = (TETA2-TETA(1))/DTETA
                IPM    = NINT(AP2)
                IPP    = IPM + 1
                IF(IPM.EQ.0) IPM=NDIRE
                IF(IPP.EQ.NDIRE+1) IPP = 1
!
!.........COMPUTES COUPLING COEFFICIENTS
!               """"""""""""""""""""""""""""""""""""
!               R(P-M,M)
                VR1 = KERBOU(XK1,XK2,FREQ1,FREQ2,DEP,TETA(JPL),TETA2)
!               R(M-P,P)
                VR2 = KERBOU(-XK1,XK3,-FREQ1,FREQ3,DEP,TETA(JPL),
     &                       TETA(IPL))
!               R(-M,P)
                VR3 = KERBOU(-XK2,XK3,-FREQ2,FREQ3,DEP,TETA2,TETA(IPL))
!
                FILT = KSPB/((XK2-K2NL)**2+KSPB*KSPB)
                FILT = -0.5D0*FILT/XK2
!
                DEP2 = DEP**2
                VAR1 = 2.D0*BMS*DEP2
                XC1  = VAR1*XK3*XK3
                XC2  = VAR1*XK2*XK2
                XC3  = VAR1*XK1*XK1
                VAR1 = BMSP*DEUPI2*DEP2
                TK1 = (GRAVIT*DEP*(1.D0+XC1)-VAR1*FREQ3*FREQ3)
                TK2 = (GRAVIT*DEP*(1.D0+XC2)-VAR1*FREQ2*FREQ2)
                TK3 = (GRAVIT*DEP*(1.D0+XC3)-VAR1*FREQ1*FREQ1)
!
                BK1 = DEUPI*FREQ3*(1.D0+3.D0*XC1)
                BK3 = DEUPI*FREQ1*(1.D0+3.D0*XC3)
!
!
!.........TAKES THE SOURCE TERM INTO ACCOUNT
!         """"""""""""""""""""""""""""""""
!
                NRJ2  = (1.D0-AP2)*((1.D0-FR1)*F(IPO,IPM,IFR)+FR1*
     &                  F(IPO,IPM,IFR+1))
     &                  + AP2*((1.D0-FR1)*F(IPO,IPP,IFR)
     &                  +FR1*F(IPO,IPP,IFR+1))
!
                BISP  = FILT*
     &                  ((VR2/TK2)*F(IPO,IPL,IFF)*F(IPO,JPL,JFF)
     &                  +(VR3/TK3)*F(IPO,IPL,IFF)*NRJ2
     &                  -(VR1/TK1)*F(IPO,JPL,JFF)*NRJ2)
!
!                D12   = FILT*((VR2/TK2)*F(IPO,JPL,JFF)
!    &                        +(VR3/TK3)*NRJ2)
!                D21   = FILT*((VR2/TK2)*F(IPO,IPL,IFF)
!    &                        -(VR1/TK1)*NRJ2)
                VR1   = DFREQ(JFF)*DTETA*VR1/BK1
                VR3   = 2.D0*DFREQ(IFF)*DTETA*VR3/BK3
!
                TSTOT(IPO,IPL,IFF) = TSTOT(IPO,IPL,IFF) + VR1*BISP
                TSTOT(IPO,JPL,JFF) = TSTOT(IPO,JPL,JFF) - VR3*BISP
!
              ENDDO ! IPO
            ENDDO ! IP1
          ENDDO ! IP3
        ENDDO ! JFF
      ENDDO ! IFF
!
      RETURN
      END
