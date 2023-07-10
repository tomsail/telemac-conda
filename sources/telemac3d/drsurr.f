!                   *****************
                    SUBROUTINE DRSURR
!                   *****************
!
     & (DELTAR,TA,BETAC,T0AC,RHO,RHO0,XMVS0,S3D_RHOS,DENLAW,S3D_SEDI,
     &  NTRAC,IND_T,IND_S,IND_SED,NSUSP_TEL,S3D_MIXTE,
     &  NUM_ISUSP_ICLA,NSICLA)
!
!***********************************************************************
! TELEMAC3D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES DELTAR = (RHO-RHO0)/RHO0.
!
!note     DENLAW =0,1,2,3 (NOTHING, DEPENDS ON S3D_TEMP., SALINITY, OR BOTH).
!
!warning  ASSUMES THAT RHO0 IS GIVEN AT T=15degC AND S=0 MG/L
!+            AND THAT BETA(S)=-1E-3 (L/MG) AT T=15degC
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+
!
!history  JMH   AG - LNHE
!+        **/11/2000
!+        V5P2
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        18/12/2012
!+        V6P3
!+   Comments changed, RHO0 had two meanings, now RHOREF and RHO0.
!+   Name of corresponding keyword changed.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!history  WA BREUGEM (IMDC)
!+        13/08/2015
!+        V8P1
!+   Jackett et al. (2006) density law added
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETAC          |-->| -(1/RHO)*(DRHO/DT) FOR TRACERS WHEN CONSTANT
!| DELTAR         |<->| (RHO-RHO0)/RHO0
!| DENLAW         |-->| CHOICE OF DENSITY LAW (SEE ABOVE)
!| IND_S          |-->| INDEX FOR SALINITY
!| IND_SED        |-->| INDEX FOR SEDIMENT
!| IND_T          |-->| INDEX FOR TEMPERATURE
!| NSUSP_TEL      |-->| NUMBER OF SEDIMENTS IN SUSPENSION
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| RHO            |<->| WATER DENSITY
!| RHO0           |-->| AVERAGE WATER DENSITY IN THE DOMAIN
!| S3D_MIXTE      |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| S3D_RHOS       |-->| SEDIMENT DENSITY FOR SEDI3D
!| S3D_SEDI       |-->| IF YES, THERE IS SEDIMENT
!| T0AC           |-->| REFERENCE CONCENTRATION OF TRACERS
!| TA             |-->| TRACERS
!| XMVS           |-->| SEDIMENT DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_DRSURR => DRSURR
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D, ONLY : ITURBV,GRAV,Z,NPLAN,NPOIN2,
     &                                   RHOPOT
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NTRAC, DENLAW,IND_T,IND_S
      INTEGER, INTENT(IN)           :: IND_SED,NSUSP_TEL
      INTEGER, INTENT(IN)           :: NSICLA
      DOUBLE PRECISION, INTENT(IN)  :: RHO0,XMVS0(NSICLA),S3D_RHOS
      DOUBLE PRECISION, INTENT(IN)  :: BETAC(NTRAC), T0AC(NTRAC)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: DELTAR
      TYPE(BIEF_OBJ), INTENT(IN)    :: TA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: RHO
      LOGICAL, INTENT(IN)           :: S3D_SEDI, S3D_MIXTE
      INTEGER, INTENT(IN)           :: NUM_ISUSP_ICLA(NSICLA)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC,NTRACM1,ISUSP,ICLA
!
      INTEGER IPOIN,IPLAN,IND
!     LOTS OF VARIABLES FOR JACKETT ET AL, 2006 EQ OF STATE
      DOUBLE PRECISION ANUM,ADEN,PRES,TH2,SQRTS, STMP,ADENPOT,ANUMPOT
      DOUBLE PRECISION,PARAMETER :: A1  =  9.9984085444849347D+02
      DOUBLE PRECISION,PARAMETER :: A2  =  7.3471625860981584D+00
      DOUBLE PRECISION,PARAMETER :: A3  = -5.3211231792841769D-02
      DOUBLE PRECISION,PARAMETER :: A4  =  3.6492439109814549D-04
      DOUBLE PRECISION,PARAMETER :: A5  =  2.5880571023991390D+00
      DOUBLE PRECISION,PARAMETER :: A6  = -6.7168282786692355D-03
      DOUBLE PRECISION,PARAMETER :: A7  =  1.9203202055760151D-03
      DOUBLE PRECISION,PARAMETER :: A8  =  1.1798263740430364D-02
      DOUBLE PRECISION,PARAMETER :: A9  =  9.8920219266399117D-08
      DOUBLE PRECISION,PARAMETER :: A10 =  4.6996642771754730D-06
      DOUBLE PRECISION,PARAMETER :: A11 = -2.5862187075154352D-08
      DOUBLE PRECISION,PARAMETER :: A12 = -3.2921414007960662D-12
      DOUBLE PRECISION,PARAMETER :: B1  =  1.0000000000000000D+00
      DOUBLE PRECISION,PARAMETER :: B2  =  7.2815210113327091D-03
      DOUBLE PRECISION,PARAMETER :: B3  = -4.4787265461983921D-05
      DOUBLE PRECISION,PARAMETER :: B4  =  3.3851002965802430D-07
      DOUBLE PRECISION,PARAMETER :: B5  =  1.3651202389758572D-10
      DOUBLE PRECISION,PARAMETER :: B6  =  1.7632126669040377D-03
      DOUBLE PRECISION,PARAMETER :: B7  = -8.8066583251206474D-06
      DOUBLE PRECISION,PARAMETER :: B8  = -1.8832689434804897D-10
      DOUBLE PRECISION,PARAMETER :: B9  =  5.7463776745432097D-06
      DOUBLE PRECISION,PARAMETER :: B10 =  1.4716275472242334D-09
      DOUBLE PRECISION,PARAMETER :: B11 =  6.7103246285651894D-06
      DOUBLE PRECISION,PARAMETER :: B12 = -2.4461698007024582D-17
      DOUBLE PRECISION,PARAMETER :: B13 = -9.1534417604289062D-18
      LOGICAL,SAVE :: DEJARHO = .FALSE.
!
!***********************************************************************
!
!-----------------------------------------------------------------------
!
! TRACERS OTHER THAN SEDIMENT
!
      IF(DENLAW.GE.1.AND.DENLAW.LE.3) THEN
!
        IF(DENLAW.EQ.1) THEN
!
!         LAW ACCORDING TO TEMPERATURE
!         RHO = RHOREF(1-(7(T-T0)**2)*1.E-6)
!                                                  -3
!         WITH T0=4degC   AND   RHOREF=999.972 KG.M
!
!         NOTE: ONLY THE GRADIENT OF DELTAR APPEARS IN EQUATIONS
!
          CALL OS( 'X=Y+C   ',X=RHO,Y=TA%ADR(IND_T)%P,C=-4.D0)
          CALL OS( 'X=XY    ',X=RHO,Y=RHO)
          CALL OS( 'X=CX    ',X=RHO,C=7.D-6 )
          CALL OS( 'X=X+C   ',X=RHO,C=-1.D0 )
          CALL OS( 'X=CX    ',X=RHO,C=-999.972D0)
!
        ELSEIF(DENLAW.EQ.2) THEN
!
!         LAW ACCORDING TO SALINITY S
!         RHO = RHOREF(1+750S*1.E-6)
!
!                                 -3
!         WITH RHOREF=999.972 KG.M
!
          CALL OS( 'X=CY    ',X=RHO,Y=TA%ADR(IND_S)%P,C=750.D-6)
          CALL OS( 'X=X+C   ',X=RHO,C=1.D0)
          CALL OS( 'X=CX    ',X=RHO,C=999.972D0)
!
        ELSEIF(DENLAW.EQ.3) THEN
!
!         LAW ACCORDING TO BOTH TEMPERATURE AND SALINITY
!         RHO = RHOREF(1-(7(T-T0)**2-750S)*1.E-6)
!                                                  -3
!         WITH T0=4degC   AND   RHOREF=999.972 KG.M
!
          CALL OS( 'X=Y+C   ',X=RHO,Y=TA%ADR(IND_T)%P,C=-4.D0  )
          CALL OS( 'X=XY    ',X=RHO,Y=RHO)
          CALL OS( 'X=CX    ',X=RHO,C=7.D-6 )
          CALL OS( 'X=X+CY  ',X=RHO,Y=TA%ADR(IND_S)%P,C=-750.D-6)
          CALL OS( 'X=X+C   ',X=RHO,C=-1.D0)
          CALL OS( 'X=CX    ',X=RHO,C=-999.972D0 )
!
        ENDIF
!
!       COMPUTES DRHO/DRO  = (RHO - RHO0)/RHO0
!       THE VALUE OF RHO0 GIVEN BY THE USER IS TAKEN HERE, IT TAKES INTO
!       ACCOUNT AN AVERAGE TEMPERATURE OR SALINITY IN THE DOMAIN, FOR A
!       BETTER BOUSSINESQ APPROXIMATION
!
        CALL OS( 'X=Y+C   ', X=DELTAR , Y=RHO , C=-RHO0 )
        CALL OS( 'X=CX    ', X=DELTAR , C=1.D0/RHO0 )
!
      ELSEIF(DENLAW.EQ.6) THEN
!       LAW ACCORDING TO
!       BASED ON JACKETT ET AL, 2006, ALGORITHMS FOR DENSITY,
!       POTENTIAL TEMPERATURE, CONSERVATIVE TEMPERATURE, AND THE
!       FREEZING TEMPERATURE OF SEAWATER. DOI: 10.1175/JTECH1946.1
!       FROM CODE DOWNLOADED AT
!       HTTP://WWW.TEOS-10.ORG/PRETEOS10_SOFTWARE/JMFWG06.HTML
!
!       RECALCULATE RHO FROM DELTAR

        !IN FIRST TIMESTEP, INITIALIZE RHO WITH RHO0 FOR PRESSURE CALC
        IF(.NOT.DEJARHO) THEN
!          RHO%R = RHO0
          CALL OS( 'X=C     ' , X=RHO, C=RHO0 )
          DEJARHO = .TRUE.
        ENDIF

        DO IPOIN=1,NPOIN2
!         SURFACE LAYER - PRESSURE IS ZERO
          PRES = 0.D0
          IND = IPOIN + (NPLAN-1) * NPOIN2
!
          TH2 = TA%ADR(IND_T)%P%R(IND)**2
          STMP = MAX(TA%ADR(IND_S)%P%R(IND),0.D0)
          SQRTS = SQRT(STMP)
!
!         NUMERATOR
          ANUM = A1 + TA%ADR(IND_T)%P%R(IND)*( A2 +
     &                TA%ADR(IND_T)%P%R(IND)*( A3 +
     &                TA%ADR(IND_T)%P%R(IND)*  A4))
     &         + STMP*(A5+TA%ADR(IND_T)%P%R(IND)*A6+STMP*A7)
!         DENOMINATOR
          ADEN = B1 + TA%ADR(IND_T)%P%R(IND)*( B2 +
     &                TA%ADR(IND_T)%P%R(IND)*( B3 +
     &                TA%ADR(IND_T)%P%R(IND)*( B4 +
     &                TA%ADR(IND_T)%P%R(IND)*  B5)))
     &         + STMP*( B6+TA%ADR(IND_T)%P%R(IND)*(B7+TH2*B8)
     &                 +SQRTS*(B9+TH2*B10))
          RHO%R(IND) = ANUM / ADEN
!         IF GOTM IS USED, CALCULATE POTENTIAL DENSITY RHOPOT
          IF(ITURBV.EQ.6) THEN
            RHOPOT%R(IND) = RHO%R(IND)
          ENDIF

!         LOOP OVER ALL NON-SURFACE LAYERS
          DO IPLAN=NPLAN-1,1,-1
            IND = IPOIN + (IPLAN-1) * NPOIN2
!
!           PRESSURE = PRESSURE ONE LAYER UP +
!             1E-4 * (RHO(THIS LAYER)+RHO(ONE LAYER UP))/2 * G *
!             (ELEVATION DIFFERENCE BETWEEN THOSE LAYERS)
!
!           CONVERSION FACTOR 1E-4 SINCE PRESSURE IN DECIBAR
!
            STMP = MAX(TA%ADR(IND_S)%P%R(IND),0.0D0)
            PRES = PRES
     &           + 0.5D-4*GRAV*(Z(IPOIN+IPLAN*NPOIN2)-
     &                          Z(IPOIN+(IPLAN-1)*NPOIN2))
     &                        *(RHO%R(IPOIN +  IPLAN*NPOIN2) +
     &                          RHO%R(IPOIN + (IPLAN-1)*NPOIN2))

!           PRE-CALCULATE SOME MANY-USED VALUES
            TH2 = TA%ADR(IND_T)%P%R(IND)**2
            SQRTS = SQRT(STMP)
!
!           NUMERATOR
            ANUMPOT = A1 + TA%ADR(IND_T)%P%R(IND)*( A2 +
     &                     TA%ADR(IND_T)%P%R(IND)*( A3 +
     &                     TA%ADR(IND_T)%P%R(IND)*  A4))
     &              + STMP*(A5 + TA%ADR(IND_T)%P%R(IND)*A6 + STMP*A7)

            ANUM = ANUMPOT + PRES*(  A8 + TH2*A9 + STMP*A10
     &                             + PRES*( A11 + TH2*A12 ))
!
!           DENOMINATOR
            ADENPOT = B1 + TA%ADR(IND_T)%P%R(IND)*( B2 +
     &                     TA%ADR(IND_T)%P%R(IND)*( B3 +
     &                     TA%ADR(IND_T)%P%R(IND)*( B4 +
     &                     TA%ADR(IND_T)%P%R(IND)*  B5)))
     &           + STMP*( B6 + TA%ADR(IND_T)%P%R(IND)*(B7+TH2*B8)
     &                       + SQRTS*(B9+TH2*B10))

            ADEN = ADENPOT + PRES*(B11+PRES*TA%ADR(IND_T)%P%R(IND)
     &                                     *(TH2*B12 + PRES*B13))
!
            RHO%R(IND) = ANUM / ADEN

!           IF GOTM IS USED, CALCULATE POTENTIAL DENSITY RHOPOT
            IF (ITURBV.EQ.6) THEN
              RHOPOT%R(IND) = ANUMPOT / ADENPOT
            ENDIF

          ENDDO
        ENDDO
!
!       COMPUTES DRHO/DRO  = (RHO - RHO0)/RHO0
!       THE VALUE OF RHO0 GIVEN BY THE USER IS TAKEN HERE, IT TAKES INTO
!       ACCOUNT AN AVERAGE TEMPERATURE OR SALINITY IN THE DOMAIN, FOR A
!       BETTER BOUSSINESQ APPROXIMATION
!
        CALL OS( 'X=Y+C   ', X=DELTAR , Y=RHO , C=-RHO0 )
        CALL OS( 'X=CX    ', X=DELTAR , C=1.D0/RHO0 )
!
      ELSEIF(DENLAW.EQ.4) THEN
!
!       COMPUTES DELTAR WITH COEFFICIENTS BETAC GIVEN BY THE USER
!
!       BEWARE : BETA = - (1/RHO0)*(RHO-RHO0)/(TA-T0AC)
!                HENCE - SIGN IN SECOND CALL TO OS
!                BECAUSE DELTAR = (RHO-RHO0)/RHO0
!
!       SEDIMENT (TRACER NUMBER NTRAC) IS REMOVED IN THIS LOOP (AND TREATED AFTER)
!
        CALL OS( 'X=0     ' , X=DELTAR )
!
        IF(NSUSP_TEL.GT.0) THEN
          NTRACM1=NTRAC-NSUSP_TEL
        ELSEIF(S3D_SEDI) THEN
          IF(S3D_MIXTE) THEN
            NTRACM1=NTRAC-2
          ELSE
            NTRACM1=NTRAC-1
          ENDIF
        ELSE
          NTRACM1=NTRAC
        ENDIF
!
        IF(NTRACM1.GT.0) THEN
          DO ITRAC = 1,NTRACM1
            CALL OS('X=X+CY  ',X=DELTAR,Y=TA%ADR(ITRAC)%P,
     &                         C=-BETAC(ITRAC) )
            CALL OS('X=X+C   ',X=DELTAR,
     &                         C=T0AC(ITRAC)*BETAC(ITRAC))
          ENDDO
        ENDIF
!
        CALL OS( 'X=CY    ', X=RHO , Y=DELTAR , C=RHO0 )
        CALL OS( 'X=X+C   ', X=RHO , C=RHO0 )
!
      ELSEIF(DENLAW.EQ.0.OR.DENLAW.EQ.5) THEN
!
        CALL OS('X=0     ',X=DELTAR)
        CALL OS('X=C     ',X=RHO,C=RHO0)
!
      ELSE
!
        WRITE(LU,*) 'WRONG DENSITY LAW IN DRSURR'
        CALL PLANTE(1)
        STOP
!
      ENDIF
      IF(ITURBV.EQ.6.AND.(DENLAW.GE.0.AND.DENLAW.LE.5)) THEN
        CALL OS('X=Y     ',X=RHOPOT,Y=RHO)
      ENDIF
!
!     EFFECT OF SEDIMENT
!     ALWAYS TAKEN INTO ACCOUNT IN THE MOMENTUM EQUATIONS
!     EXCEPT IF DENLAW = 5
!
      IF (DENLAW.NE.5) THEN
        IF(NSUSP_TEL.GT.0) THEN
          DO ITRAC = IND_SED,IND_SED+NSUSP_TEL-1
            ISUSP=ITRAC-IND_SED+1
            ICLA=NUM_ISUSP_ICLA(ISUSP)
            CALL OS('X=X+CY  ',X=DELTAR,Y=TA%ADR(ITRAC)%P,
     &               C=(XMVS0(ICLA)-RHO0)/(RHO0*XMVS0(ICLA)))
          ENDDO
        ELSEIF(S3D_SEDI) THEN
          IF(S3D_MIXTE) THEN
            CALL OS('X=X+CY  ',X=DELTAR,Y=TA%ADR(NTRAC-1)%P,
     &                         C=(S3D_RHOS-RHO0)/(RHO0*S3D_RHOS))
            CALL OS('X=X+CY  ',X=DELTAR,Y=TA%ADR(NTRAC)%P,
     &                         C=(S3D_RHOS-RHO0)/(RHO0*S3D_RHOS))
          ELSE
            CALL OS('X=X+CY  ',X=DELTAR,Y=TA%ADR(NTRAC)%P,
     &                         C=(S3D_RHOS-RHO0)/(RHO0*S3D_RHOS))
          ENDIF
        ENDIF
      ENDIF
!
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
