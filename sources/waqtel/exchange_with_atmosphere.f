!                     *******************************
                      MODULE EXCHANGE_WITH_ATMOSPHERE
!                     *******************************
!
!***********************************************************************
! WAQTEL   V8P2
!***********************************************************************
!
!brief    Module containing some subroutines to deal with heat exchange
!+        with atmosphere
!
!history  N. DURAND, A. GINEAU (EDF-LNHE)
!+        MAY 2011
!+        V6P0
!+   SOLRAD, SHORTRAD, EVAPO SUBROUTINES
!+   LEAP, DAYNUM FUNCTIONS FROM SOGREAH (NOW ARTELIA)
!
!history  C.-T. PHAM (EDF-LNHE)
!+        27/06/2012
!+        V6P2
!+   Creation of module EXCHANGE_WITH_ATMOSPHERE from previous
!+   subroutines
!
!history  N. DURAND, N. LORRAIN, C.-T. PHAM (EDF-LNHE)
!+        03/04/2014
!+        V7P0
!+   Update after Nicolas Lorrain's internship
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      PRIVATE
      PUBLIC :: SOLRAD,SHORTRAD,EVAPO,LEAP,DAYNUM
!
!-----------------------------------------------------------------------
!
!  brought to declarations_waqtel
!  CP: SPECIFIC HEAT OF WATER AT CONSTANT PRESSURE
!      DOUBLE PRECISION, PARAMETER :: CP        = 4.18D3
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!                   *****************
                    SUBROUTINE SOLRAD
!                   *****************
!
     &(RAY_SOL,NEBU,MARDAT,MARTIM,AT,LATITUDE,LONGITUDE)
!
!***********************************************************************
! TELEMAC-3D V7P0                             22/06/2012
!***********************************************************************
!
!brief    EVALUATES SOLAR RADIATION INCIDENT ON THE SEA SURFACE
!+          - CALCULATES SOLAR RADIATION AS FUNCTION OF DAY NUMBER
!+            OF THE YEAR AND GEOGRAPHICAL LOCATION
!+          - INCLUDES ATMOSPHERICAL ABSORPTION AND REFLECTION, CLOUD
!+            COVERAGE, SEA SURFACE ALBEDO
!+          - TIME EXPRESSED IN GMT
!+        SOURCES:
!+          - PERRIN DE BRICHAMBAUT (1975)
!+          - BERLIAND'S METHOD (1960)
!+          - COOPER'S FORMULA (1969)
!
!history  N. DURAND, A. GINEAU (EDF-LNHE)
!+        MAY 2011
!+        V6P0
!+
!
!history  C.-T. PHAM (EDF-LNHE)
!+        JUNE 2012
!+        V6P2
!+   Adding of MARDAT, MARTIM, LATITUDE, LONGITUDE, ALBEDO AND type of
!+   sky as new arguments + INTENT + cosmetics
!+
!
!history  C.-T. PHAM (EDF-LNHE)
!+        03/04/2014
!+        V7P0
!+   Update after Nicolas Lorrain's internship
!+   Albedo is not constant in time and is no more an argument
!+   The type of sky is no more an argument, but fixed in the subroutine
!+   and may be changed by the user
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| CURRENT TIME
!| NEBU           |-->| NEBULOSITY (IN OCTAS)
!| LATITUDE       |-->| LATITUDE
!| LONGITUDE      |-->| LONGITUDE
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| RAY_SOL        |<--| SOLAR RADIATION INCIDENT ON THE SEA SURFACE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_WAQTEL,ONLY: ISKYTYPE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(IN)  :: AT,NEBU
      DOUBLE PRECISION, INTENT(IN)  :: LATITUDE,LONGITUDE
      DOUBLE PRECISION, INTENT(OUT) :: RAY_SOL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC
!
      DOUBLE PRECISION DTR,PI,ALB
      DOUBLE PRECISION DAY,DAYREEL,NDAYS
!  HA  : SUN'S HOUR ANGLE         [rad]
!  HR  : TIME OF THE DAY IN HOURS (GMT)
!  RDEC: SUN'S DECLINATION        [rad]
!  RLAT: LATITUDE                 [rad]
!  SING: SIN(GAMMA)
!  TE  : TIME EQUATION            [hours]
      DOUBLE PRECISION HA,HR,RDEC,RLAT,SING,TE
!  AA,BB: COEFFICIENTS DEALING WITH LUMINOSITY AND SKY COLOUR
      DOUBLE PRECISION AA,BB
!
!     INTEGER  LEAP,DAYNUM
!     EXTERNAL LEAP,DAYNUM
!
!-----------------------------------------------------------------------
!
!
      IF(ISKYTYPE.EQ.1) THEN
!  VERY PURE SKY
        AA = 1130.D0
        BB = 1.15D0
      ELSEIF(ISKYTYPE.EQ.2) THEN
!  MEAN PURE SKY
        AA = 1080.D0
        BB = 1.22D0
      ELSEIF(ISKYTYPE.EQ.3) THEN
!  INDUSTRIAL AREA
        AA = 995.D0
        BB = 1.25D0
      ENDIF
!
      IYEAR  = MARDAT(1)
      IMONTH = MARDAT(2)
      IDAY   = MARDAT(3)
      IHOUR  = MARTIM(1)
      IMIN   = MARTIM(2)
      ISEC   = MARTIM(3)
!
!-----------------------------------------------------------------------
!
      PI  = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
!
!  DAY NUMBER, ORBITAL CORRECTION
      DAY = DAYNUM(IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC)
     &    + FLOOR(AT/86400.D0)
      NDAYS = 365.D0 + REAL(LEAP(IYEAR))
      DAYREEL = MODULO(DAY, NDAYS)

!  ALBEDO WITH RESPECT OF THE MONTH
      IF(DAYREEL.GE.0.D0.AND.DAYREEL.LE.31.D0) THEN
        ALB = 0.11D0
      ELSEIF(DAYREEL.GT.31.D0.AND.DAYREEL.LE.59.D0) THEN
        ALB = 0.10D0
      ELSEIF(DAYREEL.GT.59.D0.AND.DAYREEL.LE.90.D0) THEN
        ALB = 0.08D0
      ELSEIF(DAYREEL.GT.90.D0.AND.DAYREEL.LE.120.D0) THEN
        ALB = 0.07D0
      ELSEIF(DAYREEL.GT.120.D0.AND.DAYREEL.LE.151.D0) THEN
        ALB = 0.06D0
      ELSEIF(DAYREEL.GT.151.D0.AND.DAYREEL.LE.181.D0) THEN
        ALB = 0.06D0
      ELSEIF(DAYREEL.GT.181.D0.AND.DAYREEL.LE.212.D0) THEN
        ALB = 0.06D0
      ELSEIF(DAYREEL.GT.212.D0.AND.DAYREEL.LE.243.D0) THEN
        ALB = 0.07D0
      ELSEIF(DAYREEL.GT.243.D0.AND.DAYREEL.LE.273.D0) THEN
        ALB = 0.07D0
      ELSEIF(DAYREEL.GT.273.D0.AND.DAYREEL.LE.304.D0) THEN
        ALB = 0.08D0
      ELSEIF(DAYREEL.GT.304.D0.AND.DAYREEL.LE.334.D0) THEN
        ALB = 0.11D0
      ELSEIF(DAYREEL.GT.334.D0.AND.DAYREEL.LE.365.D0) THEN
        ALB = 0.12D0
      ENDIF

!  DECLINATION OF SUN (COOPER'S FORMULA)
!      RDEC = (23.45D0*SIN(2.D0*PI*(DAYREEL+284.D0)/NDAYS))*DTR
      RDEC = (23.45D0*COS(2.D0*PI*(172.D0-DAYREEL)/NDAYS))*DTR

!  TIME EQUATION
      TE = ( 450.68D0*SIN(2.D0*PI*DAYREEL/NDAYS-0.026903D0)
     &      +595.40D0*SIN(4.D0*PI*DAYREEL/NDAYS+0.352835D0))/3600.D0
!  SOLAR ALTITUDE
      HR = IHOUR+MODULO(AT,86400.D0)/3600.D0
!
      RLAT = LATITUDE*DTR
      HA   = (HR-TE-12.D0 + LONGITUDE/15.D0)*PI/12.D0
      SING = SIN(RLAT)*SIN(RDEC) + COS(RLAT)*COS(RDEC)*COS(HA)
!  SOLAR RADIATION
      IF(SING.LE.0.D0) THEN
        RAY_SOL = 0.D0
      ELSE
!  THE NEBULOSITY IS GIVEN IN OCTAS
        RAY_SOL = AA*(SING**BB)*(1.D0-0.65D0*(NEBU/8.D0)**2)
     &              *(1.D0-ALB)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SOLRAD

!                       *********************
                        INTEGER FUNCTION LEAP
!                       *********************
!
     &(IYEAR)
!
!***********************************************************************
! TELEMAC-3D V6P2                             27/06/2012
!***********************************************************************
!
!brief    DETERMINES WHETHER IYEAR IS A LEAP YEAR
!+        DESCRIPTION - RETURNS 1 IF IYEAR IS A LEAP YEAR, 0 OTHERWISE
!+
!
!history  C. GUILBAUD (SOGREAH)
!+        JUNE 2001
!+        V6P0?
!+
!
!history  C.-T. PHAM (EDF-LNHE)
!+        27/06/2012
!+        V6P2
!+   Introduction into EXCHANGE_WITH_ATMOSPHERE module + INTENT
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IYEAR          |-->| INDEX OF YEAR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IYEAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF( MOD(IYEAR,4).EQ.0.AND.
     &   (MOD(IYEAR,100).NE.0.OR.MOD(IYEAR,400).EQ.0)) THEN
        LEAP = 1
      ELSE
        LEAP = 0
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION LEAP

!                   ********************************
                    DOUBLE PRECISION FUNCTION DAYNUM
!                   ********************************
!
     &(IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC)
!
!***********************************************************************
! TELEMAC-3D V6P2                             27/06/2012
!***********************************************************************
!
!brief    RETURNS DAY NUMBER OF THE YEAR (FRACTIONAL)
!+
!
!history  C. GUILBAUD (SOGREAH)
!+        JUNE 2001
!+        V6P0?
!+
!
!history  C.-T. PHAM (EDF-LNHE)
!+        27/06/2012
!+        V6P2
!+   Introduction into EXCHANGE_WITH_ATMOSPHERE module
!+   Change for type of result (double precision, not integer)
!+   + REAL conversion + addition of seconds ISEC
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IDAY           |-->| INDEX OF DAY
!| IHOUR          |-->| INDEX OF HOUR
!| IMIN           |-->| INDEX OF MINUTE
!| IMONTH         |-->| INDEX OF MONTH
!| ISEC           |-->| INDEX OF SECOND
!| IYEAR          |-->| INDEX OF YEAR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     INTEGER  LEAP
!     EXTERNAL LEAP
!
      INTEGER MONTH(12)
      PARAMETER ( MONTH=(/0,31,59,90,120,151,181,212,243,273,304,334/) )
!
!-----------------------------------------------------------------------
!
      DAYNUM = REAL(MONTH(IMONTH)+IDAY)
     &       + REAL(IHOUR)/24.D0+REAL(IMIN)/1440.D0+REAL(ISEC)/86400.D0
      IF(IMONTH.GT.2) DAYNUM = DAYNUM + REAL(LEAP(IYEAR))
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION DAYNUM

!                   *******************
                    SUBROUTINE SHORTRAD
!                   *******************
!
     &(TREEL,TAIR,NEBU,HREL,RAY_ATM,RAY_EAU)
!
!***********************************************************************
! TELEMAC-3D V7P0                             22/06/2012
!***********************************************************************
!
!brief    CALCULATES ATMOSPHERIC AND WATER RADIATIONS
!+        SOURCES:
!+          - SWINBANK'S METHOD
!+          - T.V.A. 1972
!+
!
!history  N. DURAND, A. GINEAU (EDF-LNHE)
!+        MAY 2011
!+        V6P0
!+
!
!history  C.-T. PHAM (EDF-LNHE)
!+        JUNE 2012
!+        V6P2
!+   Type of cloud taken into account + INTENT + cosmetics
!+
!
!history  C.-T. PHAM (EDF-LNHE)
!+        03/04/2014
!+        V7P0
!+   Update after Nicolas Lorrain's internship
!+   The type of cloud is no more an argument, but fixed in the subroutine
!+   and may be changed by the user
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NEBU           |-->| NEBULOSITY (IN OCTAS)
!| RAY_ATM        |<--| ATMOSPHERIC RADIATION
!| RAY_EAU        |<--| WATER RADIATION
!| TAIR           |-->| AIR TEMPERATURE
!| TREEL          |-->| REAL WATER TEMPERATURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_WAQTEL, ONLY: EMI_EAU,BOLTZ,EMA,IRAY_ATM,COEF_K
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: TREEL,TAIR,NEBU,HREL
      DOUBLE PRECISION, INTENT(OUT) :: RAY_ATM,RAY_EAU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  NUA: COEFFICIENT DEPENDING ON THE TYPE OF CLOUDS
!  REPLACED BY COEF_K (KEYWORD COEFFICIENT OF CLOUDING RATE)
!  ICLOUDTYPE FOR THE TYPE OF CLOUD IS THEN USELESS
!
!  TMP TO USE THE SAME FORMULAE AS IN GLM
      DOUBLE PRECISION EPS_STAR
      DOUBLE PRECISION QSAT_AIR
!
!-----------------------------------------------------------------------
!
!  ATMOSPHERIC RADIATION
      IF(IRAY_ATM.EQ.1) THEN
!       CASE 1: IDSO AND JACKSON (1969)
        EPS_STAR = (1.D0+0.275D0*(NEBU/8.D0))
     &            *(1.D0-0.261D0*EXP(-7.77D-8*TAIR**2))
      ELSEIF(IRAY_ATM.EQ.2) THEN
!       CASE 2: SWINBANK (1963)
!       DEFAULT, LIKE VERSIONS V7P1 AND BEFORE
        EPS_STAR = (1.D0+COEF_K*(NEBU/8.D0)**2)*0.937D-5
     &            *(TAIR+273.15D0)**2
      ELSEIF(IRAY_ATM.EQ.3) THEN
!       CASE 3: BRUTSAERT (1975)
        QSAT_AIR = 10.D0**(9.28603523D0-(2322.37885D0/(TAIR+273.15D0)))
        EPS_STAR = (1.D0+0.275D0*(NEBU/8.D0))*0.642D0
     &            *((HREL/100.D0)*QSAT_AIR/(TAIR+273.15D0))**(1.D0/7.D0)
      ELSEIF(IRAY_ATM.EQ.4) THEN
!       CASE 4: YAJIMA TONO DAM (2014)
        QSAT_AIR = 10.D0**(9.28603523D0-(2322.37885D0/(TAIR+273.15D0)))
        EPS_STAR = (1.D0-(NEBU/8.D0)**2.796D0)*1.24D0
     &            *((HREL/100.D0)*QSAT_AIR/(TAIR+273.15D0))**(1.D0/7.D0)
     &           + 0.955D0*(NEBU/8.D0)**2.796D0
      ELSE
        WRITE(LU,*) 'FORMULA OF ATMOSPHERIC RADIATION NOT POSSIBLE'
        CALL PLANTE(1)
      ENDIF
!
      RAY_ATM = EMA*EPS_STAR*BOLTZ*(TAIR+273.15D0)**4
!
!  WATER RADIATION
      RAY_EAU = EMI_EAU*BOLTZ*(TREEL+273.15D0)**4
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SHORTRAD

!                   ****************
                    SUBROUTINE EVAPO
!                   ****************
!
     &(TREEL,TAIR,W2,PATM,HREL,RO,FLUX_EVAP,FLUX_SENS,DEBEVAP,C_ATMOS,
     & C1_ATMOS,C2_ATMOS)
!
!***********************************************************************
! WAQTEL   V8P2
!***********************************************************************
!
!brief    CALCULATES FLUX OF LATENT HEAT (W/M^2)
!+        CALCULATES SENSIBLE FLUX (W/M^2)
!+        CALCULAGES EVAPORATED WATER FLOWRATE (M/S)
!+        SOURCES:
!+          - BOLTON 1980 FOR SATURATION VAPOUR PRESSURE
!+
!
!history  N. DURAND, A. GINEAU (EDF-LNHE)
!+        MAY 2011
!+        V6P0
!+
!
!history  C.-T. PHAM (EDF-LNHE)
!+        JUNE 2012
!+        V6P2
!+   Parameter B to calibrate in arguments + cosmetics
!+
!
!history  C.-T. PHAM (EDF-LNHE)
!+        03/04/2014
!+        V7P0
!+   Update after Nicolas Lorrain's internship
!+   The salinity is not an argument anymore.
!+   Every salinity correction has been removed
!+
!
!history  M. GANT, L. ABBAS, C.-T. PHAM (EDF-LNHE)
!+        07/12/2015
!+        V7P1
!+   DEBEVAP and FLUX_EVAP have to be greater than or equal to zero.
!+   Otherwise, rain is read through files
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C_ATMOS        |-->| PARAMETER TO CALIBRATE (FWW = C_ATMOS*(1.D0+W2))
!| C1_ATMOS       |-->| PARAMETER TO CALIBRATE (FWW = C1_ATMOS+C2_ATMOS*W)
!| C2_ATMOS       |-->| PARAMETER TO CALIBRATE (FWW = C1_ATMOS+C2_ATMOS*W)
!| DEB_EVAP       |<--| EVAPORATION FLOWRATE AT THE SURFACE
!| FLUX_EVAP      |<--| ENERGY FLUX DUE TO EVAPORATED WATER
!| FLUX_SENS      |<--| HEAT FLUX BY CONVECTION
!| HREL           |-->| RELATIVE HUMIDITY
!| PATM           |-->| ATMOSPHERIC PRESSURE
!| RO             |-->| DENSITY
!| TAIR           |-->| AIR TEMPERATURE
!| TREEL          |-->| REAL WATER TEMPERATURE
!| W2             |-->| RELATIVE MAGNITUDE OF WIND AT 2 M
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_WAQTEL, ONLY: CP_AIR,N_C_ATMOS
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: TREEL,TAIR,W2,PATM,HREL,RO
      DOUBLE PRECISION, INTENT(IN)  :: C_ATMOS,C1_ATMOS,C2_ATMOS
      DOUBLE PRECISION, INTENT(OUT) :: FLUX_EVAP,FLUX_SENS,DEBEVAP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION Q_SAT_EAU,Q_SAT_AIR,HUMI_EAU,HUMI_AIR,FWW,ROAIR
!
!-----------------------------------------------------------------------
!
!  SATURATION VAPOUR PRESSURE (MAGNUS TETENS)
      Q_SAT_EAU  = EXP(2.3026D0*(7.5D0*TREEL/(TREEL+237.3D0)+0.7858D0))
      Q_SAT_AIR  = EXP(2.3026D0*(7.5D0*TAIR/(TAIR+237.3D0)+0.7858D0))
!
!  AIR DENSITY : IDEAL GAZ LAW
      ROAIR =    PATM*100.D0/(287.D0*(TAIR+273.15D0))
     &      - 1.32D-5*HREL*Q_SAT_AIR/(TAIR+273.15D0)
!
!  HUMIDITY
!  0.378D0 = 1.D0-0.622D0
      HUMI_EAU  = 0.622D0*Q_SAT_EAU/(PATM-0.378D0*Q_SAT_EAU)
      HUMI_AIR  =        0.622D0*(HREL/100.D0)*Q_SAT_AIR
     &          / (PATM-(0.378D0*(HREL/100.D0)*Q_SAT_AIR))
!  HEAT FLUX BY EVAPORATION (SALENCON)
!
      IF(N_C_ATMOS.EQ.2) THEN
        FWW     = C1_ATMOS+C2_ATMOS*W2
      ELSE
!     N_C_ATMOS.EQ.1 IS THE DEFAULT OPTION, NO OTHER OPTION THAN 1 OR 2
        FWW     = C_ATMOS*(1.D0+W2)
      ENDIF
!
      FLUX_EVAP = ROAIR*(2500.9D3-TREEL*2.365D3)*FWW
     &                 *(HUMI_EAU-HUMI_AIR)
      FLUX_EVAP = MAX(FLUX_EVAP,0.D0)
!  HEAT FLUX BY CONVECTION
      FLUX_SENS = CP_AIR*ROAIR*FWW*(TREEL-TAIR)
!  EVAPORATION FLOWRATE AT THE SURFACE
      DEBEVAP   = ROAIR*FWW/RO*(HUMI_EAU-HUMI_AIR)
      DEBEVAP   = MAX(DEBEVAP,0.D0)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE EVAPO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE EXCHANGE_WITH_ATMOSPHERE
