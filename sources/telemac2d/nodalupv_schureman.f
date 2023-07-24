!                       *****************************
                        SUBROUTINE NODALUPV_SCHUREMAN
!                       *****************************
!
     &(UPVWAVE,WWAVE,MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    COMPUTES NODAL FACTORS PHASE FROM SCHUREMAN FORMULAE
!+
!
!history  C-T PHAM (LNHE)
!+        13/01/2012
!+        V6P2
!+
!
!history  C-T PHAM (LNHE)
!+        08/01/2014
!+        V7P0
!+   Adding 7 extra harmonic constituents, for Previmer database.
!+
!
!history  L.LEBALLEUR (ACTIMAR)
!+        30/03/2017
!+        V7P3
!+   Adding 7 extra harmonic constituents, for FES2014 database.
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MARDAT         |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR,MINUTE,SECOND)
!| UPVWAVE        |<--| U+V (ORIGIN + NODAL PHASE) FOR 61 WAVES
!| WWAVE          |<--| ANGULAR FREQUENCY OF 61 WAVES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_NODALUPV_SCHUREMAN
     &                         => NODALUPV_SCHUREMAN
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: MARDAT(3),MARTIM(3)
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION, INTENT(INOUT) :: UPVWAVE(*),WWAVE(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER YEAR,MONTH,DAY,NDAY,HOUR,MINUTE,SECOND,I
!
      DOUBLE PRECISION PI,TWOPI,DTR
      DOUBLE PRECISION TJ,TT,SLUN,HSOL,PLUN,NLUN,PSOL
      DOUBLE PRECISION XI,NU,TGI2,PPP,QU,RRR,NUPRIM,NUSEC
!     DOUBLE PRECISION QQQ
      DOUBLE PRECISION IANG,TGN2,AT1,AT2
      DOUBLE PRECISION WTT,WSLUN,WHSOL,WPLUN,WPSOL
      DOUBLE PRECISION VWAVE(61),UWAVE(61)
!
      INTRINSIC DBLE,MOD,INT
!
!-----------------------------------------------------------------------
!
      PI  = 4.D0*ATAN(1.D0)
      TWOPI = 2.D0*PI
      DTR = PI/180.D0
!
      YEAR  = MARDAT(1)
      MONTH = MARDAT(2)
      DAY   = MARDAT(3)
!
      HOUR   = MARTIM(1)
      MINUTE = MARTIM(2)
      SECOND = MARTIM(3)
!  NUMBER OF THE DAY IN YEAR YEAR
      NDAY = DAY
!
      DO I=MONTH-1,1,-1
        IF((I.EQ.1).OR.(I.EQ.3).OR.(I.EQ.5).OR.(I.EQ.7).OR.(I.EQ.8)
     &  .OR.(I.EQ.10)) THEN
          NDAY = NDAY + 31
        ELSEIF((I.EQ.4).OR.(I.EQ.6).OR.(I.EQ.9).OR.(I.EQ.11)) THEN
          NDAY = NDAY + 30
        ELSEIF(I.EQ.2) THEN
          IF((MOD(YEAR,4).NE.0)
     &    .OR.((MOD(YEAR,100).EQ.0).AND.(MOD(YEAR,400).NE.0))) THEN
            NDAY = NDAY + 28
          ELSE
            NDAY = NDAY + 29
          ENDIF
        ENDIF
      ENDDO
!
      TJ = DBLE(365*(YEAR-1900)+(NDAY-1)
     &         +DBLE(INT(DBLE(YEAR-1901)/4.D0)))/36525.D0
     &   +(DBLE(HOUR)+DBLE(MINUTE)/60.D0+DBLE(SECOND)/3600.D0)/876600.D0
!
!-----------------------------------------------------------------------
!
! SCHUREMAN FORMULAE P. 162, ORDER 2 IS ENOUGH
! IN DEGREES
! TJ TIME ELAPSED SINCE 01/01/1900 AT 0 H, IN JULIAN CENTURY
!
      SLUN = MOD(277.0256206D0+481267.892D0    *TJ+  2.525D-3*TJ**2,
     &           360.D0)
      HSOL = MOD(280.1895014D0+ 36000.768925D0 *TJ+  3.025D-4*TJ**2,
     &           360.D0)
      PLUN = MOD(334.3837215D0+  4069.0322056D0*TJ-1.03444D-2*TJ**2,
     &           360.D0)
      NLUN = MOD(259.1560564D0-  1934.1423972D0*TJ+ 2.1056D-3*TJ**2,
     &           360.D0)
!
      PSOL = MOD(281.2208569D0+     1.719175D0 *TJ+  4.528D-4*TJ**2,
     &           360.D0)
!
! TT MEAN GREENWICH SOLAR ANGLE, ORIGIN AT ZENITH
! 15.D0 DEG = PI/12.D0
! PI/12.D0*24.D0 = TWOPI
      TT = TWOPI*(TJ*36525.D0-DBLE(INT(TJ*36525.D0)))+PI
!
! CONVERSION IN RADIANS
! FROM NOW, EVERY ANGLE IS IN RADIAN
      SLUN = MOD(SLUN*DTR,TWOPI)
      HSOL = MOD(HSOL*DTR,TWOPI)
      PLUN = MOD(PLUN*DTR,TWOPI)
      NLUN = MOD(NLUN*DTR,TWOPI)
      PSOL = MOD(PSOL*DTR,TWOPI)
!
      IF (SLUN.LT.0.D0) SLUN = SLUN + TWOPI
      IF (HSOL.LT.0.D0) HSOL = HSOL + TWOPI
      IF (PLUN.LT.0.D0) PLUN = PLUN + TWOPI
      IF (NLUN.LT.0.D0) NLUN = NLUN + TWOPI
      IF (PSOL.LT.0.D0) PSOL = PSOL + TWOPI
!
! SCHUREMAN FORMULAE P. 156
!
! IANG FROM TUGO TOOLS TIDES.CPP (MORE DECIMALS THAN IN SCHUREMAN P. 156)
      IANG = ACOS(0.913694997D0-0.035692561D0*COS(NLUN))
!
      TGN2 = TAN(NLUN/2.D0)
      AT1 = ATAN(1.01883D0*TGN2)
      AT2 = ATAN(0.64412D0*TGN2)
!
      XI = -AT1-AT2+NLUN
      IF (NLUN.GT.PI) THEN
        XI = XI-TWOPI
      ENDIF
      NU = AT1-AT2
!
! FOR CONSTITUENTS L2,K1,K2
!
      TGI2 = TAN(IANG/2.D0)
! SCHUREMAN P. 41 (191)
      PPP  = PLUN-XI
!
! SCHUREMAN P. 41 (196)
      QU = ATAN( SIN(2.D0*PPP)
     &          /(3.D0*COS(IANG)/COS(IANG/2.D0)**2+COS(2.D0*PPP)))
! SCHUREMAN P. 42 (204)
!     QQQ = PPP-QU
! SCHUREMAN P. 44 (214)
      RRR=ATAN(SIN(2.D0*PPP)/(1.D0/(6.D0*TGI2**2)-COS(2.D0*PPP)))
! SCHUREMAN P. 45 (224)
      NUPRIM = ATAN(  SIN(2.D0*IANG)*SIN(NU)
     &              /(SIN(2.D0*IANG)*COS(NU)+0.3347D0))
! SCHUREMAN P. 46 (232)
      NUSEC = 0.5D0*ATAN(  SIN(IANG)**2*SIN(2.D0*NU)
     &                   /(SIN(IANG)**2*COS(2.D0*NU)+0.0727D0))
!
!-----------------------------------------------------------------------
!
! NODAL FACTORS, RECURRENT FORMULAE
!
      WTT   = 13149000.D0
      WSLUN =   481267.892D0
      WHSOL =    36000.768925D0
      WPLUN =     4069.0322056D0
!     WPLUN =     4069.03220555556D0
      WPSOL =        1.719175D0
!
!-----------------------------------------------------------------------
!
! 2MK6     M2+M2+K2
      VWAVE(1) = 6.D0*TT-4.D0*SLUN+6.D0*HSOL
      WWAVE(1) = 6.D0*WTT-4.D0*WSLUN+6.D0*WHSOL
      UWAVE(1) = 4.D0*XI-4.D0*NU-2.D0*NUSEC
! 2MN6     TAB 2A
      VWAVE(2) = 6.D0*TT-7.D0*SLUN+6.D0*HSOL+PLUN
      WWAVE(2) = 6.D0*WTT-7.D0*WSLUN+6.D0*WHSOL+WPLUN
      UWAVE(2) = 6.D0*XI-6.D0*NU
! 2MS6     TAB 2A
      VWAVE(3) = 6.D0*TT-4.D0*SLUN+4.D0*HSOL
      WWAVE(3) = 6.D0*WTT-4.D0*WSLUN+4.D0*WHSOL
      UWAVE(3) = 4.D0*XI-4.D0*NU
! 2N2      A42
      VWAVE(4) = 2.D0*TT-4.D0*SLUN+2.D0*HSOL+2.D0*PLUN
      WWAVE(4) = 2.D0*WTT-4.D0*WSLUN+2.D0*WHSOL+2.D0*WPLUN
      UWAVE(4) = 2.D0*XI-2.D0*NU
! 2Q1      A17
      VWAVE(5) = TT-4.D0*SLUN+HSOL+2.D0*PLUN+PI/2.D0
      WWAVE(5) = WTT-4.D0*WSLUN+WHSOL+2.D0*WPLUN
      UWAVE(5) = 2.D0*XI-NU
! 2SM2     TAB 2A
      VWAVE(6) = 2.D0*TT+2.D0*SLUN-2.D0*HSOL
      WWAVE(6) = 2.D0*WTT+2.D0*WSLUN-2.D0*WHSOL
      UWAVE(6) = -2.D0*XI+2.D0*NU
! 2SM6     TAB 2A
      VWAVE(7) = 6.D0*TT-2.D0*SLUN+2.D0*HSOL
      WWAVE(7) = 6.D0*WTT-2.D0*WSLUN+2.D0*WHSOL
      UWAVE(7) = 2.D0*XI-2.D0*NU
! EPSILON2 ( = MNS2)    TAB 2A
      VWAVE(8) = 2.D0*TT-5.D0*SLUN+4.D0*HSOL+PLUN
      WWAVE(8) = 2.D0*WTT-5.D0*WSLUN+4.D0*WHSOL+WPLUN
! UWAVE FOR EPSILON2:
! DIFFERENCES WITH TIDES.DEF: 2.D0*XI-2.D0*NU, AND FES2LIMPRO: 0.D0
      UWAVE(8) = 4.D0*XI-4.D0*NU
! TTA1     A28
      VWAVE(9) = TT+SLUN-HSOL+PLUN-PI/2.D0
      WWAVE(9) = WTT+WSLUN-WHSOL+WPLUN
      UWAVE(9) = -NU
! J1       A24
      VWAVE(10) = TT+SLUN+HSOL-PLUN-PI/2.D0
      WWAVE(10) = WTT+WSLUN+WHSOL-WPLUN
      UWAVE(10) = -NU
! K1       NOTE 2
      VWAVE(11) = TT+HSOL-PI/2.D0
      WWAVE(11) = WTT+WHSOL
      UWAVE(11) = -NUPRIM
! K2       NOTE 4
      VWAVE(12) = 2.D0*TT+2.D0*HSOL
      WWAVE(12) = 2.D0*WTT+2.D0*WHSOL
      UWAVE(12) = -2.D0*NUSEC
! KJ2      A49
      VWAVE(13) = 2.D0*TT+SLUN+2.D0*HSOL-PLUN
      WWAVE(13) = 2.D0*WTT+WSLUN+2.D0*WHSOL-WPLUN
      UWAVE(13) = -2.D0*NU   ! BUG IN TIDES.DEF!!!
! KQ1      A32
      VWAVE(14) = TT+3.D0*SLUN+HSOL-PLUN-PI/2.D0
      WWAVE(14) = WTT+3.D0*WSLUN+WHSOL-WPLUN
      UWAVE(14) = -2.D0*XI-NU
! L2       NOTE 3
      VWAVE(15) = 2.D0*TT-SLUN+2.D0*HSOL-PLUN+PI
      WWAVE(15) = 2.D0*WTT-WSLUN+2.D0*WHSOL-WPLUN
      UWAVE(15) = 2.D0*XI-2.D0*NU-RRR
! LAMBDA2  A44
      VWAVE(16) = 2.D0*TT-SLUN+PLUN+PI
      WWAVE(16) = 2.D0*WTT-WSLUN+WPLUN
      UWAVE(16) = 2.D0*XI-2.D0*NU
! M1       NOTE 1
! FORMULAE V0 = TT-SLUN+HSOL-PI/2.D0 AND U = XI-NU+QQQ NOT USED
! BECAUSE PLUN IS HIDDEN IN QQQ,
! NOT GOOD FOR THE CALCULATION OF THE PERIOD
! IN FORMULA A71-A23, FOR U, TERM QQQ IS MISSING
      VWAVE(17) = TT-SLUN+HSOL+PLUN-PI/2.D0
      WWAVE(17) = WTT-WSLUN+WHSOL+WPLUN
      UWAVE(17) = -NU-QU
! M2       A39
      VWAVE(18) = 2.D0*TT-2.D0*SLUN+2.D0*HSOL
      WWAVE(18) = 2.D0*WTT-2.D0*WSLUN+2.D0*WHSOL
      UWAVE(18) = 2.D0*XI-2.D0*NU
! M3       A82
      VWAVE(19) = 3.D0*TT-3.D0*SLUN+3.D0*HSOL
      WWAVE(19) = 3.D0*WTT-3.D0*WSLUN+3.D0*WHSOL
      UWAVE(19) = 3.D0*XI-3.D0*NU
! M4       TAB 2A
      VWAVE(20) = 4.D0*TT-4.D0*SLUN+4.D0*HSOL
      WWAVE(20) = 4.D0*WTT-4.D0*WSLUN+4.D0*WHSOL
      UWAVE(20) = 4.D0*XI-4.D0*NU
! M6       TAB 2A
      VWAVE(21) = 6.D0*TT-6.D0*SLUN+6.D0*HSOL
      WWAVE(21) = 6.D0*WTT-6.D0*WSLUN+6.D0*WHSOL
      UWAVE(21) = 6.D0*XI-6.D0*NU
! M8       TAB 2A
      VWAVE(22) = 8.D0*TT-8.D0*SLUN+8.D0*HSOL
      WWAVE(22) = 8.D0*WTT-8.D0*WSLUN+8.D0*WHSOL
      UWAVE(22) = 8.D0*XI-8.D0*NU
! MF       A6
      VWAVE(23) = 2.D0*SLUN
      WWAVE(23) = 2.D0*WSLUN
      UWAVE(23) = -2.D0*XI
! MK3      TAB 2A
      VWAVE(24) = 3.D0*TT-2.D0*SLUN+3.D0*HSOL-PI/2.D0
      WWAVE(24) = 3.D0*WTT-2.D0*WSLUN+3.D0*WHSOL
      UWAVE(24) = 2.D0*XI-2.D0*NU-NUPRIM
! MK4      TAB 2A
      VWAVE(25) = 4.D0*TT-2.D0*SLUN+4.D0*HSOL
      WWAVE(25) = 4.D0*WTT-2.D0*WSLUN+4.D0*WHSOL
      UWAVE(25) = 2.D0*XI-2.D0*NU-2.D0*NUSEC
! MKS2     M2+K2-S2
      VWAVE(26) = 2.D0*TT-2.D0*SLUN+4.D0*HSOL
      WWAVE(26) = 2.D0*WTT-2.D0*WSLUN+4.D0*WHSOL
      UWAVE(26) = 2.D0*XI-2.D0*NU-2.D0*NUSEC
! MM       A2
      VWAVE(27) = SLUN-PLUN
      WWAVE(27) = WSLUN-WPLUN
      UWAVE(27) = 0.D0
! MN4      TAB 2A
      VWAVE(28) = 4.D0*TT-5.D0*SLUN+4.D0*HSOL+PLUN
      WWAVE(28) = 4.D0*WTT-5.D0*WSLUN+4.D0*WHSOL+WPLUN
      UWAVE(28) = 4.D0*XI-4.D0*NU
! MO3      M2+O1
      VWAVE(29) = 3.D0*TT-4.D0*SLUN+3.D0*HSOL+PI/2.D0
      WWAVE(29) = 3.D0*WTT-4.D0*WSLUN+3.D0*WHSOL
      UWAVE(29) = 4.D0*XI-3.D0*NU
! MP1      A29, NOT M2-P1 (M2-P1 ONLY FOR VWAVE AND WWAVE)
      VWAVE(30) = TT-2.D0*SLUN+3.D0*HSOL-PI/2.D0
      WWAVE(30) = WTT-2.D0*WSLUN+3.D0*WHSOL
      UWAVE(30) = -NU
! MS4      TAB 2A
      VWAVE(31) = 4.D0*TT-2.D0*SLUN+2.D0*HSOL
      WWAVE(31) = 4.D0*WTT-2.D0*WSLUN+2.D0*WHSOL
      UWAVE(31) = 2.D0*XI-2.D0*NU
! MSF      A5
      VWAVE(32) = 2.D0*SLUN-2.D0*HSOL
      WWAVE(32) = 2.D0*WSLUN-2.D0*WHSOL
      UWAVE(32) = 0.D0
! MSK6     M2+S2+K2
      VWAVE(33) = 6.D0*TT-2.D0*SLUN+4.D0*HSOL
      WWAVE(33) = 6.D0*WTT-2.D0*WSLUN+4.D0*WHSOL
      UWAVE(33) = 2.D0*XI-2.D0*NU-2.D0*NUSEC
! MSN2     M2+S2-N2
      VWAVE(34) = 2.D0*TT+SLUN-PLUN
      WWAVE(34) = 2.D0*WTT+WSLUN-WPLUN
      UWAVE(34) = 0.D0
! MSN6     TAB 2A
      VWAVE(35) = 6.D0*TT-5.D0*SLUN+4.D0*HSOL+PLUN
      WWAVE(35) = 6.D0*WTT-5.D0*WSLUN+4.D0*WHSOL+WPLUN
      UWAVE(35) = 4.D0*XI-4.D0*NU
! MSQM     A12
      VWAVE(36) = 4.D0*SLUN-2.D0*HSOL
      WWAVE(36) = 4.D0*WSLUN-2.D0*WHSOL
      UWAVE(36) = -2.D0*XI
! MTM      A7
      VWAVE(37) = 3.D0*SLUN-PLUN
      WWAVE(37) = 3.D0*WSLUN-WPLUN
      UWAVE(37) = -2.D0*XI
! MU2      A45
      VWAVE(38) = 2.D0*TT-4.D0*SLUN+4.D0*HSOL
      WWAVE(38) = 2.D0*WTT-4.D0*WSLUN+4.D0*WHSOL
      UWAVE(38) = 2.D0*XI-2.D0*NU
! N2       A40
      VWAVE(39) = 2.D0*TT-3.D0*SLUN+2.D0*HSOL+PLUN
      WWAVE(39) = 2.D0*WTT-3.D0*WSLUN+2.D0*WHSOL+WPLUN
      UWAVE(39) = 2.D0*XI-2.D0*NU
! N4       N2+N2
      VWAVE(40) = 4.D0*TT-6.D0*SLUN+4.D0*HSOL+2.D0*PLUN
      WWAVE(40) = 4.D0*WTT-6.D0*WSLUN+4.D0*WHSOL+2.D0*WPLUN
      UWAVE(40) = 4.D0*XI-4.D0*NU
! NU2      A43
      VWAVE(41) = 2.D0*TT-3.D0*SLUN+4.D0*HSOL-PLUN
      WWAVE(41) = 2.D0*WTT-3.D0*WSLUN+4.D0*WHSOL-WPLUN
      UWAVE(41) = 2.D0*XI-2.D0*NU
! O1       A14
      VWAVE(42) = TT-2.D0*SLUN+HSOL+PI/2.D0
      WWAVE(42) = WTT-2.D0*WSLUN+WHSOL
      UWAVE(42) = 2.D0*XI-NU
! OO1      A31
      VWAVE(43) = TT+2.D0*SLUN+HSOL-PI/2.D0
      WWAVE(43) = WTT+2.D0*WSLUN+WHSOL
      UWAVE(43) = -2.D0*XI-NU
! P1       B14
      VWAVE(44) = TT-HSOL+PI/2.D0
      WWAVE(44) = WTT-WHSOL
      UWAVE(44) = 0.D0
! PI1      B15
      VWAVE(45) = TT-2.D0*HSOL+PSOL+PI/2.D0
      WWAVE(45) = WTT-2.D0*WHSOL+WPSOL
      UWAVE(45) = 0.D0
! Q1       A15
      VWAVE(46) = TT-3.D0*SLUN+HSOL+PLUN+PI/2.D0
      WWAVE(46) = WTT-3.D0*WSLUN+WHSOL+WPLUN
      UWAVE(46) = 2.D0*XI-NU
! R2       B41
      VWAVE(47) = 2.D0*TT+HSOL-PSOL+PI
      WWAVE(47) = 2.D0*WTT+WHSOL-WPSOL
      UWAVE(47) = 0.D0
! RHO1     A18
      VWAVE(48) = TT-3.D0*SLUN+3.D0*HSOL-PLUN+PI/2.D0
      WWAVE(48) = WTT-3.D0*WSLUN+3.D0*WHSOL-WPLUN
      UWAVE(48) = 2.D0*XI-NU
! S1       B71
      VWAVE(49) = TT
      WWAVE(49) = WTT
      UWAVE(49) = 0.D0
! S2       B39
      VWAVE(50) = 2.D0*TT
      WWAVE(50) = 2.D0*WTT
      UWAVE(50) = 0.D0
! S4       TAB 2A
      VWAVE(51) = 4.D0*TT
      WWAVE(51) = 4.D0*WTT
      UWAVE(51) = 0.D0
! SA       B64
      VWAVE(52) = HSOL
      WWAVE(52) = WHSOL
      UWAVE(52) = 0.D0
! SIGMA1   A20
      VWAVE(53) = TT-4.D0*SLUN+3.D0*HSOL+PI/2.D0
      WWAVE(53) = WTT-4.D0*WSLUN+3.D0*WHSOL
      UWAVE(53) = 2.D0*XI-NU
! SK4      S2+K2
      VWAVE(54) = 4.D0*TT+2.D0*HSOL
      WWAVE(54) = 4.D0*WTT+2.D0*WHSOL
      UWAVE(54) = -2.D0*NUSEC
! SN4      S2+N2
      VWAVE(55) = 4.D0*TT-3.D0*SLUN+2.D0*HSOL+PLUN
      WWAVE(55) = 4.D0*WTT-3.D0*WSLUN+2.D0*WHSOL+WPLUN
      UWAVE(55) = 2.D0*XI-2.D0*NU
! SSA      B6
      VWAVE(56) = 2.D0*HSOL
      WWAVE(56) = 2.D0*WHSOL
      UWAVE(56) = 0.D0
! T2       B40
      VWAVE(57) = 2.D0*TT-HSOL+PSOL
      WWAVE(57) = 2.D0*WTT-WHSOL+WPSOL
      UWAVE(57) = 0.D0
! PHI1     B31
      VWAVE(58) = TT+3.D0*HSOL-PI/2.D0
      WWAVE(58) = WTT+3.D0*WHSOL
      UWAVE(58) = 0.D0
! KI1      A27
      VWAVE(59) = TT-SLUN+3.D0*HSOL-PLUN-PI/2.D0
      WWAVE(59) = WTT-WSLUN+3.D0*WHSOL-WPLUN
      UWAVE(59) = -NU
! PSI1     B24
      VWAVE(60) = TT+2.D0*HSOL-PSOL-PI/2.D0
      WWAVE(60) = WTT+2.D0*WHSOL-WPSOL
      UWAVE(60) = 0.D0
! Z0
      VWAVE(61) = 0.D0
      WWAVE(61) = 0.D0
      UWAVE(61) = 0.D0
!
      DO I=1,61
        UPVWAVE(I) = UWAVE(I) + VWAVE(I)
        UPVWAVE(I) = MOD(UPVWAVE(I),TWOPI)
        IF(UPVWAVE(I).LT.0.D0) UPVWAVE(I) = UPVWAVE(I) + TWOPI
        WWAVE(I)   = WWAVE(I)*DTR/3.15576D9
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
