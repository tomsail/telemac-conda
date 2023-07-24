!                   ********************
                    SUBROUTINE TELEMAC3D
!                   ********************
!
     &(PASS, NIT_ORI)
!
!***********************************************************************
! TELEMAC3D   V8P4
!***********************************************************************
!
!brief
!
!history  JACEK A. JANKOWSKI PINXIT
!+        01/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)
!+        05/05/2010
!+        V6P0
!+   K-OMEGA MODEL BY HOLGER WEILBEER (ISEB/UHA)
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
!history  J-M HERVOUET (LNHE)
!+        02/08/2011
!+        V6P1
!+   CALL MITTIT(18,AT,LT) changed into CALL MITTIT(19,AT,LT)
!+   CALL MITTIT(19,AT,LT) changed into CALL MITTIT(20,AT,LT)
!+   2 fractional steps were not correctly labelled in the listing
!
!history  J-M HERVOUET (LNHE)
!+        12/08/2011
!+        V6P2
!+   Calls to CHECK and BIL3D changed
!
!history  J-M HERVOUET (LNHE)
!+        02/04/2012
!+        V6P2
!+   Clean restart implemented.
!
!history  J-M HERVOUET (LNHE)
!+        01/06/2012
!+        V6P2
!+   Call to vector before call to Tel4del corrected (GRAZCO)
!+   Initialisation of TAN after call to condim.
!
!history  J-M HERVOUET (LNHE)
!+        18/129/2012
!+        V6P3
!+   Call to IFAB3DT added, arguments of cstkep removed.
!
!history  J-M HERVOUET (LNHE)
!+        25/01/2013
!+        V6P3
!+   TAN renamed TRN, copy of TRN on TA moved from after CONDIM to
!+   after BIEF_SUITE, FLULIM set to 1 before first call to PREADV
!
!history  J-M HERVOUET (LNHE)
!+        11/03/2013
!+        V6P3
!+   Call to METEO modified. Stop if variables not found for a 2D
!+   continuation.
!
!history  R. KOPMANN (EDF R&D, LNHE)
!+        16/04/2013
!+        V6P3
!+   Adding the file format in calls to FIND_IN_SEL.
!
!history  J-M HERVOUET (LNHE)
!+        18/03/2013
!+        V6P3
!+   Dealing with the newly created FILE FOR 2D CONTINUATION.
!
!history  J-M HERVOUET (LNHE)
!+        25/04/2013
!+        V6P3
!+   AKN and EPN initialised in case of computation continued, for the
!+   first call to PREADV.
!+   Mesh better updated in case of coupling with Sisyphe.
!
!history  J-M HERVOUET (LNHE)
!+        20/09/2013
!+        V6P3
!+   CALL PLANE_BOTTOM added at the beginning of time loop (otherwise
!+   when calling kepcl3 IPBOT is done with ZPROP at the first iteration
!+   and with Z for the others, while ZPROP is always sent as argument.
!+   This could trigger unexpected divisions by 0.
!
!history  J-M HERVOUET (LNHE)
!+        15/11/2013
!+        V6P3
!+   After second call to bief_suite, checking that Z has been found,
!+   otherwise stop
!
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  J-M HERVOUET (LNHE)
!+        14/03/2014
!+        V7P0
!+   CALL BIL3D put out of the IF(S3D_SEDI) test. Address of depth-averaged
!+   tracers from 38 to 37+NTRAC in ALIRE2D.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Boundary segments have now their own numbering, independent of
!+   boundary points numbering. Differents calls changed accordingly.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        02/05/2014
!+        V7P0
!+   Argument ZR added to FONVAS. S3D_HDEPupdated differently after calling
!+   Sisyphe, to avoid truncation errors that would give S3D_HDEP<0.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        31/07/2014
!+        V7P0
!+   Call to METEO moved just before the first call to FSGRAD, not just
!+   after (atmospheric pressure gradients now systematically added
!+   to free surface gradients).
!
!history  C VILLARET (HRW+EDF) & J-M HERVOUET (EDF - LNHE)
!+        18/09/2014
!+        V7P0
!+   Calls to sisyphe and wac chenged.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!history  R. ATA (EDF LAB, LNHE)
!+        05/11/2014
!+        V7P0
!+   add optional variables to meteo in a sake of harmonization
!+   with telemac-2d
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        31/03/2015
!+        V7P1
!+   Just a few extra debugger prints, up to CALL KEPINI, where was the
!+   last user bug I looked for.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        26/06/2015
!+        V7P1
!+   2D and 3D RESULT FILE can be optional. Tests for writing them added.
!
!history  A. JOLY (EDF LAB, LNHE)
!+        27/08/2015
!+        V7P1
!+   Imposed flowrates on the bed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        21/01/2016
!+        V7P1
!+   Initial conditions of results file in restart mode were forgotten
!+   they are useless but now requested by the Hermes module...
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        08/02/2016
!+        V7P2
!+   Adding the argument HPROP in the call to Sisyphe.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        24/03/2016
!+        V7P2
!+   Adapting to new CVDF3D, saving Z at time T(n) in ZN.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/05/2016
!+        V7P2
!+   Allowing k-epsilon model on a direction and not on the other.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/05/2016
!+        V7P2
!+   In a continued computation, K, Epsilon and the dynamic pressure
!+   must not be read if they are not necessary because their arrays
!+   are not allocated.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        22/08/2016
!+        V7P2
!+   Adding TB2 in the call to CVDF3D.
!
!history  M.JODEAU (EDF LAB, LNHE)
!+        08/2016
!+        V7P3
!+   Water quality: AED2 coupling
!
!history  R. ATA (EDF LAB, LNHE)
!+        12/01/2017
!+        V7P3
!+   Bug fix in the call of DERIVE: now zchar, nplan, transf are well
!    introduced
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        11/09/2017
!+        V7P3
!+   Adding NELMAX2 in list of arguments of ifab3d, ifab3dt, TBORD,
!+   flux_ef_vf_3d and make_zconv.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE METEO_TELEMAC !, ONLY: WINDX,WINDY,PATMOS,TAIR
      USE DECLARATIONS_WAQTEL, ONLY: WAQPROCESS,IND_SF,IND_CSF,IND_SS,
     &                               IND_CSS,IND_CSS2,IND_CSF2,
     &                               KIN_MICROPOL
      USE OILSPILL
      USE INTERFACE_TELEMAC3D, EX_TELEMAC3D => TELEMAC3D
      USE INTERFACE_TELEMAC2D
      USE INTERFACE_SISYPHE, ONLY: SISYPHE
      USE INTERFACE_GAIA, ONLY: GAIA_STEP
      USE INTERFACE_TOMAWAC, ONLY: WAC
      USE TEL4DEL, ONLY: TEL4DELWAQ
      USE DECLARATIONS_GAIA, ONLY: NSICLA,FLUDP,FLUDPT,FLUER,NSUSP_TEL,
     &                       XMVS0,NUM_ISUSP_ICLA,SETDEP
      USE GOTM_COUPLING
!
      USE DECLARATIONS_SPECIAL
      USE COUPLE_MOD
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
! DECLARES LOCAL VARIABLES FOR TELEMAC3D
!-----------------------------------------------------------------------
!
      INTEGER,          INTENT(IN) :: PASS
      INTEGER,          INTENT(IN) :: NIT_ORI
!
!-----------------------------------------------------------------------
!
      INTEGER ITRAC,ISOUSI,IPOIN,IERR
      INTEGER SCHDVI_HOR,SCHDVI_VER,SCHCVI_HOR,SCHCVI_VER
      INTEGER IBID,I,K,I3D,IP
!
!
      DOUBLE PRECISION TETADIVER
      DOUBLE PRECISION UMIN,  UMAX,  SIGMAU, VMIN,  VMAX, SIGMAV
      DOUBLE PRECISION WMIN,  WMAX,  SIGMAW
      DOUBLE PRECISION TAMIN, TAMAX, SIGMTA,TETATRA
!
      LOGICAL CLUMIN, CLUMAX, CLVMIN, CLVMAX, CLWMIN, CLWMAX
      LOGICAL CTAMIN, CTAMAX, YASEM3D,YAS0U,YAS1U
      LOGICAL CLKMIN, CLKMAX, CLEMIN, CLEMAX, CLNUMIN,CLNUMAX
      LOGICAL YAWCHU,NEWDIF,LBID,LBID2,BC
!
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC3D               '
      CHARACTER(LEN=16) FORMUL
!
      INTRINSIC MOD
!
      TYPE(SLVCFG) :: SLVD
!
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
      DOUBLE PRECISION, POINTER, DIMENSION(:,:) :: FLUXBIL
      TYPE(BIEF_OBJ), POINTER :: MULTI1, MULTI2, MULTI3, MULTI4
      DOUBLE PRECISION, PARAMETER :: RTD = 45.D0/ATAN(1.D0)
!
!=======================================================================
!
!
      ISUSP=0
      IF(.NOT.INCLUS(COUPLING,'GAIA  ').AND.PASS.LE.0) THEN
        SETDEP=-1
        NSUSP_TEL=0
        NUM_ISUSP_ICLA(1)=0
      ENDIF
!
#if defined COMPAD
      CALL AD_TELEMAC3D_BEGIN
#endif
!
!     READS TRACERS IN PREVIOUS FILES
!
      IF(NTRAC.GT.0) THEN
        DO I=ADR_TRAC,ADR_TRAC+NTRAC-1
          ALIRE3D(I)=1
        ENDDO
        DO I=ADR_TRAC_2D,ADR_TRAC_2D+NTRAC-1
          ALIRE2D(I)=1
        ENDDO
!
!       THIS IS S3D_ESOMT...
!       IF(S3D_SEDI) ALIRE2D(37)=1
!
      ENDIF
!
!     DO NOT READ K AND EPSILON IF NOT NECESSARY
!
      IF(ITURBH.NE.3.AND.ITURBV.NE.3.AND.ITURBV.NE.6.AND.
     &   ITURBH.NE.7.AND.ITURBV.NE.7) THEN
        ALIRE3D(8)=0
        ALIRE3D(9)=0
      ENDIF
!
!     DO NOT READ DYNAMIC PRESSURE IF NOT NECESSARY
!
      IF(.NOT.NONHYD) ALIRE3D(12)=0
!
!=======================================================================
! FOR DROGUES (CALLS TO FLOT3D WILL INCREASE OR DECREASE NFLOT)
!=======================================================================
!
      NFLOT=0
!
!=======================================================================
! FOR COMPUTING FLUXES OF ADVECTED VARIABLES
!=======================================================================
!
!     NO FLUX COMPUTED FOR U,V,W,K,EPSILON
      DO I=1,5
        CALCFLU(I)=.FALSE.
      ENDDO
!     DEPENDING ON BILMAS FOR TRACERS
      IF(NTRAC.GT.0) THEN
        DO I=6,5+NTRAC
          CALCFLU(I)=BILMAS
        ENDDO
      ENDIF
!
!=======================================================================
! FOR TAKING INTO ACCOUNT RAIN IN ADVECTION OF VARIOUS VARIABLES
!=======================================================================
!
!     NO RAIN FOR U,V,W,K,EPSILON
      DO I=1,5
        CALCRAIN(I)=.FALSE.
      ENDDO
!     DEPENDING OF RAIN FOR TRACERS
      IF(NTRAC.GT.0) THEN
        DO I=6,5+NTRAC
          CALCRAIN(I)=RAIN
        ENDDO
      ENDIF
!
!=======================================================================
! INITIALISATION: READS, PREPARES AND CHECKS
!=======================================================================
!
      IF(PASS.EQ.0) THEN
        WRITE(LU,*) 'INITIALISING TELEMAC3D FOR ',CODE1
        WRITE(LU,*) 'INITIALISING TELEMAC3D'
      ELSEIF(PASS.EQ.1) THEN
        GO TO 700
      ELSEIF(PASS.NE.-1) THEN
        WRITE(LU,*) 'WRONG ARGUMENT PASS: ',PASS
        CALL PLANTE(1)
        STOP
      ENDIF

      CALL TELEMAC3D_INIT
!
!=======================================================================
! THE TIME LOOP BEGINS HERE
!=======================================================================
!
      IF(PASS.EQ.0) THEN
        WRITE(LU,*) 'TELEMAC3D INITIALISED'
        RETURN
      ENDIF
!
700   CONTINUE
!
      LT = LT+1

      AT = AT + DT
!
#if defined COMPAD
      CALL AD_TELEMAC3D_TIMESTEP_BEGIN
#endif
!
!     SAVING ORIGINAL ELEVATIONS (FOR DISTRIBUTIVE SCHEMES)
!
      CALL OS('X=Y     ',X=ZN,Y=Z3)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'BOUCLE EN TEMPS LT=',LT
      INFOGR = .FALSE.
      IF (MOD(LT,LISPRD) == 0) INFOGR = .TRUE.
      INFOGR = LISTIN .AND. INFOGR
      IF (INFOGR) CALL MITTIT(1,AT,LT)
!
!=======================================================================
!
!     IPBOT HAS BEEN MODIFIED FOR CVDF3D IN THE PREVIOUS TIME STEP,
!     IT IS RESTORED HERE WITH ZPROP
!     NOTE: DIFFERENT IPBOT_Z AND IPBOT_ZPROP WOULD BE CLEARER....
      IF(LT.GT.1) THEN
        CALL PLANE_BOTTOM(IPBOT%I,ZPROP%R,NPOIN2,NPLAN,SIGMAG,OPTBAN)
      ENDIF
!
!=======================================================================
! SOURCES : COMPUTES INPUTS WHEN VARYING IN TIME
!           IF NO VARIATION IN TIME QSCE2=QSCE AND TASCE2=TASCE
!=======================================================================
!
      IF(NPTSCE.GT.0) THEN
        DO I=1,NPTSCE
          QSCE2(I)=T3D_DEBSCE(AT,I,QSCE)
        ENDDO
        IF(NTRAC.GT.0) THEN
          DO I=1,NPTSCE
            DO ITRAC=1,NTRAC
              TA_SCE%ADR(ITRAC)%P%R(I)=T3D_TRSCE(AT,I,ITRAC)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
      IF(NBUSE.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BUSE'
        CALL BUSE(RELAXB,NBUSE,ENTBUS,SORBUS,GRAV,
     &            H%R,ZF%R,DBUS%R,LRGBUS%R,HAUBUS%R,CLPBUS%I,
     &            ALTBUS%R,CSBUS%R,CEBUS%R,ANGBUS%R,LBUS%R,
     &            NTRAC,TA,TBUS,UBUS,VBUS,U%R,V%R,INFOGR,
     &            CV%R,C56%R,CV5%R,C5%R,CTRASH%R,FRICBUS%R,
     &            LONGBUS%R,CIRC%I,DELBUS%R,OPTBUSE,V2DPAR,DT,
     &            SECBUS%R,MAXSCE,NPTSCE,NPOIN2,KSCE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BUSE'
        DO I=1,NBUSE
          QSCE2(NPTSCE+I)      =-DBUS%R(I)
          QSCE2(NPTSCE+NBUSE+I)= DBUS%R(I)
        ENDDO
        IF(NTRAC.GT.0) THEN
          DO I=1,NBUSE
            DO ITRAC=1,NTRAC
              TA_SCE%ADR(ITRAC)%P%R(NPTSCE+I) = TBUS%ADR(ITRAC)%P%R(I)
              TA_SCE%ADR(ITRAC)%P%R(NPTSCE+NBUSE+I)=
     &                                      TBUS%ADR(ITRAC)%P%R(NBUSE+I)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
! FORCING AT THE BOUNDARIES
!
! METEOROLOGICAL CONDITIONS
!
      IF (VENT.OR.ATMOS.OR.INCLUS(COUPLING,'WAQTEL')) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE METEO'
        CALL METEO(PATMOS%R,WINDX%R,WINDY%R,CST_WINDX,CST_WINDY,
     &             AT,LT,NPOIN2,VENT,ATMOS,
     &             T3ATMA,T3ATMB,T3D_FILES,
     &             LISTIN,CST_PATMOS,INCLUS(COUPLING,'WAQTEL'),PLUIE,
     &             OPTWIND)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE METEO'
!       RAIN TEMPERATURE EQUAL TO THE AIR TEMPERATURE OR 1.D0
!       IS BETTER THAN 0.D0
        IF(IND_T.NE.0.AND.INCLUS(COUPLING,'WAQTEL')) THEN
          TRAIN(IND_T) = MAX(TAIR%R(1),1.D0)
        ENDIF
        IF( (INCLUS(COUPLING,'WAQTEL') ).AND.
     &      (13*INT(WAQPROCESS/13).EQ.WAQPROCESS) ) THEN
          DO I=1,NPOIN2
            WINDSPD%R(I) = SQRT(WINDX%R(I)**2+WINDY%R(I)**2)
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! END OF CAMILLE LEQUETTE'S MODIFICATIONS
!=======================================================================
!
!     COUPLING WITH TOMAWAC
!
      IF(INCLUS(COUPLING,'TOMAWAC').AND.
     &   PERCOU_WAC*((LT-1)/PERCOU_WAC).EQ.LT-1) THEN
!
        CALL CONFIG_CODE(3)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TOMAWAC'
        IF(INCLUS(COUPLING,'TOMAWAC2').OR.
     &     INCLUS(COUPLING,'TOMAWACT3D2')) THEN
          CALL SEND_COUPLE(1,NPOIN2,NVARTEL2TOM,TEL2TOM)
        ENDIF
        CALL T3D_WAC_CPL_UPDATE(NIT_ORI)
        CALL WAC(PART=1)
        IF(INCLUS(COUPLING,'TOMAWAC2').OR.
     &     INCLUS(COUPLING,'TOMAWACT3D2')) THEN
          CALL GET_MESH_NPTIR(T3D_FILES(T3DGEO)%FMT,
     &       T3D_FILES(T3DGEO)%LU,NPTIR,IERR)
          CALL RECEIVE_COUPLE(2,NPOIN2,NVARTOM2TEL,TOM2TEL)
          ! CONVERT DIRECTIONS BACK
          DO I=1,NPOIN2
            IF((ABS(SINDIR%R(I)).GE.1.D-14).OR.
     &         (ABS(COSDIR%R(I)).GE.1.D-14)) THEN
              DIRMOY%R(I) = RTD * ATAN2(SINDIR%R(I),COSDIR%R(I))
            ELSE
!     POINT HAS NO INFORMATION
              DIRMOY%R(I) = 0.D0
            ENDIF
          ENDDO
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TOMAWAC'
        CALL CONFIG_CODE(1)
!
      ENDIF
!
!=======================================================================
!
! SAVES H, TA, TP, AK, EP
! IN    HN,TRN,TPN,AKN,EPN
!
      CALL OS ( 'X=Y     ', X=HN,    Y=H     )
      CALL OS ( 'X=Y     ', X=VOLUN, Y=VOLU  )
      IF(NCSIZE.GT.1) CALL OS('X=Y     ',X=VOLUNPAR,Y=VOLUPAR)
      CALL OS ( 'X=Y     ', X=UN,    Y=U     )
      CALL OS ( 'X=Y     ', X=VN,    Y=V     )
      IF(NONHYD) CALL OS ( 'X=Y     ' , X=WN, Y=W)
      CALL OS ( 'X=Y     ', X=GRADZN,Y=GRADZS)
!     TRACERS (IF LT=1 DONE AFTER CALL CONDIM AND READ_DATASET)
      IF(NTRAC.GT.0.AND.LT.GT.1) CALL OS ('X=Y     ', X=TRN, Y=TA)
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3.OR.ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
        CALL OS ( 'X=Y     ', X=AKN, Y=AK )
        CALL OS ( 'X=Y     ', X=EPN, Y=EP )
      ENDIF
      IF(ITURBV.EQ.5.OR.ITURBV.EQ.9) THEN
        CALL OS( 'X=Y      ', X=NUN, Y=NU )
      ENDIF
!
      IF(BILMAS) THEN
        MASSEN_WATER = MASSE_WATER
        CALL OS ( 'X=Y     ', X=MASSEN, Y=MASSE )
      ENDIF
!
! COMPUTES MEAN UN AND VN IN THE VERTICAL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VERMOY'
      CALL VERMOY(UN2D%R,VN2D%R,UN%R,VN%R,2,Z,
     &            T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VERMOY'
!
! INTEGRATES USTOKES AND VSTOKES OVER DEPTH
!
      IF(INCLUS(COUPLING,'TOMAWACT3D')) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VERMOY POUR VIT STOKES'
          CALL VERMOY(US2D%R,VS2D%R,USTOKES%R,VSTOKES%R,2,Z,
     &                T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,
     &                OPTBAN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VERMOY POUR VIT STOKES'
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES FRICTION COEFFICIENT
!
!     TIME VARIATIONS OF RUGOF (CORSTR IS IN TELEMAC-2D LIBRARY)
!     MUST BE USER-IMPLEMENTED - NOTHING DONE IN STANDARD
      CALL CORSTR
!
      IF(.NOT.INCLUS(COUPLING,'TOMAWACT3D')) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE COEFRO'
        CALL COEFRO(CF,H,UN2D,VN2D,KARMAN,KFROT,RUGOF,GRAV,MESH2D,T2_01,
     &              .FALSE.)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE COEFRO'
      ELSE
!       FRICTION COEFFICIENT MODIFIED TO TAKE INTO ACCOUNT WAVES+CURRENTS
        DO IP=1,NPOIN2
          CF%R(IP) = CFWC%R(IP)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
! CHECKS AND HARMONISES THE BOUNDARY CONDITION TYPES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LICHEK'
      CALL LICHEK(LIMPRO%I,NPTFR2,
     &             MESH2D%IKLBOR%I,MESH2D%NELEB,MESH2D%NELEBX)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LICHEK'
!
! BOUNDARY CONDITIONS FOR THE K-EPSILON MODEL
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3.OR.ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
        CALL KEPICL(LIKBOF%I,LIEBOF%I,LIUBOF%I,
     &              LIKBOL%I,LIEBOL%I,LIUBOL%I,
     &              LIKBOS%I,LIEBOS%I,
     &              NPTFR2,NPLAN,NPOIN2,KENT,KSORT,KENTU)
      ENDIF
!
! BOUNDARY CONDITIONS FOR THE S-A MODEL
!
      IF(ITURBV.EQ.5.OR.ITURBV.EQ.9) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SAPICL'
        CALL SAPICL(LINUBOF%I, LIUBOF%I,
     &              LINUBOL%I, LIUBOL%I,
     &              LINUBOS%I,
     &              NPTFR2, NPLAN, NPOIN2, KENT, KSORT)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SAPICL'
      ENDIF
!
!-----------------------------------------------------------------------
!
!     SEDIMENT
!
      IF(S3D_SEDI) THEN
!
!       COMPUTES THE SEDIMENT SETTLING VELOCITY
!
        CALL VITCHU(S3D_WCHU,S3D_WCHU0,U,V,
     &  S3D_TURBA,S3D_TURBB,T3_01,T3_02,T3_03,SVIDE,MESH3D,IELM3,
     &  NPOIN2,NPOIN3,
     &  NPLAN,NTRAC,MSK,MASKEL,UETCAR,TA,HN,S3D_FLOC,
     &  S3D_FLOC_TYPE,
     &  S3D_HINDER,S3D_HIND_TYPE,S3D_CGEL,S3D_CINI)
!
!       BOUNDARY CONDITIONS FOR THE SEDIMENTOLOGY
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CLSEDI'
!GA: CLSEDI HAS BEEN MODIFIED TO ALLOW TWO SEDIMENT CLASSES
        IF(S3D_MIXTE) THEN

          DO ITRAC = NTRAC-1,NTRAC
            CALL CLSEDI
     &       (ATABOF%ADR(ITRAC)%P%R,BTABOF%ADR(ITRAC)%P%R,
     &        S3D_WCHU%R,
     &        Z, H, DELTAR%R, T3_01, T3_02%R,
     &        S3D_EPAI, S3D_CFDEP,S3D_CONC, S3D_HDEP%R, S3D_FLUER%R,
     &        S3D_FLUDPT%R, LITABF%ADR(ITRAC)%P%I,
     &        KLOG, NPOIN3, NPOIN2, NPLAN, S3D_NCOUCH,
     &        DT, RHO0, S3D_RHOS,
     &        S3D_TOCD,S3D_MPART,S3D_TOCE,UETCAR%R,
     &        GRAV,S3D_SEDCO,S3D_DMOY,S3D_CREF,ZREF,CF,S3D_AC,
     &        S3D_KSPRATIO,S3D_ICR,S3D_ICQ,
     &        RUGOF,S3D_SETDEP,S3D_HSED,
     &        S3D_WCS%R, S3D_EPAICO%R, S3D_EPAINCO%R, S3D_MIXTE,
     &        S3D_SEDNCO, S3D_FLUDPTC%R, S3D_FLUDPTNC%R, S3D_FLUERC%R,
     &        S3D_FLUERNC%R, NTRAC, ITRAC)
!           ATABOF AND BTABOF ARE NO LONGER 0 FOLLOWING CLSEDI
            ATABOF%ADR(ITRAC)%P%TYPR='Q'
            BTABOF%ADR(ITRAC)%P%TYPR='Q'
          ENDDO

        ELSEIF (S3D_SEDCO.OR.S3D_SEDNCO) THEN

        CALL CLSEDI
     &   (ATABOF%ADR(NTRAC)%P%R,BTABOF%ADR(NTRAC)%P%R,
     &    S3D_WCHU%R,
     &    Z, H, DELTAR%R, T3_01, T3_02%R,
     &    S3D_EPAI, S3D_CFDEP,S3D_CONC, S3D_HDEP%R, S3D_FLUER%R,
     &    S3D_FLUDPT%R, LITABF%ADR(NTRAC)%P%I,
     &    KLOG, NPOIN3, NPOIN2, NPLAN, S3D_NCOUCH,
     &    DT, RHO0, S3D_RHOS,
     &    S3D_TOCD,S3D_MPART,S3D_TOCE,UETCAR%R,
     &    GRAV,S3D_SEDCO,S3D_DMOY,S3D_CREF,ZREF,CF,S3D_AC,S3D_KSPRATIO,
     &    S3D_ICR,S3D_ICQ,RUGOF,S3D_SETDEP,S3D_HSED,
     &    S3D_WCS%R, S3D_EPAICO%R, S3D_EPAINCO%R, S3D_MIXTE, S3D_SEDNCO,
     &    S3D_FLUDPTC%R, S3D_FLUDPTNC%R, S3D_FLUERC%R, S3D_FLUERNC%R,
     &    NTRAC, ITRAC)
!
!         ATABOF AND BTABOF ARE NO LONGER 0 FOLLOWING CLSEDI
          ATABOF%ADR(NTRAC)%P%TYPR='Q'
          BTABOF%ADR(NTRAC)%P%TYPR='Q'
!
        ELSE
!
          WRITE(LU,*) ' '
          WRITE(LU,*) 'SEDI3D : ERROR ON SEDIMENT KEY WORD'
          CALL PLANTE(1)
          STOP
!
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CLSEDI'
      ENDIF
!
!     PREPARING BOUNDARY CONDITIONS FOR THOMPSON METHOD
!
      IF(THOMFR.AND.NFRLIQ.GT.0) THEN
!
        CALL CPSTVC(H,T2_01)
        CALL PREBOR(HBOR%R,UBOR2D%R,VBOR2D%R,TABORL,U2D%R,V2D%R,H%R,
     &              T2_01%R,TA,MESH2D%NBOR%I,
     &              MESH2D%NPOIN,MESH2D%NPTFR,
!    &              NTRAC SET TO ZERO PROVISIONALLY
     &              0    ,NFRLIQ,FRTYPE,NUMLIQ%I)
!       RESTORING USER BOUNDARY CONDITIONS BEFORE CALLING BORD3D
!       TO AVOID UNDUE CALLS TO SL3, ETC.
        CALL THOMPS_BC(3)
!
      ENDIF
!
! UPDATES BOUNDARY CONDITION VALUES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BORD3D'
      CALL BORD3D(NFRLIQ)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BORD3D'
!
      IF(THOMFR.AND.NFRLIQ.GT.0) THEN
!
!       NOW THAT BORD3D HAS BEEN CALLED
!       CHANGING AGAIN BOUNDARY CONDITIONS FOR THOMPSON
        CALL THOMPS_BC(2)
!
!       UBORL/VBORL UPDATED IN BORD3D, SO ARE UBOR2D/VBOR2D
        DO I=1,NPTFR2
          UBOR2D%R(I) = UBORL%R(I)
          VBOR2D%R(I) = VBORL%R(I)
        ENDDO
!
      ENDIF
!
! BOUNDARY CONDITIONS FOR THE VELOCITY ON LATERAL BOUNDARIES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TBORD'
      CALL TBORD(AUBORL%R,
     &           RUGOL%R,
     &           MESH2D%DISBOR%R,MESH2D%NELBOR%I,MESH2D%NULONE%I,
     &           MESH2D%IKLE%I,NELMAX2,
     &           U%R,V%R,W%R,
     &           NBOR2%I,NPOIN2,NPLAN,NPTFR2,DNUVIH,DNUVIV,
     &           KARMAN,LISRUL,KFROTL,
     &           UETCAL%R,NONHYD,
     &           T2_02%R,MESH2D)
      IF(KFROTL.EQ.0) THEN
        AUBORL%TYPR='0'
      ELSE
        AUBORL%TYPR='Q'
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TBORD, APPEL DE TFOND'
!
! BOUNDARY CONDITIONS FOR THE VELOCITY ON THE BOTTOM
!
      CALL TFOND(AUBORF%R,
     &           CF%R,UN2D%R,VN2D%R,U%R,V%R,W%R,KARMAN,
     &           LISRUF,DNUVIV,Z,NPOIN2,KFROT,RUGOF%R,UETCAR%R,
     &           NONHYD,OPTBAN,HN%R,GRAV,IPBOT%I,NPLAN)
      AUBORF%TYPR='Q'
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TFOND'
!
! BOUNDARY CONDITIONS FOR K-EPSILON MODEL + COMPUTES CONSTRAINTS
! AT THE BOTTOM AND LATERAL BOUNDARIES IF K-EPSILON IS REQUIRED
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE KEPCL3'
        CALL KEPCL3(KBORF%R,EBORF%R,LIKBOF%I,LIEBOF%I,
     &              KBORL%R,EBORL%R,LIKBOL%I,LIEBOL%I,LIUBOL%I,
     &              KBORS%R,EBORS%R,
     &              LIKBOS%I,LIEBOS%I,
     &              MESH2D%DISBOR%R,AK%R,H%R,ZPROP%R,
     &              NBOR2%I,NPOIN2,NPLAN,NPTFR2,
     &              KARMAN,CMU,
     &              KMIN,EMIN,
     &              KENT,KENTU,KSORT,KADH,KLOG,
     &              UETCAR%R,FICT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE KEPCL3'
!
      ELSEIF(ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE KOMCL3'
        CALL KOMCL3(KBORF%R,EBORF%R,LIKBOF%I,LIEBOF%I,LIUBOF%I,
     &              KBORL%R,EBORL%R,LIKBOL%I,LIEBOL%I,LIUBOL%I,
     &              EBORS%R,LIEBOS%I,
     &              MESH2D%DISBOR%R,AK%R,
     &              U%R,V%R,H%R,ZPROP%R,
     &              NBOR2%I,NPOIN2,NPLAN,NPTFR2,
     &              KARMAN,BETAS,OMSTAR,
     &              KMIN,EMIN,
     &              KENTU,KENT,KADH,KLOG,UETCAR%R,UETCAL%R)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE KOMCL3'
!
      ELSEIF(ITURBV.EQ.5.OR.ITURBV.EQ.9) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SACL3'
        CALL SACL3(NUBORF%R,LINUBOF%I,
     &             NUBORL%R,LINUBOL%I,LIUBOL%I,
     &             H%R,ZPROP%R,
     &             NBOR2%I,NPOIN2,NPLAN,NPTFR2,
     &             KARMAN,UETCAR%R,NUMIN,
     &             KENT,KENTU,KSORT,KADH,KLOG,FICT)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SACL3'
      ENDIF
!
! CLIPS HBOR
!
      IF(OPTBAN.EQ.2) THEN
        CALL CLIP(HBOR,HMIN,.TRUE.,1.D6,.FALSE.,0)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     THOMPSON BOUNDARY CONDITIONS
!
      IF(THOMFR) THEN
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE THOMPS'
!     T2_01 IS HERE A VALUE OF H SAVED BEFORE CALLING BORD3D
      CALL THOMPS(HBOR%R,UBOR2D%R,VBOR2D%R,TABORL,U2D,V2D,T2_01,
     &            TA,ZF,MESH2D%X%R,MESH2D%Y%R,MESH2D%NBOR%I,
     &            FRTYPE,T2_02,T2_03,T2_04,T2_06,T2_07,
     &            LIHBOR%I,LIUBOL%I,LIVBOL%I,IT1%I,
     &            T2_08%R,T2_09%R,W1%R,T2_21,T2_22,T2_23,
     &            TAC,T2_10,MESH2D%SURDET%R,MESH2D%IKLE%I,
     &            MESH2D%IFABOR%I,MESH2D%NELEM,MESH2D,
     &            MESH2D%XNEBOR%R,MESH2D%YNEBOR%R,
!                                                   NTRAC SET TO ZERO
     &            MESH2D%NPOIN,MESH2D%NPTFR,DT,GRAV,0    ,
     &            NFRLIQ,KENT,KENTU,MSK,MASKEL,
     &            MESH2D%NELMAX,11,T2_11%R,NUMLIQ%I,MAT2D%ADR(1)%P%X%R,
     &            T2_12%R,T2_13%R,T2_14%R,IT3,IT4,
     &            T2_15,T2_16,T2_17,T2_18,T2_19,T2_20,T3_01)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE THOMPS'
!
!     DUPLICATING ON THE VERTICAL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE THOMPS_2DTO3D'
      CALL THOMPS_2DTO3D
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE THOMPS_2DTO3D'
!
      ENDIF
!
!-----------------------------------------------------------------------
! SOURCE TERMS
!
      IF(NPTSCE.GT.0) THEN
        CALL FINDKSCE(NPOIN2,NPLAN,Z3%R,NPTSCE,ISCE,ZSCE,KSCE,INFOGR)
      ENDIF
      IF(NBUSE.GT.0) THEN
        CALL FINDKSCE(NPOIN2,NPLAN,Z3%R,NBUSE,ENTBUS,ALTBUS%R,
     &                KENTBUS,INFOGR)
        CALL FINDKSCE(NPOIN2,NPLAN,Z3%R,NBUSE,SORBUS,ALTBUS%R,
     &                KSORBUS,INFOGR)
      ENDIF
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TRISOU'
      CALL TRISOU
     & (S0U%R,S0V%R, S0U,S0V,UN%R,VN%R,X,Y,Z,
     &  T3_01%R, DELTAR, MESH3D, FCOR, CORIOL, NTRAC,
     &  AT, SURFA2%R, T3_02%R, T3_02, W1%R,
     &  MESH3D%M%X%R(1:6*NELEM3),MESH3D%M%X%R(6*NELEM3+1:12*NELEM3),
     &  GRAV, NPOIN3, NELEM3, NPOIN2, NELEM2, NPLAN, NETAGE,
     &  IKLE3%I, LV, MSK, MASKEL%R, INCHYD,
     &  VOLU,SVIDE,IELM3,MASKEL,NREJEU,ISCE,KSCE,QSCE2,
     &  U_SCE%R,V_SCE%R,
     &  GRADZS%ADR(1)%P,GRADZS%ADR(2)%P,MESH2D,
     &  T3_03, T3_03%R, T3_04, T3_04%R, LONGIT,
     &  YASEM3D,SCHCVI,DENLAW,FXH,FYH,COUROU,NPTH,T3D_FILES,T3DBI1)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TRISOU, APPEL DE SOURCE'
!
      CALL SOURCE(S0U, S0V, S0W, S1U, S1V, S1W,
     &            U, V, WS, W,
     &            VOLU, VOLUN,T3_01,
     &            NPOIN3, NTRAC, LT, AT, DT, PRIVE, NONHYD,
     &            NPOIN2, NSCE,ISCE,KSCE,QSCE2,U_SCE%R,V_SCE%R,MAXSCE)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOURCE'
!
!     SAVES BOUNDARY VALUES FOR TIME TN
!
      IF(NSOUSI.GT.1) THEN
        DO IP=1,NPTFR3
          UBORSAVE%R(IP)=UN%R(NBOR3%I(IP))
          VBORSAVE%R(IP)=VN%R(NBOR3%I(IP))
        ENDDO
        IF(NONHYD) THEN
          DO IP=1,NPTFR3
            WBORSAVE%R(IP)=WN%R(NBOR3%I(IP))
          ENDDO
        ENDIF
        IF(ITURBV.EQ.3.OR.ITURBH.EQ.3.OR.
     &     ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
          DO IP=1,NPTFR3
            KBORSAVE%R(IP)=AKN%R(NBOR3%I(IP))
            EBORSAVE%R(IP)=EPN%R(NBOR3%I(IP))
          ENDDO
        ELSEIF(ITURBV.EQ.5.OR.ITURBV.EQ.9) THEN
          DO IP=1, NPTFR3
            NUBORSAVE%R(IP)=NUN%R(NBOR3%I(IP))
          ENDDO
        ENDIF
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            DO IP=1,NPTFR3
              TRBORSAVE%ADR(ITRAC)%P%R(IP)=
     &        TRN%ADR(ITRAC)%P%R(NBOR3%I(IP))
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! THE SUB-ITERATIONS LOOP BEGINS HERE
!=======================================================================
!
      SUBITER: DO ISOUSI = 1,NSOUSI
!
#if defined COMPAD
      CALL AD_TELEMAC3D_SUBITERATION_BEGIN
#endif
!
!     RESTORES BOUNDARY VALUES FOR TIME TN
!
      IF(ISOUSI.GT.1) THEN
        DO IP=1,NPTFR3
          UN%R(NBOR3%I(IP))=UBORSAVE%R(IP)
          VN%R(NBOR3%I(IP))=VBORSAVE%R(IP)
        ENDDO
        IF(NONHYD) THEN
          DO IP=1,NPTFR3
            WN%R(NBOR3%I(IP))=WBORSAVE%R(IP)
          ENDDO
        ENDIF
        IF(ITURBV.EQ.3.OR.ITURBH.EQ.3.OR.
     &     ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
          DO IP=1,NPTFR3
            AKN%R(NBOR3%I(IP))=KBORSAVE%R(IP)
            EPN%R(NBOR3%I(IP))=EBORSAVE%R(IP)
          ENDDO
        ELSEIF(ITURBV.EQ.5.OR.ITURBV.EQ.9) THEN
          DO IP=1,NPTFR3
            NUN%R(NBOR3%I(IP))=NUBORSAVE%R(IP)
          ENDDO
        ENDIF
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            DO IP=1,NPTFR3
              TRN%ADR(ITRAC)%P%R(NBOR3%I(IP))=
     &        TRBORSAVE%ADR(ITRAC)%P%R(IP)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!     BUILDS THE MESH FOR PROPAGATION STEP
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MESH_PROP'
      CALL MESH_PROP(HPROP,HN,H,PROLIN,HAULIN,TETAH,NSOUSI,ZPROP,
     &               IPBOT,NPOIN2,NPLAN,OPTBAN,SIGMAG,OPT_HNEG,
     &               MESH3D,VOLU3D,VOLU3DPAR,
     &               UNSV3D,MASKEL,IELM3)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MESH_PROP'
!
      IF(ISOUSI.GT.1) THEN
!       REBUILDS THE INITIAL MESH
!       NOTE: EVOLUTION OF ZF IS NOT TAKEN INTO ACCOUNT HERE - INVESTIGATE
        CALL CALCOT(Z,HN%R)
        CALL OS('X=Y     ',X=VOLU,Y=VOLUN)
        IF(NCSIZE.GT.1) CALL OS('X=Y     ',X=VOLUPAR,Y=VOLUNPAR)
        CALL GRAD2D(GRADZF%ADR(1)%P,GRADZF%ADR(2)%P,ZPROP,NPLAN,SVIDE,
     &              UNSV2D,T2_02,T2_03,T2_04,
     &              IELM2H,MESH2D,MSK,MASKEL)
        CALL FSGRAD(GRADZS,ZFLATS,Z(NPOIN3-NPOIN2+1:NPOIN3),
     &              ZF,IELM2H,MESH2D,MSK,MASKEL,
     &              UNSV2D,T2_01,NPOIN2,OPTBAN,SVIDE)
      ENDIF
!
!     SOURCES AND SINKS OF WATER
!
!     TEMPORARILY PUTS ZPROP IN MESH3D%Z
      SAVEZ     =>MESH3D%Z%R
      MESH3D%Z%R=>ZPROP%R
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOURCES_SINKS'
      CALL SOURCES_SINKS
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOURCES_SINKS'
!
      IF(INCLUS(COUPLING,'TOMAWACT3D')) THEN
!       ADD THE NEW TERMS ASSOCIATED WITH THE USTOKES AND VSTOKES
!       INTEGRATED OVER DEPTH
        CALL OS ('X=YZ     ',X=DUS2D,Y=US2D,Z=H)
        CALL VECTOR(DUS2DX,'=','GRADF          X',IELMH,1.D0,DUS2D,
     &              SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,.FALSE.,MASKEL,
     &              ASSPAR=.TRUE.)
!
        CALL OS ('X=YZ     ',X=DVS2D,Y=VS2D,Z=H)
        CALL VECTOR(DVS2DY,'=','GRADF          Y',IELMH,1.D0,DVS2D,
     &              SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,.FALSE.,MASKEL,
     &              ASSPAR=.TRUE.)
!
        CALL OS('X=X-Y      ',X=SMH,Y=DUS2DX)
        CALL OS('X=X-Y      ',X=SMH,Y=DVS2DY)
      ENDIF
!     RESTORES Z
      MESH3D%Z%R=>SAVEZ

      IF(BEDBOU)THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'DEUXIEME APPEL DE BED_FLUXES'
        CALL BED_FLUXES
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BED_FLUXES'
      ENDIF
!
!     SETS ADVECTION AND DIFFUSION PARAMETERS TO MONITOR CVDF3D
!     DIFFUSION AND SOURCE TERMS ARE DONE IN WAVE_EQUATION
!     IN CVDF3D (THIS IS DONE IN WAVE_EQUATION)
!
!     DIFFUSION OF U AND V IS DONE IN WAVE_EQUATION
      SCHDVI_HOR = 0
      SCHDVI_VER = SCHDVI
!
      SCHCVI_HOR = SCHCVI
      SCHCVI_VER = SCHCVI
!     ADVECTION IS NOT DONE AT THE FIRST TIME-STEP (THIS WAS VERSION 6.1)
!     IF(LT.EQ.1.AND.ISOUSI.EQ.1) THEN
!       SCHCVI_HOR = 0
!       SCHCVI_VER = 0
!     ENDIF
!
!     WHEN SCHCVI=ADV_SUP DIFF3D IS CALLED AND
!     SOURCE TERMS WOULD BE TREATED TWICE
      YAS0U=.FALSE.
      YAS1U=.FALSE.
!
!-----------------------------------------------------------------------
! ADVECTION-DIFFUSION STEP FOR VELOCITY COMPONENTS
!-----------------------------------------------------------------------
!
!     HERE DIFFUSION IS DONE IN MESH3D%Z, IT IS DIFFERENT FROM ZPROP IF
!     FROM THE SECOND SUB-ITERATION ON. SO IPBOT IS REDONE HERE
!
      IF(ISOUSI.GT.1) THEN
        CALL PLANE_BOTTOM(IPBOT%I,Z,NPOIN2,NPLAN,SIGMAG,OPTBAN)
      ENDIF
!
      IF(INFOGR) THEN
        IF (NONHYD) THEN
          CALL MITTIT(17,AT,LT)
        ELSE
          CALL MITTIT(4,AT,LT)
        ENDIF
      ENDIF
!
      SIGMAU = 1.D0
      UMIN   = 0.D0
      UMAX   = 1.D0
      CLUMIN = .FALSE.
      CLUMAX = .FALSE.
      YAWCHU = .FALSE.
!     YASEM3D = DONE IN TRISOU
      NEWDIF=.TRUE.

      FLUXBIL(1:MAXFRO+NSCE+1,1:5+NTRAC) => FLUXB%R
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR U'
      CALL CVDF3D
     & (UD,UC,UN,VISCVI,SIGMAU,S0U,YAS0U,S1U,YAS1U,
     &  UBORL, UBORF, UBORS, AUBORL, AUBORF, AUBORS,
     &  BUBORL, BUBORF, BUBORS, LIUBOL, LIUBOF, LIUBOS,
     &  FLUXBIL(1:MAXFRO+NSCE+1,1),
     &  FLUX%R(1), FLUEXT,FLUEXTPAR,UMIN, CLUMIN, UMAX, CLUMAX,
     &  SCHCVI_HOR,SCHDVI_HOR,SLVDVI,TRBAVI,INFOGR,NEWDIF,
     &  CALCFLU(1),T2_01,T2_03,
     &  T3_01,T3_02,T3_03,T3_04,MESH3D,IKLE3,MASKEL,MTRA1,
     &  NPTFR3,MMURD,MURD_TF,VOLU3D,VOLU3DPAR,VOLU3D,VOLU3DPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM3,
     &  NPLAN,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SMU,YASEM3D,SVIDE,IT1,
     &  TRAV3,MESH2D,OPTBAN,TETADI,YAWCHU,WCHU,S3D_WCHU,
     &  AGGLOD,NSCE,SOURCES,U_SCE%R,NUMLIQ%I,DIRFLU,NFRLIQ,
     &  VOLUT,ZT,ZPROP,CALCRAIN(1),PLUIE,PARAPLUIE,0.D0,FLODEL,FLOPAR,
     &  SIGMAG,IPBOT%I,MAXADV,FLUDPT,FLUDP,FLUER,VOLU2D,V2DPAR,SETDEP,
     &  S3D_FLUDPT,S3D_FLUDP,S3D_FLUER,S3D_SETDEP,OPTSOU,ZN%R,
     &  OPTADV_VI,NCO_DIST,NSP_DIST,TB2)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR U'
!
      SIGMAV = 1.D0
      VMIN   = 0.D0
      VMAX   = 1.D0
      CLVMIN = .FALSE.
      CLVMAX = .FALSE.
      YAWCHU = .FALSE.
!     YASEM3D = DONE IN TRISOU
!     MDIFF ALREADY COMPUTED FOR U
      NEWDIF=.FALSE.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR V'
!     USE OF AUBORL,AUBORF,AUBORS IS NOT A MISTAKE
      CALL CVDF3D
     & (VD,VC,VN,VISCVI,SIGMAV,S0V,YAS0U,S1V,YAS1U,
     &  VBORL, VBORF, VBORS, AUBORL,AUBORF,AUBORS,
     &  BVBORL, BVBORF, BVBORS, LIVBOL, LIVBOF, LIVBOS,
     &  FLUXBIL(1:MAXFRO+NSCE+1,2),
     &  FLUX%R(2), FLUEXT,FLUEXTPAR,VMIN, CLVMIN, VMAX, CLVMAX,
     &  SCHCVI_HOR,SCHDVI_HOR,SLVDVI,TRBAVI,INFOGR,NEWDIF,
     &  CALCFLU(2),T2_01,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     &  NPTFR3,MMURD,MURD_TF,VOLU3D,VOLU3DPAR,VOLU3D,VOLU3DPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM3,
     &  NPLAN,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SMV,YASEM3D,SVIDE,IT1,
     &  TRAV3,MESH2D,OPTBAN,TETADI,YAWCHU,WCHU,S3D_WCHU,
     &  AGGLOD,NSCE,SOURCES,V_SCE%R,NUMLIQ%I,DIRFLU,NFRLIQ,
     &  VOLUT,ZT,ZPROP,CALCRAIN(2),PLUIE,PARAPLUIE,0.D0,FLODEL,FLOPAR,
     &  SIGMAG,IPBOT%I,MAXADV,FLUDPT,FLUDP,FLUER,VOLU2D,
     &  V2DPAR,SETDEP,S3D_FLUDPT,S3D_FLUDP,S3D_FLUER,S3D_SETDEP,OPTSOU,
     &  ZN%R,OPTADV_VI,NCO_DIST,NSP_DIST,TB2)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR V'
!
      IF(NONHYD) THEN
!
        SIGMAW = 1.D0
        WMIN   = 0.D0
        WMAX   = 1.D0
        CLWMIN = .FALSE.
        CLWMAX = .FALSE.
        YASEM3D= .FALSE.
        YAWCHU = .FALSE.
        NEWDIF=.TRUE.
!       TETADI MAY BE EQUAL TO 2 FOR U AND V, WHEN THE WAVE EQUATION
!       IS USED - NOT DONE ON W SO FAR
        TETADIVER = MIN(TETADI,1.D0)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR W'
!       USE OF AUBORL,AUBORF,AUBORS IS NOT A MISTAKE
        CALL CVDF3D
     & (WD,WC,WN,VISCVI,SIGMAW,S0W,.TRUE.,S1W,.TRUE.,
     &  WBORL, WBORF, WBORS, AUBORL, AUBORF, AUBORS,
     &  BWBORL, BWBORF, BWBORS, LIWBOL, LIWBOF, LIWBOS,
     &  FLUXBIL(1:MAXFRO+NSCE+1,3),
     &  FLUX%R(3), FLUEXT,FLUEXTPAR,WMIN, CLWMIN, WMAX, CLWMAX,
     &  SCHCVI_VER,SCHDVI_VER,SLVDVI,TRBAVI,INFOGR,NEWDIF,
     &  CALCFLU(3),T2_01,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     &  NPTFR3,MMURD,MURD_TF,VOLU3D,VOLU3DPAR,VOLU3D,VOLU3DPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM3,
     &  NPLAN,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,
     &  TRAV3,MESH2D,OPTBAN,
     &  TETADIVER,YAWCHU,WCHU,S3D_WCHU,AGGLOD,NSCE,SOURCES,W_SCE%R,
     &  NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,CALCRAIN(3),PLUIE,
     &  PARAPLUIE,0.D0, FLODEL,FLOPAR,
     &  SIGMAG,IPBOT%I,MAXADV,FLUDPT,FLUDP,FLUER,VOLU2D,
     &  V2DPAR,SETDEP,S3D_FLUDPT,S3D_FLUDP,S3D_FLUER,S3D_SETDEP,OPTSOU,
     &  ZN%R,OPTADV_VI,NCO_DIST,NSP_DIST,TB2)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR W'
      ENDIF
!
!-----------------------------------------------------------------------
! DIFFUSION AND PROPAGATION STEP BY WAVE_EQUATION
!-----------------------------------------------------------------------
!
      IF(INFOGR) THEN
        CALL MITTIT(6,AT,LT)
      ENDIF
!     TEMPORARILY PUTS ZPROP IN MESH3D%Z
      SAVEZ     =>MESH3D%Z%R
!     ALL PROPAGATION WILL BE DONE WITH ZPROP INSTEAD OF Z
      MESH3D%Z%R=>ZPROP%R
!     IPBOT HAS BEEN MODIFIED FOR CVDF3D, IT IS RESTORED HERE WITH ZPROP
      IF(ISOUSI.GT.1) THEN
        CALL PLANE_BOTTOM(IPBOT%I,ZPROP%R,NPOIN2,NPLAN,SIGMAG,OPTBAN)
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WAVE_EQUATION'
!
      CALL WAVE_EQUATION(ISOUSI)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WAVE_EQUATION'
!
!     RESTORES Z
      MESH3D%Z%R=>SAVEZ
!
!-----------------------------------------------------------------------
! CLIPS NEGATIVE DEPTHS
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORRECTION_DEPTH_3D'
      CALL CORRECTION_DEPTH_3D(MESH2D%GLOSEG%I,MESH2D%GLOSEG%DIM1)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORRECTION_DEPTH_3D'
!
!-----------------------------------------------------------------------
! BUILDS NEW MESH WITH THE NEW FREE SURFACE
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CALCOT'
      CALL CALCOT(Z,H%R)
!     IPBOT UPDATED ACCORDINGLY, E.G. FOR CALLS TO PREDIV AND CVDF3D
      CALL PLANE_BOTTOM(IPBOT%I,Z,NPOIN2,NPLAN,SIGMAG,OPTBAN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CALCOT'
!
!----------------------------------------------------------------------
!
!     GENERATES DATA FOR DELWAQ
!
      IF(INCLUS(COUPLING,'DELWAQ')) THEN
!
!     COMPUTING FLODEL (POINT TO POINT FLUXES)
!
      FORMUL = 'VGRADP       HOR'
      FORMUL(8:8) = '2'
!     ADVECTION FLUXES PER NODE (STORED IN MESH3D%W%R)
!     THE ASSEMBLED RESULT IN T3_04 IS NOT USED HERE
      SAVEZ     =>MESH3D%Z%R
      MESH3D%Z%R=>ZPROP%R
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VECTOR'
      CALL VECTOR(T3_04,'=',FORMUL,IELM3,-1.D0,DM1,SVIDE,GRAZCO,
     &            UCONV,VCONV,SVIDE,MESH3D,MSK,MASKEL)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VECTOR'
      CALL FLUX_EF_VF_3D(FLODEL%R,MESH2D%W%R,MESH3D%W%R,
     &                   MESH2D%NSEG,NELEM2,NELMAX2,
     &                   MESH2D,.TRUE.,
     &                   2,2,MESH3D%TYPELM+1,NPLAN,
!                          2: HORIZONTAL FLUXES FROM TOP TO BOTTOM
     &                   MESH3D%IKLE%I,MESH3D%NELMAX,MESH2D%KNOLG%I)
!     FLUX LIMITATION (FLULIM IS 2D, SO NUMBERING FROM TOP TO BOTTOM
!                      MAKES NO PROBLEM)
      IF(OPT_HNEG.EQ.2) THEN
        CALL FLUX3DLIM(FLODEL%R,FLULIM%R,NPLAN,MESH2D%NSEG,NPOIN2,1)
      ENDIF
      MESH3D%Z%R=>SAVEZ
!
!     NOW CALLING TEL4DELWAQ WITH FLODEL COMPLETED
!
!     SENDS UCONV AND VCONV AS ADVECTING FIELD (SEE WAVE_EQUATION)
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TEL4DELWAQ'
      CALL TEL4DELWAQ(NPOIN3,NPOIN2,MESH2D%NSEG,
     &  MESH2D%IKLE%I,MESH2D%ELTSEG%I,MESH2D%GLOSEG%I,
     &  MESH2D%GLOSEG%DIM1,X,Y,MESH3D%NPTFR,LIHBOR%I,MESH3D%NBOR%I,
     &  NPLAN,AT,DT,LT,NIT_ORI,H%R,HPROP%R,MESH3D%Z%R,UCONV%R,
     &  VCONV%R,TA%ADR(MAX(IND_S,1))%P%R,TA%ADR(MAX(IND_T,1))%P%R,
     &  VISCVI%ADR(3)%P%R,TITCAS,
     &  T3D_FILES(T3DGEO)%NAME,T3D_FILES(T3DCLI)%NAME,WAQPRD,
     &  T3DDL1,T3DDL2,T3DDL3,T3DDL5,T3DDL6,T3DDL7,T3DL11,T3DDL4,
     &  T3DDL8,T3DDL9,T3DL10,INFOGR,NELEM2,SALI_DEL,TEMP_DEL,VELO_DEL,
     &  DIFF_DEL,MARDAT,MARTIM,FLODEL%R,V2DPAR%R,MESH2D%KNOLG%I,
     &  T3D_FILES)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TEL4DELWAQ'
!
      ENDIF
!
!----------------------------------------------------------------------
!
! MASKING
!
      IF(ISOUSI.EQ.NSOUSI) THEN
        IF(MSK) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASK3D'
        IF(MSK) CALL MASK3D(MESH3D%IFABOR%I,MASKEL%R,MASKPT,MASKBR%R,
     &          X2%R,Y2%R,ZF%R,ZFE%R,H%R,HMIN,AT,LT,IT1%I,
     &          MESH3D%NELBOR%I,NELMAX2,NELEM2,NPOIN2,MESH2D%NELEB,
     &          NPLAN,NETAGE,IELM3,MESH2D)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MASK3D'
        ENDIF
      ENDIF
!
! COMPUTES SURFACE GRADIENTS AT TIME LEVEL N+1 AND DSSUDT
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FSGRAD'
      CALL FSGRAD(GRADZS,ZFLATS,Z(NPOIN3-NPOIN2+1:NPOIN3),
     &            ZF,IELM2H,MESH2D,MSK,MASKEL,
     &            UNSV2D,T2_01,NPOIN2,OPTBAN,SVIDE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FSGRAD'
!
      CALL OS( 'X=Y-Z   ', X=DSSUDT, Y=H, Z=HN )
      CALL OS( 'X=CX    ', X=DSSUDT, C=1.D0/DT )
!
! COMPUTES THE VOLUMES ASSOCIATED WITH NODES
!
      CALL VECTOR(VOLU, '=', 'MASBAS          ',IELM3,1.D0-AGGLOH,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
      IF(AGGLOH.GT.1.D-6) THEN
        CALL VECTOR(VOLU, '+', 'MASBAS2         ',IELM3,AGGLOH,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
      ENDIF
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=Y     ',X=VOLUPAR,Y=VOLU)
        CALL PARCOM(VOLUPAR,2,MESH3D)
      ENDIF
!
! IN 2D, ONLY IF MASKING (OTHERWISE NOTHING CHANGED)
!
      IF(MSK) CALL MASBAS2D(VOLU2D,V2DPAR,UNSV2D,
     &                      IELM2H,MESH2D,MSK,MASKEL,T2_01,SVIDE)
!
!-----------------------------------------------------------------------
! CONTINUITY STEP (NON-HYDROSTATIC OPTION) IN NEW MESH
!-----------------------------------------------------------------------
!
      IF(NONHYD.AND..NOT.DPWAVEQ) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'DANS NONHYDRO1'
        IF(INFOGR) CALL MITTIT(19,AT,LT)
!
        CALL OS ('X=Y     ', X=W , Y=WD  )
!
!-----------------------------------------------------------------------
!
! COMPUTES THE DYNAMIC PRESSURE
!
!       WITH WAVE EQUATION, DYNAMIC PRESSURE HERE IS INCREMENTAL
!       THUS WITHOUT BOUNDARY CONDITIONS
        BC=.NOT.DPWAVEQ
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREDIV'
        CALL PREDIV(DP,U,V,W,INFOGR,BC,1,.TRUE.,.TRUE.,.TRUE.)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREDIV'
!
!-----------------------------------------------------------------------
! VELOCITY PROJECTION STEP
!-----------------------------------------------------------------------
!
        IF(INFOGR) CALL MITTIT(20,AT,LT)
!
        CALL VELRES(U,V,W,DP,
     &             T3_01,T3_02,T3_03,MSK,MASKEL,MESH3D,
     &             SVIDE,IELM3,OPTBAN,T3_04,.TRUE.,NPOIN3,NPOIN2,
     &             SIGMAG,IPBOT%I,AGGLOH,KSORT,NPTFR3,LIUBOL,CONCOR)
!
!       BOUNDARY CONDITIONS ON W AT THE BOTTOM AND FREE SURFACE
!
!       FREE SURFACE (NOT ALWAYS TO BE DONE, DSSUDT IS SOMETIMES TOO BIG)
!
        IF(CLDYN) THEN
!
          CALL OV('X=Y     ',X=W%R(NPOIN3-NPOIN2+1:NPOIN3), Y=DSSUDT%R,
     &                       DIM1=NPOIN2)
          CALL OV('X=X+YZ  ',X=W%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       Y=GRADZS%ADR(1)%P%R,
     &                       Z=U%R(NPOIN3-NPOIN2+1:NPOIN3), DIM1=NPOIN2)
          CALL OV('X=X+YZ  ',X=W%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       Y=GRADZS%ADR(2)%P%R,
     &                       Z=V%R(NPOIN3-NPOIN2+1:NPOIN3), DIM1=NPOIN2)
!
        ENDIF
!
!       BOTTOM
!
        IF(VELPROBOT) THEN
          IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
            DO I=1,NPOIN2
              DO IP=0,IPBOT%I(I)
                I3D=IP*NPOIN2+I
                W%R(I3D)=GRADZF%ADR(1)%P%R(I)*U%R(I3D)
     &                  +GRADZF%ADR(2)%P%R(I)*V%R(I3D)
              ENDDO
            ENDDO
          ELSE
            DO I=1,NPOIN2
              W%R(I)=GRADZF%ADR(1)%P%R(I)*U%R(I)
     &              +GRADZF%ADR(2)%P%R(I)*V%R(I)
            ENDDO
          ENDIF
        ENDIF
!
!       RE-ENSURES THE DIRICHLET BOUNDARY CONDITIONS AND U.N = 0
!
        CALL AIRWIK2(LIHBOR%I, UBORF%R, VBORF%R, WBORF%R,
     &               LIUBOF%I, LIVBOF%I, LIWBOF%I,
     &               UBORL%R, VBORL%R, WBORL%R,
     &               LIUBOL%I, LIVBOL%I, LIWBOL%I,
     &               UBORS%R, VBORS%R, WBORS%R,
     &               LIUBOS%I, LIVBOS%I, LIWBOS%I,
     &               U%R,V%R,W%R,MESH2D%XNEBOR%R,MESH2D%YNEBOR%R,
     &               NBOR2%I,NPTFR2,NPLAN,NPOIN2,KENT,KADH,KLOG,KENTU,
     &               VELPROLAT)
!
      ENDIF ! IF NONHYD
!
!=======================================================================
! INTERNAL COUPLING WITH GAIA
!=======================================================================
!
      IF( INCLUS(COUPLING,'GAIA  ')) THEN
!
!       U AND V WITH 2D STRUCTURE : BOTTOM VELOCITY AS A
!       2D VARIABLE
        CALL CPSTVC(U2D,U)
        CALL CPSTVC(V2D,V)
!       TFOND IS CALLED FOR AN UPDATE OF
!       UETCAR at TIME N+1
        CALL TFOND(AUBORF%R,
     &             CF%R,U2D%R,V2D%R,U%R,V%R,W%R,KARMAN,
     &             LISRUF,DNUVIV,Z,NPOIN2,KFROT,RUGOF%R,UETCAR%R,
     &             NONHYD,OPTBAN,HN%R,GRAV,IPBOT%I,NPLAN)
!
!       NOW RUNS ONE TURN OF GAIA'S TIME LOOP AND RETURNS CALL
        CALL CONFIG_CODE(5)
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL GAIA_STEP'
!
        CALL GAIA_STEP(
     &       LT,GRAPRD,LISPRD,NIT_ORI,U2D,V2D,H,ZF,
     &       UETCAR,DELTAR,CF,RUGOF,CODE1,
     &       U,V,AT,VISCVI,DT,CHARR_TEL,SUSP_TEL,
     &       RHO0,GRAV,
!            VARIABLES TRANSMITTED FROM TOMAWAC
     &       DIRMOY,HM0,TPR5,ORBVEL,.TRUE.)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK GAIA_STEP'
        CALL CONFIG_CODE(1)
!
!       RETRIEVES ORIGINAL U AND V STRUCTURE
        CALL CPSTVC(UN,U)
        CALL CPSTVC(VN,V)
!
      ENDIF
!
!=======================================================================
! INTERNAL COUPLING WITH SISYPHE
!=======================================================================
!
      IF( INCLUS(COUPLING,'SISYPHE')   .AND.
     &   (PERCOU_SIS*(LT/PERCOU_SIS).EQ.LT.OR.LT.EQ.1) ) THEN
!
!       U AND V WITH 2D STRUCTURE : BOTTOM VELOCITY AS A 2D VARIABLE
        CALL CPSTVC(U2D,U)
        CALL CPSTVC(V2D,V)
!
!       NOW RUNS ONE TURN OF SISYPHE'S TIME LOOP AND RETURNS
        CALL CONFIG_CODE(2)
        IBID=1
        LBID=.FALSE.
        LBID2=.FALSE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SISYPHE'
        CALL SISYPHE(1,LT,GRAPRD,LISPRD,NIT_ORI,U2D,V2D,H,HN,ZF,
     &               UETCAR,CF,RUGOF,LBID,IBID,LBID2,CODE1,PERCOU_SIS,
     &               U,V,AT,VISCVI,DT*PERCOU_SIS,S3D_CHARR,S3D_SUSP,
!                          1 PRECLUDES THE USE OF THE 4 FOLLOWING ARGUMENTS
     &               FLBOR,1,DM1,UCONV,VCONV,ZCONV,
!                    VARIABLES TRANSMITTED FROM TOMAWAC
     &               DIRMOY,HM0,TPR5,ORBVEL,.TRUE.)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SISYPHE'
        CALL CONFIG_CODE(1)
!
!       S3D_HDEPMUST BE UPDATED BECAUSE SISYPHE CHANGED ZF
        IF(S3D_SEDI) CALL OS('X=Y-Z   ',X=S3D_HDEP,Y=ZF,Z=ZR)
!
!       RETRIEVES ORIGINAL U AND V STRUCTURE
        CALL CPSTVC(UN,U)
        CALL CPSTVC(VN,V)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     SEDIMENT
!
      IF(NSUSP_TEL.GT.0) THEN
!
!       COMPUTES THE SEDIMENT SETTLING VELOCITY
!
        CALL COMPUTE_SETTLING_VEL(WCHU,U,V,
     &       T3_01,T3_02,T3_03,SVIDE,MESH3D,IELM3,NPOIN2,NPOIN3,
     &       NPLAN,MSK,MASKEL,UETCAR,TA,HN)
!
!       BOUNDARY CONDITIONS FOR THE
!       SEDIMENTOLOGY
!
        CALL COMPUTE_BC_SEDI
!
      ENDIF
!
!-----------------------------------------------------------------------
!     PREPARING SOURCE TERMS FOR ADVECTION-DIFFUSION STEP
!-----------------------------------------------------------------------
!
!     PREPARING SOURCE TERMS FOR K-EPSILON AND K-OMEGA MODELS
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3.OR.ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
!
        IF (INFOGR) CALL MITTIT(7,AT,LT)
!
        S0AK%TYPR='Q'
        S0EP%TYPR='Q'
        S1AK%TYPR='Q'
        S1EP%TYPR='Q'
!
        IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOUKEP'
        CALL SOUKEP(S0AK%R,S0EP%R,S1AK%R,S1EP%R,
     &              U,V,W,DELTAR,RI%R,T3_01,T3_02,T3_03,T3_04,
     &              T3_05,T3_06,T3_07,T3_08,T3_09,
     &              T3_10,AK%R,EP%R,C1,C2,CMU,GRAV,
     &              NPOIN3,MSK,MASKEL,MESH3D,IELM3,SVIDE,
     &              VENT,WINDX,WINDY,NPOIN2,KMIN,PRANDTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOUKEP'
!
        ENDIF
!
        IF(ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOUKOM'
        CALL SOUKOM(S0AK,S0EP,S1AK,S1EP,U,V,W,
     &              DELTAR,T3_01,T3_02,T3_03,
     &              T3_04,T3_05,T3_06,T3_07,T3_08,
     &              T3_09,T3_10,T3_12,T3_13,
     &              T3_14,T3_15,T3_16,T3_17,
     &              ROTAT,AK,EP,ALPHA,BETA,BETAS,GRAV,
     &              T3_11,NPOIN3,MSK,MASKEL,MESH3D,IELM3,SVIDE,PRANDTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOUKOM'
!
        ENDIF
!
      ELSEIF(ITURBV.EQ.5.OR.ITURBV.EQ.9) THEN
!
        IF (INFOGR) CALL MITTIT(33,AT,LT)
        S0NU%TYPR='Q'
        S1NU%TYPR='Q'
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOUSA'

        CALL SOUSA(S0NU%R,S1NU%R,U,V,W,ROTAN,STRAIN,T3_01,
     &              NU,NPOIN3,MSK,MASKEL,MESH3D,
     &              IELM3,SVIDE,WDIST,NPOIN2, ITURBV)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOUSA'
      ENDIF
!
!     PREPARING SOURCE TERMS FOR TRACERS
!
      IF( (INCLUS(COUPLING,'WAQTEL') ).AND.
     &    (13*INT(WAQPROCESS/13).EQ.WAQPROCESS) ) THEN
!       ORDER: NUMBER OF THE PLANES FROM TOP TO BOTTOM
!       THEN NUMBER OF THE 2D NODES
        DO I=1,NPOIN2
!         K=1
!          THICK4AED2%R(1+(I-1)*NPLAN) = (Z(I+(NPLAN-1)*NPOIN2)
!     &                                -  Z(I+(NPLAN-2)*NPOIN2))*0.5D0
          THICK4AED2%R(1+(I-1)*NPLAN) = (Z(I+NPOIN3-NPOIN2)
     &                                -  Z(I+NPOIN3-2*NPOIN2))*0.5D0
          DO K=2,NPLAN-1
            THICK4AED2%R(K+(I-1)*NPLAN) = (Z(I+(NPLAN-K+1)*NPOIN2)
     &                                  -  Z(I+(NPLAN-K-1)*NPOIN2))/2.D0
          ENDDO
!         K=NPLAN
          THICK4AED2%R(I*NPLAN) = (Z(I+NPOIN2)-Z(I))*0.5D0
        ENDDO
!
        DO I=1,NPOIN2
          BENTH4AED2%I(I) = NPLAN
          DO K=NPLAN,2,-1
            IF(THICK4AED2%R(K+(I-1)*NPLAN).LT.EPS_AED2) THEN
              BENTH4AED2%I(I) = K-1
            ENDIF
          ENDDO
        ENDDO
!
!       ORDER: NUMBER OF THE PLANES FROM TOP TO BOTTOM
!       THEN NUMBER OF THE 2D NODES
        DO I=1,NPOIN2
          DO K=1,NPLAN
            TEMP4AED2%R(K+(I-1)*NPLAN) =
     &      TA%ADR(IND_T)%P%R(I+(NPLAN-K)*NPOIN2)
            SALI4AED2%R(K+(I-1)*NPLAN) =
     &      TA%ADR(IND_S)%P%R(I+(NPLAN-K)*NPOIN2)
          ENDDO
        ENDDO
!
      ENDIF
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOURCE_TRAC'
      IF(NTRAC.GT.0) CALL SOURCE_TRAC
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOURCE_TRAC'
!
!----------------------------------------------------------------------
! ADVECTION-DIFFUSION STEP FOR ALL ADVECTED VARIABLES
!----------------------------------------------------------------------
!
!     ALL ADVECTION SCHEMES EXCEPT SUPG
!
      IF (INFOGR .AND. (.NOT.NONHYD)) CALL MITTIT(9,AT,LT)
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREADV'
      CALL PREADV(W,WS,ZPROP,ISOUSI,LT,VOLU,VOLUN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREADV'
!
!----------------------------------------------------------------------
!     NOW CVDF3D WILL DO SUPG AND DIFFUSION
!----------------------------------------------------------------------
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3.OR.ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
!
        CLKMIN = .TRUE.
        CLKMAX = .TRUE.
        YASEM3D = .FALSE.
        YAWCHU = .FALSE.
        NEWDIF = .TRUE.
        TETATRA=MIN(TETADI,1.D0)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR AK'
        CALL CVDF3D
     & (AK,AKC,AKN,VISCVI,SIGMAK,S0AK,.TRUE.,S1AK,.TRUE.,
     &  KBORL, KBORF, KBORS, AKBORL, AKBORF, AKBORS,
     &  BKBORL, BKBORF, BKBORS, LIKBOL, LIKBOF, LIKBOS,
     &  FLUXBIL(1:MAXFRO+NSCE+1,1),
     &  FLUX%R(1), FLUEXT,FLUEXTPAR,KMIN, CLKMIN, KMAX, CLKMAX,
     &  SCHCKE,SCHDKE,SLVDKE,TRBAKE,INFOGR,NEWDIF,CALCFLU(4),
     &  T2_01,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     &  NPTFR3,MMURD,MURD_TF,VOLU,VOLUPAR,VOLUN ,VOLUNPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM3,
     &  NPLAN,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,
     &  TRAV3,MESH2D,OPTBAN,TETATRA,
     &  YAWCHU,WCHU,S3D_WCHU,AGGLOD,NSCE,SOURCES,AK_SCE%R,
     &  NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,CALCRAIN(4),
     &  PLUIE,PARAPLUIE,0.D0,FLODEL,FLOPAR,SIGMAG,IPBOT%I,MAXADV,
     &  FLUDPT,FLUDP,FLUER,VOLU2D,V2DPAR,SETDEP,
     &  S3D_FLUDPT,S3D_FLUDP,S3D_FLUER,S3D_SETDEP,OPTSOU,
     &  ZN%R,OPTADV_KE,NCO_DIST,NSP_DIST,TB2)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR AK'
!
        CLEMIN  = .TRUE.
        CLEMAX  = .TRUE.
        YASEM3D = .FALSE.
        YAWCHU  = .FALSE.
!
!       NEGLECTS MOLECULAR DIFFUSIVITY...
!       DIFFUSION MATRIX NOT RECOMPUTED
        NEWDIF = .FALSE.
        CALL OM('M=CM    ',M=MDIFF,C=SIGMAE/SIGMAK,MESH=MESH3D)
!
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR EP'
        CALL CVDF3D
     & (EP,EPC,EPN,VISCVI,SIGMAE,S0EP,.TRUE.,S1EP,.TRUE.,
     &  EBORL, EBORF, EBORS, AEBORL, AEBORF, AEBORS,
     &  BEBORL, BEBORF, BEBORS, LIEBOL, LIEBOF, LIEBOS,
     &  FLUXBIL(1:MAXFRO+NSCE+1,1),
     &  FLUX%R(1), FLUEXT,FLUEXTPAR,EMIN, CLEMIN, EMAX, CLEMAX,
     &  SCHCKE,SCHDKE,SLVDKE,TRBAKE,INFOGR,NEWDIF,CALCFLU(5),
     &  T2_01,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     &  NPTFR3,MMURD,MURD_TF,VOLU,VOLUPAR,VOLUN,VOLUNPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM3,
     &  NPLAN,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,
     &  TRAV3,MESH2D,OPTBAN,TETATRA,
     &  YAWCHU,WCHU,S3D_WCHU,AGGLOD,NSCE,SOURCES,EP_SCE%R,
     &  NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,CALCRAIN(5),
     &  PLUIE,PARAPLUIE,0.D0,FLODEL,FLOPAR,SIGMAG,IPBOT%I,
     &  MAXADV,FLUDPT,FLUDP,FLUER,VOLU2D,V2DPAR,
     &  SETDEP,S3D_FLUDPT,S3D_FLUDP,S3D_FLUER,S3D_SETDEP,
     &  OPTSOU,ZN%R,OPTADV_KE,NCO_DIST,NSP_DIST,TB2)
!
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR EP'
!
      ELSEIF(ITURBV.EQ.5.OR.ITURBV.EQ.9) THEN
!
        CLNUMIN = .TRUE.
        CLNUMAX = .FALSE.
        YASEM3D = .FALSE.
        YAWCHU = .FALSE.
        NEWDIF = .TRUE.
        TETATRA=MIN(TETADI,1.D0)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDF3D POUR NU'
        CALL CVDF3D
     & (NU,NUC,NUN,VISCNU,SIGMANU,S0NU,.TRUE.,S1NU,.TRUE.,
     &  NUBORL, NUBORF, NUBORS, ANUBORL, ANUBORF, ANUBORS,
     &  BNUBORL, BNUBORF, BNUBORS, LINUBOL, LINUBOF, LINUBOS,
     &  FLUXBIL(1:MAXFRO+NSCE+1,1),
     &  FLUX%R(1),FLUEXT,FLUEXTPAR,NUMIN,CLNUMIN,NUMAX,CLNUMAX,
     &  SCHCKE,SCHDKE,SLVDKE,TRBAKE,INFOGR,NEWDIF,CALCFLU(4),
     &  T2_01,T2_03,
     &  T3_01,T3_02,T3_03,T3_04, MESH3D , IKLE3 , MASKEL , MTRA1,
     &  NPTFR3,MMURD,MURD_TF,VOLU,VOLUPAR,VOLUN ,VOLUNPAR,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM3,
     &  NPLAN,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,
     &  TRAV3,MESH2D,OPTBAN,TETATRA,
     &  YAWCHU,WCHU,S3D_WCHU,AGGLOD,NSCE,SOURCES,NU_SCE%R,
     &  NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,CALCRAIN(4),
     &  PLUIE,PARAPLUIE,0.D0,
     &  FLODEL,FLOPAR,SIGMAG,IPBOT%I,MAXADV,FLUDPT,FLUDP,
     &  FLUER,VOLU2D,V2DPAR,SETDEP,S3D_FLUDPT,S3D_FLUDP,S3D_FLUER,
     &  S3D_SETDEP,OPTSOU,ZN%R,OPTADV_KE,NCO_DIST,
     &  NSP_DIST,TB2)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDF3D POUR NU'
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES THE VISCOSITIES VISCVI, VISCTA AND VISCTP
!
      IF(ITURBH.EQ.1.OR.ITURBV.EQ.1) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCOS'
        CALL VISCOS(VISCVI,VISCTA,DNUTAV,DNUTAH,
     &              DNUVIV,DNUVIH,NTRAC,ITURBH,ITURBV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCOS'
!
      ENDIF
!
      IF(ITURBV.EQ.2) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCLM'
        CALL VISCLM(VISCVI,VISCTA,RI,U,V,DELTAR,Z3,H,
     &              T3_01, T3_02, T3_03, T3_04, T3_05, T3_07,
     &              MESH3D, IELM3, GRAV, NPLAN,
     &              NPOIN3, NPOIN2, NTRAC, MSK, MASKEL,
     &              MIXING,DAMPING,DNUVIV,DNUTAV,KARMAN,
     &              PRANDTL,KFROT,RUGOF,ZF,LINLOG,IPBOT%I)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCLM'
!
      ENDIF
!
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCKE'
        CALL VISCKE(VISCVI,VISCTA,AK,EP,NTRAC,CMU,
     &              DNUVIH,DNUVIV,DNUTAH,DNUTAV,EMIN,
     &              ITURBH,ITURBV,PRANDTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCKE'
!
      ENDIF
!
      IF(ITURBH.EQ.4) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISSMA'
        CALL VISSMA(VISCVI,VISCTA,
     &              DNUTAH,DNUVIH,DNUVIV,DNUTAV,
     &              U,V,W,T3_01,T3_02,T3_03,T3_04,T3_05,T3_06,
     &              SVIDE,MESH3D,
     &              IELM3,NTRAC,MSK,MASKEL,ITURBV,PRANDTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISSMA'
!
      ENDIF
!
      IF(ITURBV.EQ.7.OR.ITURBH.EQ.7) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCKO'
        CALL VISCKO(VISCVI,VISCTA,ROTAT,AK,EP,NTRAC,
     &              DNUVIH,DNUVIV,DNUTAH,DNUTAV,ITURBH,ITURBV,
     &              T3_01,T3_02,PRANDTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCKO'
!
      ENDIF
!
      IF(ITURBV.EQ.5.OR.ITURBV.EQ.9) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VISCSA'
        CALL VISCSA(VISCVI,VISCNU,VISCTA,NU,NTRAC,
     &              DNUVIH,DNUVIV,DNUTAH,DNUTAV,AK,EP,STRAIN,PRANDTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VISCSA'
      ENDIF
!
      IF(ITURBV.EQ.6) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE GOTM_COUPLING_STEP'
          CALL GOTM_COUPLING_STEP
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE GOTM_COUPLING_STEP'
      ENDIF
!
      IF(INCLUS(COUPLING,'TOMAWACT3D')) THEN
!       ADDING VERTICAL MIXING DUE TO WAVES
        CALL OS('X=X+Y   ',X=VISCVI%ADR(3)%P,Y=FDK)
      ENDIF
!
      IF(OPTBAN.EQ.1) THEN
!
        CALL VISCLIP(VISCVI,VISCTA,H,NPLAN,NPOIN2,NTRAC,HLIM)
!
      ENDIF
!
!=======================================================================
!     OIL SPILL MODEL (UNDER DEVELOPMENT IN MYGRHYCAR PROJECT)
!=======================================================================
!
      IF(SPILL_MODEL) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE OIL_SPILL_3D'
          CALL OIL_SPILL_3D(LT,IELM2H,MESH2D,NFLOT_MAX,T3D_FILES,
     &                     MAXLU_T3D,NPOIN2,T3DMIG,UCONV,VCONV,WCONV,
     &                     NFLOT,NPLAN,MESH3D,AT,DT,GRAV,CF,X,Y,Z,H,HN,
     &                     IELM3,NPOIN3,NELEM2,XFLOT,YFLOT,ZFLOT,SHPFLO,
     &                     SHZFLO,TAGFLO,CLSFLO,ELTFLO,ETAFLO,FLOPRD,
     &                     T3DFLO,IT1,IT2,T3_01,T3_02,T3_03,MTRA1,MTRA2,
     &                     VISCVI,WINDX,WINDY,UNSV3D,NTRAC,TRN,TRAV3,
     &                     ATABOS,T2_17,VENT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE OIL_SPILL_3D'
!
      ENDIF
!
!-----------------------------------------------------------------------
! ADVECTION-DIFFUSION OF TRACERS
!
!GA: POINTERS ARE USED TO ALLOW TWO SEDIMENT CLASSES (MULTI1,2,3,4)
      IF(NTRAC.GT.0) THEN
!
        IF (INFOGR) CALL MITTIT(5,AT,LT)
!
          SIGMTA = 1.D0
          TAMIN  = 0.D0
          TAMAX  = 1.D0
          CTAMIN = .FALSE.
          CTAMAX = .FALSE.
          YASEM3D = .FALSE.
          NEWDIF = .TRUE.
          TETATRA=MIN(TETADI,1.D0)
!
!     IF MICROPOL MODULE IS ACTIVATED, TRACERS BOTTOM SEDIMENT (IND_SF)
!     AND MICROPOLLUTANT ADSORBED BY BOTTOM SEDIMENT (IND_CSF)
!     ARE NEITHER ADVECTED NOR DIFFUSED (NO ADVECTION DONE IN LECDON)
        IF(INCLUS(COUPLING,'WAQTEL').AND.
     &     7*INT(WAQPROCESS/7).EQ.WAQPROCESS) THEN
          CALL OS('X=0     ', X=VISCTA%ADR(IND_SF)%P)
          CALL OS('X=0     ', X=VISCTA%ADR(IND_CSF)%P)
          IF(KIN_MICROPOL.EQ.2) THEN
            CALL OS('X=0     ', X=VISCTA%ADR(IND_CSF2)%P)
          ENDIF
        ENDIF
!
        DO ITRAC = 1,NTRAC
!
          MULTI1 => S3D_FLUDPT
          MULTI2 => S3D_FLUDP
          MULTI3 => S3D_FLUER
          MULTI4 => S3D_WCHU
!
          IF(ITRAC.EQ.IND_SS.OR.ITRAC.EQ.IND_CSS.OR.
     &       ITRAC.EQ.IND_CSS2) THEN
            YAWCHU=.TRUE.
            SETDEP=0
!           SOLVER STRUCTURE
            SLVD=SLVDTA(ITRAC)
          ELSEIF(ITRAC.GE.IND_SED.AND.ITRAC.LE.IND_SED+NSUSP_TEL-1) THEN
            ISUSP=ITRAC-IND_SED+1
            YAWCHU=.TRUE.
!           SOLVER STRUCTURE
            SLVD=SLVDTA(ITRAC)
          ELSEIF(S3D_SEDI.AND.ITRAC.EQ.NTRAC) THEN
            YAWCHU=.TRUE.
!           SOLVER STRUCTURE
            SLVD=S3D_SLVDSE
          ELSE
            YAWCHU=.FALSE.
!           SOLVER STRUCTURE
            SLVD=SLVDTA(ITRAC)
          ENDIF
!
          IF(S3D_SEDI.AND.S3D_MIXTE.AND.ITRAC.EQ.(NTRAC-1)) THEN
            YAWCHU=.TRUE.
            SLVD=S3D_SLVDSE
            MULTI1 => S3D_FLUDPTNC
            MULTI2 => S3D_FLUDPNC
            MULTI3 => S3D_FLUERNC
            MULTI4 => S3D_WCS
          ELSEIF(S3D_SEDI.AND.S3D_MIXTE.AND.ITRAC.EQ.NTRAC) THEN
            MULTI1 => S3D_FLUDPTC
            MULTI2 => S3D_FLUDPC
            MULTI3 => S3D_FLUERC
          ENDIF
!
          IF(DEBUG.GT.0) THEN
            WRITE(LU,*) 'APPEL DE CVDF3D POUR TRACEUR ',ITRAC
          ENDIF
!
          CALL CVDF3D
     &   (TA%ADR(ITRAC)%P,TAC%ADR(ITRAC)%P,TRN%ADR(ITRAC)%P,
     &    VISCTA%ADR(ITRAC)%P,SIGMTA,
     &    S0TA%ADR(ITRAC)%P,.TRUE.,S1TA%ADR(ITRAC)%P,.TRUE.,
     &    TABORL%ADR(ITRAC)%P,TABORF%ADR(ITRAC)%P,TABORS%ADR(ITRAC)%P,
     &    ATABOL%ADR(ITRAC)%P,ATABOF%ADR(ITRAC)%P,ATABOS%ADR(ITRAC)%P,
     &    BTABOL%ADR(ITRAC)%P,BTABOF%ADR(ITRAC)%P,BTABOS%ADR(ITRAC)%P,
     &    LITABL%ADR(ITRAC)%P,LITABF%ADR(ITRAC)%P,LITABS%ADR(ITRAC)%P,
     &    FLUXBIL(1:MAXFRO+NSCE+1,5+ITRAC),
     &    FLUX%R(5+ITRAC),FLUEXT,FLUEXTPAR,
     &    TAMIN,CTAMIN,TAMAX,CTAMAX,SCHCTA(ITRAC),
     &    SCHDTA,SLVD,TRBATA,INFOGR,NEWDIF,CALCFLU(5+ITRAC),
     &    T2_01,T2_03,T3_01,T3_02,T3_03,T3_04,MESH3D,IKLE3,MASKEL,
     &    MTRA1,NPTFR3,MMURD,MURD_TF,VOLU,VOLUPAR,VOLUN,VOLUNPAR,
     &    NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM3,NPLAN,IELM3,MSUPG,
     &    IELM2H,IELM2V,MDIFF,MTRA2,INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,
     &    SVIDE,IT1,TRAV3,MESH2D,OPTBAN,TETATRA,
     &    YAWCHU,WCHU%ADR(ITRAC)%P,MULTI4,AGGLOD,NSCE,SOURCES,
     &    TA_SCE%ADR(ITRAC)%P%R,
     &    NUMLIQ%I,DIRFLU,NFRLIQ,VOLUT,ZT,ZPROP,CALCRAIN(5+ITRAC),
     &    PLUIE,PARAPLUIE,TRAIN(ITRAC),FLODEL,FLOPAR,SIGMAG,IPBOT%I,
     &    MAXADV,FLUDPT%ADR(MAX(NUM_ISUSP_ICLA(MAX(ISUSP,1)),1))%P,
     &    FLUDP%ADR(MAX(NUM_ISUSP_ICLA(MAX(ISUSP,1)),1))%P,
     &    FLUER%ADR(MAX(NUM_ISUSP_ICLA(MAX(ISUSP,1)),1))%P,VOLU2D,
     &    V2DPAR,SETDEP,
     &    MULTI1,MULTI2,MULTI3,S3D_SETDEP,OPTSOU,
     &    ZN%R,OPTADV_TR(ITRAC),NCO_DIST,NSP_DIST,TB2)
!
!         NEWDIF=.FALSE. (POSSIBLE IF SIGMTA UNCHANGED)
!
          IF(DEBUG.GT.0) THEN
            WRITE(LU,*) 'RETOUR DE CVDF3D POUR TRACEUR ',ITRAC
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
! COMPUTES DELRA RHO / RHO FOR THE BUOYANCY TERMS
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DRSURR'
        CALL DRSURR(DELTAR,TA,BETAC,T0AC,RHO,RHO0,XMVS0,S3D_RHOS,
     &              DENLAW,S3D_SEDI,NTRAC,IND_T,IND_S,IND_SED,NSUSP_TEL,
     &              S3D_MIXTE,NUM_ISUSP_ICLA,NSICLA)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DRSURR'
!
        IF( (INCLUS(COUPLING,'WAQTEL') ).AND.
     &      (13*INT(WAQPROCESS/13).EQ.WAQPROCESS) ) THEN
!       ORDER: NUMBER OF THE PLANES FROM TOP TO BOTTOM
!       THEN NUMBER OF THE 2D NODES
          DO I=1,NPOIN2
            DO K=1,NPLAN
              RHO4AED2%R(K+(I-1)*NPLAN) = RHO%R(I+(NPLAN-K)*NPOIN2)
            ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
#if defined COMPAD
      CALL AD_TELEMAC3D_SUBITERATION_END
#endif
!
      END DO SUBITER
!
!-----------------------------------------------------------------------
!
! SEDIMENTOLOGY : FLUDP SENT TO GAIA
!
      IF(NSUSP_TEL.GT.0) THEN
        DO ITRAC=IND_SED, IND_SED+NSUSP_TEL-1
          ISUSP=ITRAC-IND_SED+1
!
          IF(SETDEP.NE.1) THEN
            IF(OPTBAN.EQ.1) THEN
              DO IPOIN=1,NPOIN2
!               correction for tidal flats: take the first point above crushed planes
!               IPBOT=0: no tidal flats
!               IPBOT=NPLAN-1: dry element
                IF(IPBOT%I(IPOIN).NE.NPLAN-1) THEN
                  FLUDP%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN)=
     &                  FLUDPT%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN)*
     &                  TA%ADR(ITRAC)%P%R(IPBOT%I(IPOIN)*NPOIN2+IPOIN)
                  FLUDP%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN)=
     &                  MAX(FLUDP%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN),
     &                      0.D0)
                ELSE
                  FLUDP%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN)=0.D0
                ENDIF
              ENDDO
            ELSE
              DO IPOIN=1,NPOIN2
                FLUDP%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN)=
     &                FLUDPT%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN)*
     &                TA%ADR(ITRAC)%P%R(IPOIN)
!               FLUDP MUST BE POSITIVE, EVEN IF TA<0 DUE TO TRUNCATION ERRORS PROBLEM
!               SEEN WITH TA=-1.D-87 !!!!!
                FLUDP%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN)=
     &                MAX(FLUDP%ADR(NUM_ISUSP_ICLA(ISUSP))%P%R(IPOIN),
     &                0.D0)
              ENDDO
            ENDIF
          ENDIF
!
        ENDDO
      ENDIF
!
! SEDIMENTOLOGY SEDI3D: BOTTOM TREATMENT
!
      IF(S3D_SEDI) THEN
!
!       FONVAS DOES ZF=ZR+S3D_HDEP, THUS S3D_HDEPMUST INCLUDE BEDLOAD
!       EROSION, HAS BEEN TAKEN INTO ACCOUNT INTO CLSEDI ABOVE
!   GA: FONVAS HAS BEEN MODIFIED TO ALLOW TWO SEDIMENT CLASSES
        IF (S3D_MIXTE) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FONVAS S3D_MIXTE'
          CALL FONVAS
     &       (S3D_EPAI,S3D_CONC,
     &        S3D_HDEP%R,S3D_FLUDP%R,S3D_FLUDPT%R,S3D_FLUER%R,ZF%R,
     &        TA%ADR(NTRAC)%P%R,
     &        NPOIN2,NPOIN3,S3D_NCOUCH,DT,
     &        S3D_ZF_S%R,S3D_ESOMT%R,VOLU2D,S3D_MASDEP,S3D_SETDEP,ZR%R,
     &        TA%ADR(NTRAC-1)%P%R,S3D_FLUDPTC%R,S3D_FLUDPTNC%R,
     &        S3D_FLUERC%R,S3D_FLUERNC%R,S3D_MIXTE,S3D_FLUDPC%R,
     &        S3D_FLUDPNC%R,S3D_PVSCO%R,S3D_PVSNCO%R,S3D_CFDEP,
     &        S3D_EPAICO%R,S3D_EPAINCO%R)

          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FONVAS S3D_MIXTE'
!
        ELSEIF (S3D_SEDCO.OR.S3D_SEDNCO) THEN

          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FONVAS '
          CALL FONVAS
     &       (S3D_EPAI,S3D_CONC,
     &        S3D_HDEP%R,S3D_FLUDP%R,S3D_FLUDPT%R,S3D_FLUER%R,ZF%R,
     &        TA%ADR(NTRAC)%P%R,
     &        NPOIN2,NPOIN3,S3D_NCOUCH,DT,
     &        S3D_ZF_S%R,S3D_ESOMT%R,VOLU2D,S3D_MASDEP,S3D_SETDEP,ZR%R,
     &        TA%ADR(NTRAC)%P%R,S3D_FLUDPTC%R,S3D_FLUDPTNC%R,
     &        S3D_FLUERC%R,S3D_FLUERNC%R,S3D_MIXTE,S3D_FLUDPC%R,
     &        S3D_FLUDPNC%R,S3D_PVSCO%R,S3D_PVSNCO%R,S3D_CFDEP,
     &        S3D_EPAICO%R,S3D_EPAINCO%R)

          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FONVAS '
        ELSE
!
          WRITE(LU,*) ' '
          WRITE(LU,*) 'SEDI3D : ERROR ON SEDIMENT KEY WORD'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!
      IF(INCLUS(COUPLING,'GAIA  ').AND.NSUSP_TEL.GT.0) THEN
!
!       U AND V WITH 2D STRUCTURE : BOTTOM VELOCITY AS A 2D
!       VARIABLE
        CALL CPSTVC(U2D,U)
        CALL CPSTVC(V2D,V)
!
!       NOW RUNS ONE TURN OF GAIA'S TIME LOOP
!       AND RETURNS
        CALL CONFIG_CODE(5)
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING GAIA_STEP 2'
        CALL GAIA_STEP(
     &       LT,GRAPRD,LISPRD,NIT_ORI,U2D,V2D,H,ZF,
     &       UETCAR,DELTAR,CF,RUGOF,CODE1,
     &       U,V,AT,VISCVI,DT,CHARR_TEL,SUSP_TEL,
     &       RHO0,GRAV,
!            VARIABLES TRANSMITTED FROM TOMAWAC
     &       DIRMOY,HM0,TPR5,ORBVEL,.TRUE.)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK GAIA_STEP 2'
        CALL CONFIG_CODE(1)
!
!       check if this is necessary
!       RETRIEVES ORIGINAL U AND V STRUCTURE
        CALL CPSTVC(UN,U)
        CALL CPSTVC(VN,V)
!       end check
      ENDIF
!
! UPDATES GEOMETRY IF THE BOTTOM HAS EVOLVED
!
      IF(INCLUS(COUPLING,'SISYPHE').OR.INCLUS(COUPLING,'GAIA  ')
     &   .OR.S3D_SEDI) THEN
!
!       COPIES MODIFIED BOTTOM TOPOGRAPHY INTO Z AND ZPROP
        CALL OV('X=Y     ', X=Z(1:NPOIN2), Y=ZF%R, DIM1=NPOIN2)
        CALL OV('X=Y     ', X=ZPROP%R(1:NPOIN2), Y=ZF%R, DIM1=NPOIN2)
!       COMPUTES NEW BOTTOM GRADIENTS AFTER SEDIMENTATION
        CALL GRAD2D(GRADZF%ADR(1)%P,GRADZF%ADR(2)%P,ZPROP,NPLAN,SVIDE,
     &              UNSV2D,T2_02,T2_03,T2_04,
     &              IELM2H,MESH2D,MSK,MASKEL)
!       COMPUTES NEW Z COORDINATES
        CALL CALCOT(Z,H%R)
!       USEFUL ? NOT SURE, IS DONE AT EACH TIMESTEP ELSEWHERE, SO..
!       CALL CALCOT(ZPROP%R,HPROP%R)
        CALL FSGRAD(GRADZS,ZFLATS,Z(NPOIN3-NPOIN2+1:NPOIN3),
     &              ZF,IELM2H,MESH2D,MSK,MASKEL,
     &              UNSV2D,T2_01,NPOIN2,OPTBAN,SVIDE)
        CALL VECTOR(VOLU, '=', 'MASBAS          ',IELM3,1.D0-AGGLOH,
     &       SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
        IF(AGGLOH.GT.1.D-6) THEN
          CALL VECTOR(VOLU, '+', 'MASBAS2         ',IELM3,AGGLOH,
     &        SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,.FALSE.,MASKEL)
        ENDIF
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=VOLUPAR,Y=VOLU)
          CALL PARCOM(VOLUPAR,2,MESH3D)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! PREPARES 2D AND 3D OUTPUT
!
!     ALWAYS CALLED (SEE E.G. OUTPUT MAXZ)
!
      CALL PRERES_TELEMAC3D
!
!     CALLED IF OUTPUTS REQUESTED
!
      IF(MOD(LT,GRAPRD).EQ.0.AND.LT.GE.GRADEB) THEN
!
!       3D OUTPUT
!
        IF(T3D_FILES(T3DRES)%NAME(1:1).NE.' ') THEN
          CALL BIEF_DESIMP(T3D_FILES(T3DRES)%FMT,VARSO3,
     &                     NPOIN3,T3D_FILES(T3DRES)%LU,AT,LT,
     &                     LISPRD,GRAPRD,
     &                     SORG3D,SORIM3,MAXVA3,TEXT3,GRADEB,LISDEB)
        ENDIF
!
!       2D OUTPUT
!
        IF(T3D_FILES(T3DHYD)%NAME(1:1).NE.' ') THEN
          CALL BIEF_DESIMP(T3D_FILES(T3DHYD)%FMT,VARSOR,
     &                     NPOIN2,T3D_FILES(T3DHYD)%LU,AT,LT,
     &                     LISPRD,GRAPRD,
     &                     SORG2D,SORIMP,MAXVAR,TEXTE,GRADEB,LISDEB)
        ENDIF
!
      ENDIF
!
!     3D OUTPUT FOR RESTART, ONLY FOR FINAL TIME STEP
!
      IF(RESTART_MODE.AND.T3D_FILES(T3DRST)%NAME(1:1).NE.' ') THEN
        IF(RSTPRD.NE.0.AND.MOD(LT,RSTPRD).EQ.0) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BIEF_DESIMP RESTART'
          CALL BIEF_DESIMP(T3D_FILES(T3DRST)%FMT,VARSO3,NPOIN3,
     &                     T3D_FILES(T3DRST)%LU,AT,LT,
     &                     1,RSTPRD,SOREST,SORIS3,MAXVA3,TEXT3,0,0)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BIEF_DESIMP RESTART'
        ELSEIF(RESTART_RECORD.EQ.-1.AND.LT.EQ.NIT_ORI
     &     .OR.RESTART_RECORD.NE.-1.AND.LT.EQ.RESTART_RECORD) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BIEF_DESIMP RESTART'
          CALL BIEF_DESIMP(T3D_FILES(T3DRST)%FMT,VARSO3,NPOIN3,
     &                     T3D_FILES(T3DRST)%LU,AT,LT,
     &                     1,LT,SOREST,SORIS3,MAXVA3,TEXT3,0,0)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BIEF_DESIMP RESTART'
        ENDIF
      ENDIF
!
! SEDIMENTOLOGY OUTPUT FOR SEDI
!
      IF(S3D_SEDI.AND.T3D_FILES(S3D_T3DSED)%NAME(1:1).NE.' ') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DESSED'
        CALL DESSED(NPF%I,S3D_IVIDE%R,S3D_EPAI,S3D_HDEP%R,
     &              S3D_TEMP%R,ZR%R,NPOIN2,S3D_NPFMAX,
     &              S3D_NCOUCH,GRAPRD,LT,S3D_DTC,S3D_TASSE,
     &              S3D_GIBSON,T3D_FILES(S3D_T3DSED)%LU,TITCAS,
     &              S3D_BIRSED,0)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DESSED'
      ENDIF
!
! OPTIONAL USER OUTPUT
!
      CALL UTIMP(GRADEB,GRAPRD,LISDEB,LISPRD)
!
! SEDIMENT OUTPUT FOR SEDI
!
      IF(S3D_SEDI) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE IMPSED'
        CALL IMPSED(S3D_IVIDE%R,S3D_EPAI,S3D_CONC,S3D_TEMP%R,S3D_HDEP%R,
     &              S3D_PDEPO%R,S3D_FLUER%R,
     &              ZR%R,ZF%R,TA%ADR(NTRAC)%P%R,S3D_WCHU%R,X,Y,
     &              NPOIN2,NPOIN3,S3D_NPFMAX,S3D_NCOUCH,NPF%I,LT,
     &              S3D_RHOS,S3D_CFMAX,
     &              S3D_CFDEP,S3D_EPAI0,S3D_TASSE,S3D_GIBSON,
     &              PRIVE,LISPRD)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE IMPSED'
      ENDIF
!
! KHIONE OUTPUT
!
      IF(INCLUS(COUPLING,'KHIONE')) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING OUTPUT_KHIONE'
        CALL OUTPUT_KHIONE(
     &    TITCAS,        ! TITLE
     &    NPOIN2,        ! LOCAL NUMBER OF POINTS
     &    AT,LT,         ! TIME AND TIME NUMBER
     &    MESH2D,
     &    VARSOR,        ! POINTERS TO TELEMAC VARIABLES
     &    MARDAT,        ! START DATE
     &    MARTIM,        ! START TIME
     &    .FALSE.,       ! WHETHER TO WRITE THE FILE HEADER
     &    .FALSE.,       ! WHETHER TO WRITE THE FILE MESH
     &    .TRUE.,        ! WHETEHR TO WRITE THE VARIABLES AT AT
     &    TA,
     &    NPOIN3,NPLAN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM OUTPUT_KHIONE'
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING OUTPUT3D_KHIONE'
        CALL OUTPUT3D_KHIONE(
     &    TITCAS,        ! TITLE
     &    AT,LT,         ! TIME AND TIME NUMBER
     &    MESH3D,
     &    MARDAT,        ! START DATE
     &    MARTIM,        ! START TIME
     &    .FALSE.,       ! WHETHER TO WRITE THE FILE HEADER
     &    .FALSE.,       ! WHETHER TO WRITE THE FILE MESH
     &    .TRUE.,        ! WHETHER TO WRITE THE VARIABLES AT AT
     &    TA,
     &    NPOIN3,NPLAN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM OUTPUT3D_KHIONE'
      ENDIF
!
!=======================================================================
! DROGUES/FLOATS/BUOYS
!=======================================================================
!
      IF(NFLOT_MAX.GT.0.AND..NOT.SPILL_MODEL) THEN
!
        IF(SPHERI) THEN
          CALL OS('X=Y/Z   ',X=UCONV,Y=UCONV,Z=MESH3D%COSLAT)
          CALL OS('X=Y/Z   ',X=VCONV,Y=VCONV,Z=MESH3D%COSLAT)
        ENDIF
!
!       ADDING AND REMOVING DROGUES
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FLO3D'
        CALL FLOT3D(XFLOT%R,YFLOT%R,ZFLOT%R,NFLOT,NFLOT_MAX,X,Y,Z,
     &              MESH3D%IKLE%I,
     &              MESH3D%NELEM,MESH3D%NELMAX,NPOIN3,NPLAN,
     &          TAGFLO%I,CLSFLO%I,SHPFLO%R,SHZFLO%R,ELTFLO%I,ETAFLO%I,
     &              MESH3D,LT,NIT_ORI,AT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FLO3D'
!
        IF(INFOGR) CALL MITTIT(12,AT,LT)
!
!       MOVING THEM
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DERIVE'
        CALL DERIVE(UCONV%R,VCONV%R,WCONV%R,DT,AT,
     &              X,Y,Z,
     &              MESH2D%IKLE%I,MESH3D%IFABOR%I,LT,IELM3,UCONV%ELM,
     &              3,3,
     &              NPOIN3,NPOIN2,NELEM2,MESH2D%NELMAX,
     &              MESH2D%SURDET%R,XFLOT%R,YFLOT%R,ZFLOT%R,
     &          SHPFLO%R,SHZFLO%R,TAGFLO%I,CLSFLO%I,ELTFLO%I,ETAFLO%I,
     &          NFLOT,NFLOT_MAX,MESH3D,
     &              IT1%I,T3_01%R,T3_02%R,T3_03%R,IT2%I,
!                                              NO STOCHASTIC DIFFUSION
     &              MTRA1%X%R,MTRA2%X%R,NPOIN3,0,SVIDE,
     &              NPLAN,ZCHAR%R,TRANSF)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DERIVE'
!
!       PRINTING THEM
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING UTIMP_DROGUES'
        CALL UTIMP_DROGUES( LT,AT,NPOIN2,NPOIN3,
     &    XFLOT%R,YFLOT%R,ZFLOT%R,TAGFLO%I,CLSFLO%I,
     &    NFLOT,NFLOT_MAX,FLOPRD,DEJA_DROGUES_UTIMP,
     &    T3D_FILES(T3DFLO),T3D_FILES(T3DBLO), MARDAT,MARTIM )
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM UTIMP_DROGUES'
!
        IF(SPHERI) THEN
          CALL OS('X=XY    ',X=UCONV,Y=MESH3D%COSLAT)
          CALL OS('X=XY    ',X=VCONV,Y=MESH3D%COSLAT)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! MASS BALANCE FOR THE CURRENT TIMESTEP
!
      IF (BILMAS) THEN
!
        IF (.NOT.INFMAS) INFOGR = .FALSE.
        INFOGR = INFOGR .AND. LISTIN
        IF (INFOGR) CALL MITTIT(10,AT,LT)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASS3D'
        CALL MASS3D(INFOGR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MASS3D'
!
!
        IF(S3D_SEDI) THEN
!
!         DETERMINE S3D_MASSUSP: MASS IN SUSPENSION
!         S3D_MASBED: MASS OF SEDIMENT BED
!         S3D_MASDEP: DEPOSITED MASS
          S3D_MASSUSP= MASSE%R(5+NTRAC)
!         INITIALISATION
          IF(LT.EQ.1) S3D_MASSUSP0= S3D_MASSUSP
!
!GA: SED3D HAS BEEN MODIFIED TO ALLOW TWO SEDIMENT CLASSES
          IF(INFOGR) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SED3D'
            CALL SED3D(S3D_MASBED,S3D_MASBED0, S3D_MASDEP,
     &                 S3D_EPAI,S3D_CONC,
     &                 T2_01%R,
     &                 NPOIN2,S3D_NCOUCH,
     &                 AT,VOLU2D%R,
     &                 S3D_CFDEP,S3D_EPAICO%R,S3D_EPAINCO%R,S3D_MIXTE)
!
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SED3D'
          ENDIF
!
        ENDIF
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIL3D'
        CALL BIL3D(LT,NIT_ORI,MESH3D%IKLBOR%I,NPTFR2,NETAGE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIL3D'
!
      ENDIF
!
! COMPARES WITH REFERENCE FILE
!
      IF(VALID.AND.T3D_FILES(T3DRES)%NAME(1:1).NE.' ') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_VALIDA'
        CALL BIEF_VALIDA(TRAV3,TEXTP3,
     &                   T3D_FILES(T3DREF)%LU,T3D_FILES(T3DREF)%FMT,
     &                   VARSO3,TEXT3,
     &                   T3D_FILES(T3DRES)%LU,T3D_FILES(T3DRES)%FMT,
     &                   MAXVA3,NPOIN3,LT,NIT_ORI,ALIRE3D)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_VALIDA'
      ELSEIF(VALID.AND.T3D_FILES(T3DRES)%NAME(1:1).EQ.' '.AND.
     &       LT.EQ.NIT_ORI) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) ' '
        WRITE(LU,*) 'NO VALIDATION IF NO'
        WRITE(LU,*) '3D RESULT FILE!!!'
      ENDIF
!
!
! CHECKS VALUES SHARED BETWEEN SUBDOMAINS
!
!     CALL CHECK_DIGITS(H ,T2_01,MESH2D)
!     CALL CHECK_DIGITS(U ,T3_01,MESH3D)
!     CALL CHECK_DIGITS(V ,T3_01,MESH3D)
!     CALL CHECK_DIGITS(W ,T3_01,MESH3D)
!     IF(NTRAC.GT.0) THEN
!       DO ITRAC=1,NTRAC
!         CALL CHECK_DIGITS(TA%ADR(ITRAC)%P,T3_01,MESH3D)
!       ENDDO
!     ENDIF
!
#if defined COMPAD
      CALL AD_TELEMAC3D_TIMESTEP_END
#endif
!
      IF(LT.LT.NIT) GO TO 700
! END OF TIME LOOP
!
!=======================================================================
! THE TIME LOOP ENDS HERE
!=======================================================================
!
      IF(PASS.NE.1) THEN
        IF(LISTIN) WRITE(LU,19)
19      FORMAT(/,1X,'END OF TIME LOOP',////)
      ENDIF
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC3D_END
#endif
!
      RETURN
      END

