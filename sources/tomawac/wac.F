!                   **************
                    SUBROUTINE WAC
!                   **************
!
     &     (PART, NIT_ORI)
!
!**********************************************************************
!     TOMAWAC   V7P3
!**********************************************************************
!
!     brief    MAIN SUBROUTINE OF TOMAWAC
!     +               SOLVES THE EQUATION FOR THE
!     +               DIRECTIONAL WAVE SPECTRUM
!
!     history  J-M HERVOUET (EDF - LNHE)
!     +        29/01/2013
!     +        V6P3
!     +Radiation stresses for Telemac now computed independently of the
!     +   printouts on results file.
!     +   Call to tomawac_constants moved to lecdon_tomawac.
!
!     history  J-M HERVOUET (EDF - LNHE)
!     +        22/03/2013
!     +        V6P3
!     +   New arguments DIRMOY_TEL,HM0_TEL, TPR5_TEL for transmission to
!     +   Sisyphe through Telemac-2D or 3D. Values computed in case of triple
!     +   coupling.
!
!     history  E. GAGNAIRE-RENOU (EDF - LNHE)
!     +        12/09/2014
!     +        V7P0
!     +   New arguments for SEMIMP for wave-blocking effects
!     +   Call the new routine LIMITE
!
!     history  J-M HERVOUET (EDF - LNHE)
!     +        15/09/2014
!     +        V7P0
!     +   Initialisation of SUSOLD and STRA34 if .NOT.VENT
!     +   (a former overlooked bug)
!     +   PERCOU_WAC now used (correction by C. Villaret (HR+EDF)
!     +   and Pablo*(Tassi+Santoro)).
!
!     history  C VILLARET (HRW+EDF) & J-M HERVOUET (EDF - LNHE)
!     +        18/09/2014
!     +        V7P0
!     +   Adding the variable ORBVEL_TEL in argument (orbital velocity)
!     +   for sending it back to the calling program.
!
!     history Y AUDOUIN (LNHE)
!     +       25/05/2015
!     +       V7P0
!     +       Modification to comply with the hermes module
!
!     history A JOLY (LNHE)
!     +       18/05/2017
!     +       V7P3
!     + Possibility to read a text file with coordinates of the nodes
!     +       for which a spectra will be written.
!
!     history A JOLY (LNHE)
!     +       18/05/2017
!     +       V7P3
!     +    Moved LIMWAC to the end of the time stepping operations to
!     +       better impose dirac boundary conditions.
!
!     history A JOLY (EDF-LNHE)
!     +       18/05/2017
!     +       V7P3
!     +  In semimp, new condition to stop source terms being added to
!     +       open boundaries.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     | PART     |-->| -1: NO COUPLING
!     |          |   |  0: COUPLING WITH TELEMAC (INITIALISATION)
!     |          |   |  1: COUPLING WITH TELEMAC (LOOP OVER TIME STEPS)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE COUPLE_MOD
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_WAC => WAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      INTEGER,           INTENT(IN)      :: PART
      INTEGER,           INTENT(IN),OPTIONAL :: NIT_ORI
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      DOUBLE PRECISION DT_MIN,DT_MAX
      INTEGER          DUMMY,LT_WAC, IREC, IERR, I
!
      INTEGER LT1
      INTEGER DATE(3),TIME(3),IP
!
      INTEGER ADC,MDC,JDC
      INTEGER(KIND=K8) HDC
      LOGICAL IMPRES, DEBRES
      INTEGER TOTAL_ITER
!-----------------------------------------------------------------------
!
      IF(PART.EQ.WAC_FULL_RUN.OR.
     &     PART.EQ.WAC_CPL_INIT.OR.
     &     PART.EQ.WAC_CPL_RUN.OR.
     &     PART.EQ.WAC_API_INIT) THEN
#if defined COMPAD
        CALL AD_TOMAWAC_BEGIN
        CALL AD_TOMAWAC_INITIALISATION_BEGIN
#endif
!=====C
!     1  C INITIALISATIONS DES VARIABLES LOCALES
!=====C======================================
!     COUPLAGE : verification des conditions pour le couplage
!     TELEMAC-TOMAWAC
!
        IF(PART.EQ.WAC_CPL_INIT.OR.PART.EQ.WAC_CPL_RUN) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'ENTERING TOMAWAC, WITH T_TEL=',
     &         CPL_WAC_DATA%AT_TEL,
     &         'NIT_TEL',CPL_WAC_DATA%NIT_TEL
          IF(MAREE.OR.COUSTA.OR.DONTEL) THEN
            WRITE(LU,*) ''
            WRITE(LU,*) '***************************************'
            WRITE(LU,*) ' ATTENTION : COUPLING TELEMAC-TOMAWAC :'
            WRITE(LU,*) ' CURRENT/WATER LEVEL FILE CANNOT BE    '
            WRITE(LU,*) ' USED AS INPUT FILE.                   '
            WRITE(LU,*) '         END OF THE COMPUTATION        '
            WRITE(LU,*) '***************************************'
            CALL PLANTE(1)
            STOP
          ENDIF
!
          DT_MAX=MAX(DT,CPL_WAC_DATA%DT_TEL*CPL_WAC_DATA%PERCOU_WAC)
          DT_MIN=MIN(DT,CPL_WAC_DATA%DT_TEL*CPL_WAC_DATA%PERCOU_WAC)
!
          IF(ABS(NINT(DT_MAX/DT_MIN)-DT_MAX/DT_MIN).GT.1.D-6) THEN
            WRITE(LU,*) ''
            WRITE(LU,*) '***************************************'
            WRITE(LU,*) ' ATTENTION : COUPLING TELEMAC-TOMAWAC :'
            WRITE(LU,*) ' THE CHOSEN TIME STEPS ARE NOT MULTIPLE'
            WRITE(LU,*) ' OF EACH OTHER.                        '
            WRITE(LU,*) '         END OF THE COMPUTATION        '
            WRITE(LU,*) '***************************************'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!
!.....1.4 INITIALISATION DES TABLEAUX DATE ET TIME
!
        ADC=INT(DDC*1.D-8)
        MDC=INT(DDC*1.D-6)
        JDC=INT(DDC*1.D-4)
        HDC=INT(DDC*1.D-2,KIND=K8)
        DATE(1)=ADC
        DATE(2)=MDC-100*ADC
        DATE(3)=JDC-100*MDC
        TIME(1)=INT(HDC-100.D0*DBLE(JDC))
        TIME(2)=INT(DDC-100.D0*DBLE(HDC))
        TIME(3)=0
!
!.....1.5 READ ASCII FILES WITH COORDINATES OF POINTS FOR WHICH
!     SPECTRA WILL NEED TO BE READ
!
        IF((NAMWXY(1:1).NE.' ').AND.
     &       (PART.EQ.WAC_FULL_RUN.OR.PART.EQ.WAC_CPL_INIT.OR.
     &       PART.EQ.WAC_API_INIT))THEN
          IF(DEBUG.GT.0)
     &         WRITE(LU,*)'CALLING READ_SPECTRA_COORDS FOR PRINTOUT'
          CALL READ_SPECTRA_COORDS(LUWXY, NPLEO, XLEO, YLEO)
          IF(DEBUG.GT.0)WRITE(LU,*)'BACK FROM  READ_SPECTRA_COORDS'
!     ALLOCATE THE MEMORY FOR THE NODE NUMBERS OF THE SPECTRA
          IF(ALLOCATED(NOLEO))DEALLOCATE(NOLEO)
          ALLOCATE(NOLEO(NPLEO))
        ENDIF
!
!.....1.5 READ ASCII FILES WITH COORDINATES OF POINTS FOR WHICH
!     SPECTRA WILL NEED TO BE READ
!
        IF((NAMIXY(1:1).NE.' ').AND.
     &       (PART.EQ.WAC_FULL_RUN.OR.PART.EQ.WAC_CPL_INIT.OR.
     &       PART.EQ.WAC_API_INIT))THEN
          IF(DEBUG.GT.0)
     &    WRITE(LU,*)'CALLING READ_SPECTRA_COORDS FOR IMPOSED SPECTRA'
          CALL READ_SPECTRA_COORDS(LUIXY, NPSPE,XSPE,YSPE)
          IF(DEBUG.GT.0)WRITE(LU,*)'BACK FROM  READ_SPECTRA_COORDS'
        ENDIF
!
        IF (INCLUS(COUPLING,'TOMAWAC2').OR.
     &      INCLUS(COUPLING,'TOMAWACT3D2')) THEN
          CALL GET_MESH_NPTIR(WAC_FILES(WACGEO)%FMT,
     &       WAC_FILES(WACGEO)%LU,NPTIR,IERR)
        ENDIF
!=====C
!     2 LECTURE DES CONDITIONS LIMITES ET INDICES DES POINTS FRONTIERES
!=====C================================================================
!
!     COUPLAGE TELEMAC-TOMAWAC : initialisation
!
        IF(PART.EQ.WAC_FULL_RUN.OR.PART.EQ.WAC_CPL_INIT.OR.
     &       PART.EQ.WAC_API_INIT) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WAC_INIT'
          CALL WAC_INIT (PART,IMPRES,DEBRES,DATE,TIME)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WAC_INIT'
        ENDIF
      ENDIF ! PART.LE.2
!
!     COUPLAGE TELEMAC-TOMAWAC
      IF(PART.NE.WAC_CPL_INIT.AND.PART.NE.WAC_API_INIT) THEN
!
!     Preparation au couplage : on calcule le nombre de cycle
!     de la boucle en temps, selon la duree du pas de temps dans
!     Telemac (CPL_WAC_DATA%DT_TEL) et dans Tomawac (DT).
!     On assigne a DUMMY la valeur originaire du nombre de pas
!     de temps specifie dans le fichier .cas de Tomawac.
!
        IF(PART.EQ.WAC_CPL_RUN) THEN
          AT=CPL_WAC_DATA%AT_TEL
          DUMMY=NIT
          IF(DT.GT.CPL_WAC_DATA%DT_TEL*CPL_WAC_DATA%PERCOU_WAC) THEN
            WRITE(LU,*) ''
            WRITE(LU,*) '***************************************'
            WRITE(LU,*) ' ATTENTION : COUPLING TELEMAC-TOMAWAC :'
            WRITE(LU,*) ' TOMAWAC TIME STEP CAN NOT BE GREATER  '
            WRITE(LU,*) ' THAN TELEMAC TIME STEP                '
            WRITE(LU,*) '         END OF THE COMPUTATION        '
            WRITE(LU,*) '***************************************'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(DT.LE.CPL_WAC_DATA%DT_TEL*CPL_WAC_DATA%PERCOU_WAC) THEN
            NIT = NINT(CPL_WAC_DATA%DT_TEL*CPL_WAC_DATA%PERCOU_WAC/DT)
          ENDIF

!       RECEIVE DATA FROM TOMAWAC
!                                                       WAC2
          IF (INCLUS(COUPLING,'TOMAWAC2').OR.
     &        INCLUS(COUPLING,'TOMAWACT3D2')) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE RECEIVE_COUPLE'
            CALL RECEIVE_COUPLE(1,NPOIN2,NVARTEL2TOM,TEL2TOM)
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE RECEIVE_COUPLE'
            CPL_WAC_DATA%U_TEL => U_TEL
            CPL_WAC_DATA%V_TEL => V_TEL
            CPL_WAC_DATA%H_TEL => H_TEL
            CPL_WAC_DATA%FX_WAC => FX_WAC
            CPL_WAC_DATA%FY_WAC => FY_WAC
            CPL_WAC_DATA%UV_TEL => UV_TEL
            CPL_WAC_DATA%VV_TEL => VV_TEL
            CPL_WAC_DATA%DIRMOY_TEL => DIRMOY_TEL
            CPL_WAC_DATA%HM0_TEL => HM0_TEL
            CPL_WAC_DATA%TPR5_TEL => TPR5_TEL
            CPL_WAC_DATA%ORBVEL_TEL => ORBVEL_TEL
            IF(INCLUS(COUPLING,'TOMAWACT3D2')) THEN
              CPL_WAC_DATA%WIPDXW => WIPDX
              CPL_WAC_DATA%WIPDYW => WIPDY
              CPL_WAC_DATA%USTW => USTW
              CPL_WAC_DATA%VSTW => VSTW
              CPL_WAC_DATA%WSTW => WSTW
              CPL_WAC_DATA%ZTELW => ZTELW
    !          CPL_WAC_DATA%NZW = NPLAN
              CPL_WAC_DATA%WIPW => WIPW
              CPL_WAC_DATA%FDXW => FDXW
              CPL_WAC_DATA%FDYW => FDYW
              CPL_WAC_DATA%FBXW => FBXW
              CPL_WAC_DATA%FBYW => FBYW
              CPL_WAC_DATA%CFWCW => CFWCW
              CPL_WAC_DATA%FDKW => FDKW
              CPL_WAC_DATA%FWX => FWXW
              CPL_WAC_DATA%FWY => FWYW
            ENDIF
          ENDIF
        ENDIF!     Fin COUPLAGE
!
!     COUPLAGE TELEMAC-TOMAWAC : LT est defini comme le numero de pas
!     de temps effectifs de TOMAWAC. La variable LT_WAC
!     compte les pas de temps de chaque boucle, meme
!     quand TOMAWAC est appele par TELEMAC
!
!=====C
!     11 C BOUCLE EN TEMPS PRINCIPALE.
!=====C============================
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'TIME LOOP BEGINNING'
        DO LT_WAC=1,NIT
!
!------------------------------------------------------------------
!
#if defined COMPAD
          CALL AD_TOMAWAC_TIMESTEP_BEGIN
#endif
!
!------------------------------------------------------------------
!
!.....11.1 AFFECTATION DE LA DATE DE FIN DU PAS DE TEMPS COURANT.
!
          AT=AT+DT
!     TODO: Calcul de LT (NOTE JMH: WHY NOT LT=LT+1 ?)
          LT=NINT((AT-AT0)/DT)
!
!     Fin COUPLAGE
!
          CALL IMPR(LISPRD,LT,AT,LT,3)
!
!     11.2 AFFECTATION DES CONDITIONS AUX LIMITES.
!
          IF (AVANT) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LIMWAC'
            CALL LIMWAC(F,       FBOR, NPTFR, NDIRE,  NF, NPOIN2,
     &      KENT, PRIVE, NPRIV, WAC_FILES(IMPSPE))
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LIMWAC'
          ENDIF
!
!     11.2 MISE A ZERO DU SPECTRE SUR LES POINTS OU PROF < PROMIN
!     I GUESS I DO NOT HAVE THOSE ANYMORE....SINCE I CHANGED DEPTH
          IF (.NOT.PROINF) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRETE'
            IF (ECRET) CALL ECRETE(SF%R,SDEPTH%R,NPOIN2,NDIRE,NF,
     &                             PROMIN)
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRETE'
          ENDIF
!
          IF (MAREE.OR.COURAN) LT1=(LT/LAM)*LAM
!
!......11.3 UPDATING DEPTH AND CURRENTS
!
!     COUPLING TELEMAC-TOMAWAC OR CURRENTS AND/OR DEPTH IN A FILE
!     THEY ARE UPDATED HERE.
!
          IF(MAREE.AND.LT.EQ.LT1.OR.
     &         (PART.EQ.WAC_CPL_RUN.AND.LT_WAC.EQ.1)) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORMAR'
            CALL CORMAR(PART, CPL_WAC_DATA%U_TEL,
     &           CPL_WAC_DATA%V_TEL,CPL_WAC_DATA%H_TEL, 
     &        CPL_WAC_DATA%UV_TEL, CPL_WAC_DATA%VV_TEL)
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORMAR'
            DO IP=1,NPOIN2
              IF(DEPTH(IP).LT.PROMIN) DEPTH(IP)=0.9D0*PROMIN
            ENDDO
!
!......11.3.1 PREPARING PROPAGATION (METHOD OF CHARACTERISTICS).
!
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INIPHY'
            CALL INIPHY(XK, CG, B, NPOIN2, NF)
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INIPHY'
!
            IF(PROP) THEN
!
              CALL IMPR(LISPRD,LT,AT,LT,1)
              CALL IMPR(LISPRD,LT,AT,LT,2)
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREPRO 2'
!
              CALL PREPRO( STSDER, STSTOT, IKLE2, IBOR, ELT, ETA, FRE,
     &             XK,     CG,     ITR01, NPOIN3, NPOIN2, NELEM2,
     &             NDIRE,  NF,    COURAN.OR.PART.EQ.WAC_CPL_RUN)
!     Fin COUPLAGE
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREPRO 2'
            ENDIF
          ENDIF
!     Fin cycle IF((MAREE.AND.LT.EQ.LT1).OR.
!     (PART.EQ.WAC_CPL_RUN.AND.LT_WAC.EQ.1))
!------------------------------------------------------------------
!     V6P2 Diffraction : diffraction term calculation
          IF(DIFFRA.GT.0) THEN
            IF(LT.EQ.NPTDIF)THEN
              WRITE(LU,*)'*********************************'
              WRITE(LU,*)'DIFFRACTION IS TAKEN INTO ACCOUNT'
              WRITE(LU,*)'*********************************'
            ENDIF
            IF(LT.GE.NPTDIF) THEN
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREDIF'
              CALL PREDIF( STSDER, STSTOT, IKLE2, IBOR,   ELT,    ETA,
     &        XK,     CG,     ITR01, NPOIN3, NPOIN2, NELEM2, NDIRE,
     &        NF, COURAN.OR.PART.EQ.WAC_CPL_RUN, F,
     &        RX, RY,     RXX,    RYY,    NEIGB)
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREDIF'
            ENDIF
          ENDIF
!     V6P2 End diffraction
!-------------------------------------------------------------------
!
!.....11.3 PROPAGATION (INTERPOLATION AU PIED DES CARACTERISTIQUES).
!
          IF(PROP) THEN
            CALL IMPR(LISPRD,LT,AT,LT,5)
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROPA',LT
            CALL PROPA(F, B,    ELT, ETA, FRE ,NPOIN3, NPOIN2,
     &           NDIRE,NF, COURAN.OR.PART.EQ.WAC_CPL_RUN, TSDER)
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROPA'
          ENDIF
!
!.....11.4 ECRETE PAR RAPPORT A UN SPECTRE LIMITE APRES PROPAGATION.
!
          IF(SDSCU.EQ.1) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LIMITE'
            CALL LIMITE(F, FREQ, NPOIN2, NDIRE, NF)
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LIMITE'
          ELSEIF(SDSCU.GT.2) THEN
            WRITE(LU,*) ''
            WRITE(LU,*) '**************************'
            WRITE(LU,*) '   UNKNOWN OPTION FOR     '
            WRITE(LU,*) '    STRONG CURRENTS       '
            WRITE(LU,*) '**************************'
            CALL PLANTE(1)
            STOP
          ELSEIF(SDSCU.EQ.2.AND..NOT.TSOU) THEN
            WRITE(LU,*) ''
            WRITE(LU,*) '****************************'
            WRITE(LU,*) '  CONSIDERATION OF SOURCE   '
            WRITE(LU,*) '   TERMS MANDATORY FOR      '
            WRITE(LU,*) '    OPTION 2 FOR STRONG     '
            WRITE(LU,*) '         CURRENTS           '
            WRITE(LU,*) '****************************'
            CALL PLANTE(1)
            STOP
          ENDIF
!
!.....11.5 INTEGRATION DES TERMES SOURCES.
!
          IF(TSOU) THEN
            CALL IMPR(LISPRD,LT,AT,NSITS,4)
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SEMIMP'
            CALL SEMIMP( F,    CF,     XK, NF, NDIRE, NPOIN2,
     &      IANGNL, TSTOT, TSDER, TOLD,  TNEW, TRA35, TRA36,
     &      TRA37,  TRA38, TRA39, T1,    T2,   T3,     T4,
     &      MDIA,  IANMDI, COEMDI, FBOR, PART)

            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SEMIMP'
          ENDIF
!
!     11.8 AFFECTATION DES CONDITIONS AUX LIMITES.
!
          IF(.NOT.AVANT) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LIMWAC'
            CALL LIMWAC(F,   FBOR, NPTFR, NDIRE,  NF, NPOIN2, KENT,
     &      PRIVE, NPRIV,  WAC_FILES(IMPSPE))
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LIMWAC'
          ENDIF
!
!.....10.6 TEST POUR SAVOIR SI ON IMPRIME OU PAS.
!     """""""""""""""""""""""""""""""""""""""""""
          IMPRES=.FALSE.
          DEBRES=.FALSE.
          IF(LT.GE.GRADEB.AND.MOD(LT-GRADEB,GRAPRD).EQ.0) IMPRES=.TRUE.
          IF(LT.EQ.GRADEB) DEBRES=.TRUE.
!
          IF(IMPRES) THEN
!
!.....11.7 PASSAGE EN FREQUENCE ABSOLUE.
!     """""""""""""""""""""""""""""""""""""""""""""
!
            IF(COURAN.OR.PART.EQ.WAC_CPL_RUN) THEN
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TRANSF'
              IF(LT.GE.GRADEB.AND.MOD(LT-GRADEB,GRAPRD).EQ.0) THEN
                CALL TRANSF(TSTOT, F,   XK, ITR11, ITR12,ITR13, TRA40,
     &               TRA41, NPOIN2, NDIRE,NF)
              ENDIF
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TRANSF'
            ENDIF
!
!.....11.9 IMPRESSION (EVENTUELLE) DES VARIABLES SUR LE MAILLAGE 2D.
!
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DUMP2D'
            IF(COURAN.OR.PART.EQ.WAC_CPL_RUN) THEN
              CALL DUMP2D(TSTOT, NPOIN3*NF)
            ELSE
              CALL DUMP2D(F, NPOIN3*NF)
            ENDIF
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DUMP2D'
!
            IF(NAMRES.NE.' ') THEN
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_DESIMP'
              CALL BIEF_DESIMP(FMTRES,VARSOR, NPOIN2,LURES,AT, LT,
     &             LISPRD,GRAPRD, SORLEO,SORIMP,MAXVAR,TEXTE,GRADEB,
     &         GRADEB)
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_DESIMP'
            ENDIF
!
!.....11.10 IMPRESSION (EVENTUELLE) DES SPECTRES DIRECTIONNELS.
!
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRSPE'
            IREC = (LT - GRADEB)/GRAPRD
            IF(COURAN.OR.PART.EQ.WAC_CPL_RUN) THEN
              CALL ECRSPE(TSTOT, NDIRE, NF, NPOIN2, IREC,  TSDER,
     &        NOLEO, NPLEO, DEBRES,DATE,  TIME, MESH%KNOLG%I, MESH)
            ELSE
              CALL ECRSPE(F,     NDIRE, NF, NPOIN2, IREC,  TSDER,
     &        NOLEO, NPLEO, DEBRES,DATE,  TIME, MESH%KNOLG%I, MESH)
            ENDIF
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRSPE'
!
          ENDIF
          IF(PART.EQ.WAC_CPL_RUN.AND.LT_WAC.EQ.NIT) THEN
            IF(CPL_WAC_DATA%COUPL3D) THEN
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TRANSF'
              CALL TRANSF(TSTOT, F,    XK,    ITR11, ITR12,ITR13,
     &        TRA40, TRA41, NPOIN2, NDIRE, NF)
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TRANSF'
!     DISSIPATION DUE TO DEPTH-INDUCED WAVE BREAKING
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FDISS3D'
              CALL FDISS3D ( CPL_WAC_DATA%FDXW%R, CPL_WAC_DATA%FDYW%R,
     &             NPOIN2, XK, NDIRE, F, NF)
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FDISS3D'

!     DISSIPATION DUE TO BOTTOM FRICTION
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FBOTT3D'
              CALL FBOTT3D
     &             ( CPL_WAC_DATA%FBXW%R, CPL_WAC_DATA%FBYW%R, F,
     &             NPOIN2, XK, NDIRE,NF)
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FBOTT3D'
!     The velocity STOKES DRIFT HORIZONTAL COMPONENTS
!     ONE SHOULD CALL TRANSF BEFORE !!!
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE UVSTOKES'
              CALL UVSTOKES( CPL_WAC_DATA%USTW%R, CPL_WAC_DATA%VSTW%R,
     &             CPL_WAC_DATA%WSTW%R, TSTOT,  NPOIN2,XK,ZF, NDIRE,
     &             CPL_WAC_DATA%ZTELW%R, CPL_WAC_DATA%NZW,NF)
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE UVSTOKES'

!     The wave induced pressure J
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WIPJ'
              CALL WIPJ (CPL_WAC_DATA%WIPW%R, TSTOT, NPOIN2, XK,
     &        CPL_WAC_DATA%WIPDXW%R,CPL_WAC_DATA%WIPDYW%R, NDIRE, NF)
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WIPJ'
!
!     WAVE ENHANCED MIXING
              CALL TOTNRJ( VARIAN, TSTOT, NF, NDIRE, NPOIN2)
              DO IP=1,NPOIN2
                IF (VARIAN(IP).GE.0) THEN
                  HM0(IP)=4.D0*SQRT(VARIAN(IP))
                ELSE
                  HM0(IP)=0
                ENDIF   
              ENDDO
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FDISSK'
              CALL FDISSK( CPL_WAC_DATA%FDKW%R, NPOIN2, NDIRE, F,
     &             CPL_WAC_DATA%ZTELW%R,CPL_WAC_DATA%NZW, HM0, TRA37,
     &             NF)
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FDISSK'

!     FRICTION COEFFICIENT - WAVES+CURRENTS
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VITFON'
              CALL VITFON(VIFOND, TSTOT, XK, NF,
     &             NPOIN2,NDIRE)
!     MODIFIED COEFFICIENT TO TAKE INTO ACCOUNT WAVES+CURRENTS
              CALL TETMOY( TRA40, TSTOT, NDIRE, NF  , NPOIN2)
              IF(TRIGO) THEN
                DO IP=1,NPOIN2
                  TRA40(IP)=(PISUR2-TRA40(IP))
                ENDDO
              ENDIF
              IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FRIC3D'
              CALL FRIC3D
     &             ( CPL_WAC_DATA%CFWCW, NPOIN2, TRA40,
     &             CPL_WAC_DATA%U_TEL, CPL_WAC_DATA%V_TEL, VIFOND)
              IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FRIC3D'
!     SURFACE STRESS DUE TO WIND INPUT ENERGY AND WHITECAPPING
              IF(VENT) THEN
                IF(SVENT.EQ.1) THEN
                  IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WINDISS1'
                  CALL WINDISS1( CPL_WAC_DATA%FWX%R, CPL_WAC_DATA%FWY%R,
     &                 NPOIN2, XK, NDIRE, F, NF)
                  IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WINDISS1'
                ELSEIF(SVENT.EQ.2) THEN
                  IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WINDISS2'
                  CALL WINDISS2( CPL_WAC_DATA%FWX%R, CPL_WAC_DATA%FWY%R,
     &                 NPOIN2, XK, NDIRE, F, NF)
                  IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WINDISS2'
                ELSEIF(SVENT.EQ.3) THEN
                  IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WINDISS3'
                  CALL WINDISS3( CPL_WAC_DATA%FWX%R, CPL_WAC_DATA%FWY%R,
     &                 NPOIN2, XK, NDIRE, F, NF)
                  IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WINDISS3'
                ENDIF
                IF((SMOUT.EQ.1).OR.SMOUT.EQ.2) THEN
                  CALL MOUDISS( CPL_WAC_DATA%FWX%R, CPL_WAC_DATA%FWY%R,
     &                  NPOIN2, XK, NDIRE, F, NF)
                ENDIF
              ENDIF

            ENDIF               !IF (CPL_WAC_DATA%COUPL3D)
!
!     RADIATION STRESSES COMPUTED HERE FOR TELEMAC INDEPENDENTLY
!     OF THE PRINTS TO RESULT FILE, WHICH IS NOT MANDATORY
!
!     STSTOT WORK ARRAY (ABSOLUTE FREQUENCY) IN ALL THIS IF BLOCK
            CALL TRANSF(TSTOT, F, XK, ITR11, ITR12,ITR13, TRA40, TRA41,
     &           NPOIN2,NDIRE, NF)
            CALL RADIAT(CPL_WAC_DATA%FX_WAC%R, CPL_WAC_DATA%FY_WAC%R,
     &           XK, TSTOT, CG, TSDER, TRA36, TRA37, TRA38, TRA39)
            IF(INCLUS(COUPLING,'SISYPHE').OR.INCLUS(COUPLING,'GAIA'))
     &      THEN
!     3 VARIABLES THAT WILL BE TRANSMITTED TO SISYPHE OR GAIA
!     ALL THIS IF BLOCK ADAPTED FROM DUMP2D
!     MEAN DIRECTION
              CALL TETMOY(CPL_WAC_DATA%DIRMOY_TEL%R, TSTOT, NDIRE,NF,
     &         NPOIN2)
            IF(TRIGO) THEN
              DO IP=1,NPOIN2
                CPL_WAC_DATA%DIRMOY_TEL%R(IP) =
     &               (PISUR2-CPL_WAC_DATA%DIRMOY_TEL%R(IP))*RADDEG
              ENDDO
            ELSE
              DO IP=1,NPOIN2
                CPL_WAC_DATA%DIRMOY_TEL%R(IP) =
     &               CPL_WAC_DATA%DIRMOY_TEL%R(IP)*RADDEG
              ENDDO
            ENDIF
!     SIGNIFICANT WAVE HEIGHT
            CALL TOTNRJ(TRA37, TSTOT, NF, NDIRE, NPOIN2)
            DO IP=1,NPOIN2
              CPL_WAC_DATA%HM0_TEL%R(IP)=4.D0*SQRT(TRA37(IP))
            ENDDO
!     TPR5
            CALL FPREAD(CPL_WAC_DATA%TPR5_TEL%R, TSTOT,
     &           NF, NDIRE, NPOIN2, 5.D0)
            DO IP=1,NPOIN2
              CPL_WAC_DATA%TPR5_TEL%R(IP)=
     &      1.D0/MIN(MAX(CPL_WAC_DATA%TPR5_TEL%R(IP),FREQ(1)),FREQ(NF))
            ENDDO
!     NEAR BED ORBITAL VELOCITY
            CALL VITFON(CPL_WAC_DATA%ORBVEL_TEL%R, TSTOT, XK,  NF,
     &           NPOIN2,NDIRE)
          ENDIF
          IF (ALG_DISLODGE) THEN
            CALL VITFON(CPL_WAC_DATA%ORBVEL_TEL%R, TSTOT, XK,  NF,
     &           NPOIN2,NDIRE)
          ENDIF
!
        ENDIF
!
!------------------------------------------------------------------
!
#if defined COMPAD
        CALL AD_TOMAWAC_TIMESTEP_END
#endif
!
!------------------------------------------------------------------
!
      ENDDO                     ! LT_WAC
!
!     Fin COUPLAGE : fin du cycle IF(PART.NE.0)
      ENDIF
!
!=====C
!     12 C IMPRESSIONS GLOBALES (EVENTUELLES) EN FIN DE CALCUL.
!=====C=====================================================
!
!     COUPLAGE TELEMAC-TOMAWAC : reset de la variable NIT (nombre
!     de pas de temps de TOMAWAC indique dans la fichier .cas)
      IF(PART.EQ.WAC_CPL_RUN) NIT=DUMMY
      IF(PRESENT(NIT_ORI)) THEN
        TOTAL_ITER = NIT_ORI
      ELSE
        TOTAL_ITER = NIT
      ENDIF
!     Fin COUPLAGE
!
      IF(ABS(AT-AT0-TOTAL_ITER*DT).LT.1.D-6) THEN
!
        IF(GLOB) THEN
          CALL IMPR(LISPRD,NIT,AT,NIT,6)
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOR3D'
          CALL SOR3D(F,  NDIRE, NF, NPOIN2,  VENT,
     &         COURAN.OR.PART.EQ.WAC_CPL_RUN,
     &         MAREE.OR.PART.EQ.WAC_CPL_RUN, TITCAS, TSDER, MESH3D)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOR3D'
        ENDIF
!
!----------------------------------------------------------------------
!
!     VALIDATION DES RESULTATS SUR LE FICHIER DE REFERENCES
!
        IF(VALID) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BIEF_VALIDA'
          CALL BIEF_VALIDA(BST1,  TEXTE,  LUREF, FMTREF,
     &         VARSOR, TEXTE, LURES, FMTRES,
     &         MAXVAR,NPOIN2,   NIT,    NIT,ALIRE)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_VALIDA'
        ENDIF
!
      ENDIF
!
!------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TOMAWAC_END
#endif
!
!------------------------------------------------------------------
!------------------------------------------------------------------------
!
!   SEND DATA BACK TO TELEMAC
!
!                                                       WAC2
      IF (INCLUS(COUPLING,'TOMAWAC2').OR.
     &      INCLUS(COUPLING,'TOMAWACT3D2')) THEN
        !CONVERT DIRECTIONS
        DO I=1,NPOIN2
          COSDIR_TEL%R(I) = COS(DEGRAD*DIRMOY_TEL%R(I))
          SINDIR_TEL%R(I) = SIN(DEGRAD*DIRMOY_TEL%R(I))
        ENDDO
        CALL SEND_COUPLE(2,NPOIN2,NVARTOM2TEL,TOM2TEL)
      ENDIF
!
      RETURN
      END

