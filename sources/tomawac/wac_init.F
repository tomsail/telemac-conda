      SUBROUTINE WAC_INIT
     & (PART,IMPRES,DEBRES,DATE,TIME)
!
!***********************************************************************
! TOMAWAC   V7P3
!***********************************************************************
!
!brief    MAIN SUBROUTINE OF TOMAWAC (INTIALISATION PART)
!+               SOLVES THE EQUATION FOR THE
!+               DIRECTIONAL WAVE SPECTRUM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE COUPLE_MOD
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_WAC_INIT => WAC_INIT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(INOUT) :: IMPRES, DEBRES
      INTEGER, INTENT(IN) :: PART
      INTEGER, INTENT(INOUT) :: DATE(3),TIME(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IREC, K, IDIRE, IFREQ, IP
      DOUBLE PRECISION :: LAMBD0
      DOUBLE PRECISION VITVEN,VITMIN
!
!-----------------------------------------------------------------------
!
!     MESH ORGANISATION - 2D LEVEL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECLIM POUR MESH2D'
      CALL LECLIM(LIFBOR,ITR31,ITR31,ITR31,FBOR,
     &            TSDER,TSDER,TSDER,TSDER,TSDER,TSDER,
     &            NPTFR,'WAC',.FALSE., FMTGEO, LUGEO,
     &            KENT,KENTU,KSORT,KADH,KLOG,KINC,ITR31,
     &            MESH,BOUNDARY_COLOUR)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE LECLIM'
!
!
!     LAMBD00 is not initialised yet and can produce some Nan
!     Is it useful to have it as a keyword like in telemac2D ?
      LAMBD0=0.D0
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF POUR MESH2D'
      CALL INBIEF(LIFBOR,KLOG,SITR31,SITR32,SITR33,
     &            LVMAC,IELM2,LAMBD0,SPHE,MESH,STSDER,STSTOT,1,1,EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE INBIEF'
!
!     EXTENSION OF IKLE2 (SEE CALL TO POST_INTERP IN PROPA)
!
      CALL BUILD_IKLE_EXT(IKLE_EXT%I,IKLE_EXT%DIM1,IKLE2,NELEM2)
!
!     MESH ORGANISATION - 3D LEVEL
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECLIM POUR MESH3D'
      CALL LECLIM (LIFBOR,ITR31,ITR31,ITR31,FBOR,
     &             TSDER,TSDER,TSDER,TSDER,TSDER,TSDER,
     &             NPTFR,'WAC',.FALSE., FMTGEO, LUGEO,
     &             KENT,KENTU,KSORT,KADH,KLOG,KINC,ITR31,MESH3D)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE LECLIM'
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF POUR MESH3D'
      CALL INBIEF(LIFBOR,KLOG,SITR31,SITR32,SITR33,
     &            LVMAC,IELM3,LAMBD0,SPHE,MESH3D,
     &            STSDER,STSTOT,1,1,EQUA,MESH2D=MESH)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INBIEF'
!
!     3D IFABOR
!
      CALL IFABTOM(MESH3D%IFABOR%I,NELEM2,NDIRE-1)
!
!-----------------------------------------------------------------------
!
!     V6P2 Diffraction : FREEMESH METHOD
!
      IF(DIFFRA.GT.0) THEN
        IF(NCSIZE.GT.1.AND.OPTDER.EQ.1) THEN
          WRITE(LU,*) ''
          WRITE(LU,*) '***************************************'
          WRITE(LU,*) ' ATTENTION : DIFFRACTION               '
          WRITE(LU,*) ' OPTION FOR THE SECOND DERIVATIVES     '
          WRITE(LU,*) ' SET TO 2 IN PARALLEL MODE             '
          WRITE(LU,*) '***************************************'
        ENDIF
        WRITE(LU,*) '****************************************'
        WRITE(LU,*) 'DIFFRACTION IS TAKEN INTO ACCOUNT      '
        WRITE(LU,*) 'STARTING FROM TIME STEP ',NPTDIF
        IF(DIFFRA.EQ.1) THEN
          WRITE(LU,*) 'MILD SLOPE EQUATION FORMULATION'
        ELSE
          WRITE(LU,*)'REVISED MILD SLOPE EQUATION FORMULATION'
        ENDIF
        WRITE(LU,*) '****************************************'
!
!    SETS UP OF THE SUBDOMAINS FOR THE FREEMSESH METHOD
!    AND CALCULATES THE INVERSE MATRICES FOR EACH SUBDOMAIN
!
        IF(OPTDER.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FREEMESH'
          CALL FRMSET( NEIGB, NPOIN2, NELEM2, IKLE2,
     &                 RK,    RX,     RY,     RXX, RYY)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FREEMESH'
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
!
! LECTURE DE LA COTE DU FOND (ZF) SUR LE FICHIER DE GEOMETRIE
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FONSTR'
      CALL FONSTR(ST1,SZF,ST2,ST3, LUGEO, FMTGEO, LUFON, NAMFON,
     &            MESH,  1.D0,.TRUE., 0, NAMES_PRIVE,SPRIVE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FONSTR'
!
! CORRECTION EVENTUELLE DES VALEURS DU FOND (OU CALCUL DU FOND SI CELA
! N'A PAS ETE FAIT DANS FONSTR)
! EN STANDARD, TOM_CORFON NE FAIT RIEN (ATTENTION, ALLER CHERCHER
! LE TOM_CORFON DE TOMAWAC).
! DANS LE CAS DE COUPLAGE AVEC TELEMAC, ON LIT LE FOND A PARTIR DU
! MODELE TELEMAC ET TOM_CORFON N EST PAS UTILISE
!
      IF(PART.EQ.WAC_FULL_RUN.OR.PART.EQ.WAC_CPL_INIT.OR.
     &   PART.EQ.WAC_API_INIT)THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TOM_CORFON'
        CALL TOM_CORFON
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TOM_CORFON'
      ENDIF
!
!     CALCUL DE LA PROFONDEUR D'EAU (TABLEAU DEPTH)
!
      IF(.NOT.PROINF) THEN
        DO IP=1,NPOIN2
          DEPTH(IP)=MAX(ZREPOS-ZF(IP),0.9D0*PROMIN)
        ENDDO
!     DO NOT UNDERSTAND 0.9
      ENDIF
!
!-----------------------------------------------------------------------
!
!     PREPARATION DES SORTIES GRAPHIQUES
!
!     CREATION DU JEU DE DONNEES POUR UN FORMAT DE FICHIER FORMAT_RES.
!     LE JEU DE DONNEES EST CREE DANS LE FICHIER NRES, ET EST DEFINI
!     PAR UN TITRE ET LES VARIABLES A ECRIRE.
!
      IF(NAMRES.NE.' ') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WRITE_HEADER'
        CALL WRITE_HEADER(FMTRES, ! FORMAT FICHIER RESULTAT
     &                    LURES,  ! LU FICHIER RESULTAT
     &                    TITCAS,     ! TITRE DE L'ETUDE
     &                    MAXVAR,     ! MAX VARIABLES SORTIE
     &                    TEXTE,      ! NOMS VARIABLES SORTIE
     &                    SORLEO)     ! SORTIE OU PAS DES VARIABLES
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WRITE_HEADER'
!
!     ECRITURE DU MAILLAGE DANS LE FICHIER SORTIE :
!     SI ON EST ON PARALLEL, FAUT L'INDIQUER VIA NCSIZE ET NPTIR.
!     LES AUTRES INFORMATIONS SONT DANS MESH.
!     EN PLUS : DATE/TEMPS DE DEPART ET LES COORDONNEES DE L'ORIGINE.
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WRITE_MESH'
        CALL WRITE_MESH(FMTRES, ! FORMAT FICHIER RESULTAT
     &                  LURES,  ! LU FICHIER RESULTAT
     &                  MESH,
     &                  1,             ! NOMBRE DE PLAN /NA/
     &                  DATE,          ! DATE DEBUT
     &                  TIME,          ! HEURE DEBUT
     &                  STRA40, STRA41,! WORKING ARRAYS
     &                  NCSIZE.GT.1, NPTIR,
     &                  NGEO=LUGEO,
     &                  GEOFORMAT=FMTGEO)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WRITE_MESH'
      ENDIF
!
!-----------------------------------------------------------------------
! CONDITIONS INITIALES
!=====C INITIALISATION DES VECTEURS DE DISCRETISATION, DU COURANT,
!  2  C DU VENT ET DU SPECTRE DE VARIANCE.
!=====C===========================================================
      LT=0
      DTSI=DT/NSITS
!-----------------------------------------------------------------------
!     INITIALISES TETA
!     BY DEFAULT THE DIRECTIONS OF PROPAGATION ARE EVENLY DISTRIBUTED
      DO IDIRE = 1,NDIRE+1
        TETA(IDIRE) = (IDIRE-1)*DEUPI/NDIRE
      ENDDO
!
!-----------------------------------------------------------------------
!
!     INITIALISES FREQ AND DFREQ, THE FREQUENCIES OF PROPAGATION
!     ARE DISTRIBUTED USING AN EXPONENTIAL LAW
!
      DO IFREQ = 1,NF
        FREQ(IFREQ) = F1*RAISF**(IFREQ-1)
      ENDDO
!-----------------------------------------------------------------------
!
!     INITIALISING DZHDT (BUT MAYBE REDONE IN LECSUI OR CONDIW)
!
      DO IP=1,NPOIN2
        DZHDT(IP)=0.D0
      ENDDO

!      RECEIVE DATA FROM TELEMAC
!                                                       WAC2
      IF ((PART.EQ.0).AND.INCLUS(COUPLING,'TOMAWAC2').OR.
     &      INCLUS(COUPLING,'TOMAWACT3D2')) THEN
        CALL RECEIVE_COUPLE(1,NPOIN2,NVARTEL2TOM,TEL2TOM)
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
!
      IF(SUIT) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECSUI'
        CALL LECSUI(F, NDIRE, NF, NPOIN2,
     &              VENT,COURAN.OR.PART.EQ.WAC_CPL_INIT ,
     &              LUPRE,FMTPRE, MAREE.OR.PART.EQ.WAC_CPL_INIT,TSDER)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LECSUI'
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CONDIW'
        CALL CONDIW(PART,
     &       CPL_WAC_DATA%U_TEL, CPL_WAC_DATA%V_TEL, CPL_WAC_DATA%H_TEL,
     &       CPL_WAC_DATA%UV_TEL, CPL_WAC_DATA%VV_TEL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CONDIW'
!       DEPTH MAY BE MODIFIED IN CONDIW
        IF(.NOT.PROINF) THEN
          DO IP=1,NPOIN2
            IF(DEPTH(IP).LT.PROMIN) DEPTH(IP)=0.9D0*PROMIN
          ENDDO
        ENDIF
      ENDIF
!
      IF(RAZTIM) AT=0.D0
      IF(PART.EQ.WAC_CPL_INIT) AT=CPL_WAC_DATA%AT_TEL
!
      AT0=AT
!
      IF(.NOT.PROINF) THEN
        DO IP=1,NPOIN2
          IF(DEPTH(IP).LE.0.D0) THEN
            WRITE(LU,*) ''
            WRITE(LU,*) '**************************'
            WRITE(LU,*) ' ! NEGATIVE WATER DEPTH ! '
            WRITE(LU,*) '   END OF THE COMPUTATION '
            WRITE(LU,*) '**************************'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDIF
!
!=====C
!  4  C CALCULS PREPARATOIRES POUR INTERACTIONS NON-LINEAIRES.
!=====C=======================================================
!.....DIA method (Hasselmann et al., 1985)
!
      IF(STRIF.EQ.1) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRENL1'
        CALL PRENL1( IANGNL, COEFNL, NDIRE , NF , RAISF , XLAMD )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRENL1'
!
!.....MDIA method (Tolman, 2004)
!
      ELSEIF (STRIF.EQ.2) THEN
!.....Setting parametres for MDIA
        XLAMDI(1)=0.075D0
        XMUMDI(1)=0.023D0
        XLAMDI(2)=0.219D0
        XMUMDI(2)=0.127D0
        XLAMDI(3)=0.299D0
        XMUMDI(3)=0.184D0
        XLAMDI(4)=0.394D0
        XMUMDI(4)=0.135D0
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRENL2'
        DO K=1,MDIA
          CALL PRENL2(IANMDI(1,1,K),COEMDI(1,K),NDIRE,NF,RAISF,
     &                XLAMDI(K),XMUMDI(K))
        ENDDO
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRENL2'
!
!.....GQM method (Lavrenov, 2001)
!
      ELSEIF(STRIF.EQ.3) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRENL3'
        CALL PRENL3
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRENL3'
      ENDIF
!
      IF(STRIA.EQ.2) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREQT2'
        CALL PREQT2
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREQT2'
      ENDIF
!
!=====C INITIALISATION DE LA CONTRAINTE DE HOULE, PUIS CALCUL DES
!  5  C VITESSE DE FROTTEMENT U*, RUGOSITE Z0 ET DIRECTION INITIALES.
!=====C==============================================================
!
!.....5.1 INITIALISATION DE LA CONTRAINTE DE HOULE INITIALE.
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""
      CALL OV('X=0     ', X=TAUWAV, DIM1=NPOIN2)
!
!.....5.2 CALCUL DE U* ET Z0 SELON LA METHODE CONSIDEREE.
!     """""""""""""""""""""""""""""""""""""""""""""""""""
      IF (VENT) THEN
        IF (SVENT.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR1'
          CALL USTAR1( USOLD, Z0OLD, TAUWAV, NPOIN2   )
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR1'
        ELSEIF (SVENT.GE.2) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR2'
          CALL USTAR2(USOLD, NPOIN2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR2'
        ELSEIF (SVENT.EQ.0.AND.LVENT.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR2'
          CALL USTAR2(USOLD, NPOIN2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR2'
        ELSEIF (SVENT.EQ.0.AND.LVENT.EQ.0.AND.SMOUT.EQ.2) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR2'
          CALL USTAR2(USOLD, NPOIN2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR2'
        ELSE
          WRITE(LU,*) 'PB IN WAC : WIND PRESENT, BUT SVENT NOT CORRECT'
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
!       USOLD
        CALL OS('X=0     ',X=SUSOLD)
!       USNEW
        CALL OS('X=0     ',X=SUSNEW)
      ENDIF
!
!.....5.3 CALCUL DE LA DIRECTION DU VENT
!     """"""""""""""""""""""""""""""""""
      VITMIN=1.D-3
      IF (VENT) THEN
        DO IP=1,NPOIN2
          VITVEN=SQRT(UV(IP)**2+VV(IP)**2)
          IF (VITVEN.GT.VITMIN) THEN
            TWOLD(IP)=ATAN2(UV(IP),VV(IP))
          ELSE
            TWOLD(IP)=0.D0
          ENDIF
        ENDDO
      ENDIF
!
!=====C
!  6  C INITIALISATION DE CERTAINS TABLEAUX UTILES.
!=====C============================================
!
!     COUPLAGE TELEMAC-TOMAWAC si PART=0
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INITAB'
      CALL INITAB( IBOR, IFABOR, NELEM2, PART)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INITAB'
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE IMPR'
      CALL IMPR(LISPRD,LT,AT,LT,3)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE IMPR'
      IF (PART.EQ.WAC_CPL_INIT) THEN
!     ONLY FOR COUPLAGE BUT CAN INDUCE PROBLEM IN TELEMAC2D IF NOT DONE.
        CALL OS('X=0     ',X=CPL_WAC_DATA%FX_WAC)
        CALL OS('X=0     ',X=CPL_WAC_DATA%FY_WAC)
      ENDIF
!=====C
!  7  C AFFECTATION DES CONDITIONS AUX LIMITES A L'INSTANT INITIAL.
!=====C============================================================
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LIMWAC'
      CALL LIMWAC
     &(F   , FBOR , NPTFR,  NDIRE,    NF, NPOIN2,
     & KENT, PRIVE,  NPRIV,  WAC_FILES(IMPSPE))
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LIMWAC'
!
!=====C CALCUL DES NOMBRES D'ONDE (XK), DE LA VITESSE DE GROUPE (CG) ET
!  8  C DU FACTEUR DE PASSAGE (B) EN SPECTRE DE VARIANCE EN (FR,TETA).
!=====C=================================================================
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INIPHY'
      CALL INIPHY ( XK, CG, B, NPOIN2, NF )
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INIPHY'
!
!=====C
!  8b C MISE A ZERO DU SPECTRE SUR LES POINTS OU PROF < PROMIN
!=====C=======================================================
!
      IF(.NOT.PROINF) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRETE'
        IF (ECRET) CALL ECRETE(F, DEPTH, NPOIN2, NDIRE, NF, PROMIN)
!   SHOULD THINK OT THAT ....
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRETE'
      ENDIF
!
!=====C
!  9  C SORTIES GRAPHIQUES (EVENTUELLES) A L'ETAT INITIAL.
!=====C===================================================
!
!.....9.1 CHOIX DES POINTS DE SORTIE DU SPECTRE DIRECTIONNEL.
!
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROXIM'
      IF(NPLEO.GT.0) THEN
        CALL PROXIM(NOLEO, XLEO, YLEO, X, Y, NPLEO, NPOIN2,
     &        IKLE2,NELEM2,NELEM2)
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROXIM'
!
!.....9.2 TEST POUR SAVOIR SI ON IMPRIME OU PAS.
!
      IMPRES=.FALSE.
      DEBRES=.FALSE.
      IF(LT.EQ.GRADEB) THEN
        IMPRES=.TRUE.
        DEBRES=.TRUE.
      ENDIF
!
      IF(IMPRES) THEN
!
!.....9.3 IMPRESSION (EVENTUELLE) DES VARIABLES SUR LE MAILLAGE 2D.
!
!
!       THE VARIABLES ARE COMPUTED HERE WITH THE ORIGINAL SPECTRUM
!       DONE IN SPEINI, THERE IS NO CALL TRANSF BEFORE BECAUSE
!       CURRENTS ARE NOT TAKEN INTO ACCOUNT IN SPEINI
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DUMP2D'
        CALL DUMP2D(F, NPOIN3*NF)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DUMP2D'
!
        IF(NAMRES(1:1).NE.' ') THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_DESIMP'
          CALL BIEF_DESIMP(FMTRES,VARSOR, NPOIN2,LURES,AT,
     &              LT,LISPRD,GRAPRD, SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_DESIMP'
        ENDIF
!
!.....9.4 IMPRESSION (EVENTUELLE) DES SPECTRES DIRECTIONNELS.
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRSPE'
        IREC = (LT - GRADEB)    ! Should be 0
        CALL ECRSPE(F,     NDIRE , NF  , NPOIN2, IREC, TSDER, NOLEO,
     &              NPLEO, DEBRES, DATE, TIME, MESH%KNOLG%I ,MESH)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRSPE'
      ENDIF
!
!     CASE OF TRIPLE COUPLING, INITIAL CONDITIONS
!
!                                                       WAC2
      IF (INCLUS(COUPLING,'TOMAWAC2').OR.
     &      INCLUS(COUPLING,'TOMAWACT3D2')) THEN
        CALL OS('X=0     ',X=FX_WAC)
        CALL OS('X=0     ',X=FY_WAC)
      ENDIF
      IF ((INCLUS(COUPLING,'SISYPHE').OR.INCLUS(COUPLING,'GAIA'))
     &  .AND.PART.EQ.WAC_CPL_INIT) THEN
!       3 VARIABLES THAT WILL BE TRANSMITTED TO SISYPHE OR GAIA
!       ALL THIS IF BLOCK ADAPTED FROM DUMP2D
!       MEAN DIRECTION
        CALL TETMOY(CPL_WAC_DATA%DIRMOY_TEL%R, F, NDIRE, NF, NPOIN2)
        IF(TRIGO) THEN
          DO IP=1,NPOIN2
            CPL_WAC_DATA%DIRMOY_TEL%R(IP) =
     &          (PISUR2-CPL_WAC_DATA%DIRMOY_TEL%R(IP))*RADDEG
          ENDDO
        ELSE
          DO IP=1,NPOIN2
            CPL_WAC_DATA%DIRMOY_TEL%R(IP) =
     &          CPL_WAC_DATA%DIRMOY_TEL%R(IP)*RADDEG
          ENDDO
        ENDIF
!       SIGNIFICANT WAVE HEIGHT
        CALL TOTNRJ(TRA37, F, NF,NDIRE,NPOIN2)
        DO IP=1,NPOIN2
          CPL_WAC_DATA%HM0_TEL%R(IP)=4.D0*SQRT(TRA37(IP))
        ENDDO
!       TPR5
        CALL FPREAD(CPL_WAC_DATA%TPR5_TEL%R, F, NF, NDIRE,
     &              NPOIN2, 5.D0)
        DO IP=1,NPOIN2
          CPL_WAC_DATA%TPR5_TEL%R(IP)=
     &    1.D0/MIN(MAX(CPL_WAC_DATA%TPR5_TEL%R(IP),FREQ(1)),FREQ(NF))
        ENDDO
!       ORBITAL VELOCITY SET TO 0.D0
        CALL OS('X=0     ',X=CPL_WAC_DATA%ORBVEL_TEL)
      ENDIF
!
!=====C
!  10 C PREPARATION DE LA PROPAGATION (REMONTEE DES CARACTERISTIQUES).
!=====C===============================================================
!
      IF(PROP) THEN
!
        CALL IMPR(LISPRD,LT,AT,LT,1)
        CALL IMPR(LISPRD,LT,AT,LT,2)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREPRO 1'
        CALL PREPRO
     & ( STSDER, STSTOT, IKLE2,  IBOR, ELT, ETA, FRE,
     &   XK,  CG,  ITR01, NPOIN3, NPOIN2, NELEM2,
     &   NDIRE,  NF, COURAN.OR.PART.EQ.WAC_CPL_INIT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREPRO 1'
!
      ENDIF
!
!------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TOMAWAC_INITIALISATION_END
#endif
!
!=======================================================================
!
!COUPLAGE : end cycle IF(PART.LE.0) pour couplage avec TELEMAC
      END SUBROUTINE WAC_INIT
