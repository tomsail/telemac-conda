!                   *****************
                    SUBROUTINE PREADV
!                   *****************
!
     &(W,WS,ZPROP,ISOUSI,LT,VOLU,VOLUN)
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    PREPARES THE ADVECTION STEP BY COMPUTING THE
!+                PARAMETERS COMMON TO ALL THE VARIABLES TO ADVECT.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  JMH
!+        07/08/2008
!+
!+   CALLS CHARAC INSTEAD OF CARACT
!
!history  JMH
!+        13/08/2008
!+
!+   IMMEDIATE INTERPOLATION IN CHARAC
!
!history  JMH
!+        29/06/2009
!+
!+   POINT TO POINT FLUXES COMPUTED IN FLODEL
!
!history  JMH
!+        18/08/2009
!+
!+   UCONVC AND VCONVC FOR ADVECTION FIELD
!
!history  JMH
!+        16/02/2010
!+
!+   COMPUTES ZCHAR TO CALL CHARAC
!
!history  JM HERVOUET (LNHE)     ; JM JANIN (LNH)
!+        26/04/2010
!+        V6P0
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
!history  J-M HERVOUET (LNHE)
!+        24/01/2012
!+        V6P2
!+   Adaptations to tetrahedra. Building FLOPAR only done with ADV_LPO
!+   advection schemes.
!
!history  J-M HERVOUET (LNHE)
!+        06/04/2012
!+        V6P2
!+   Specific treatment for LT=0 suppressed.
!
!history  J-M HERVOUET (LNHE)
!+        02/05/2013
!+        V6P3
!+   Was named PRECON, renamed PREADV for Python scanning sake.
!
!history  J-M HERVOUET (LNHE)
!+        08/01/2015
!+        V7P0
!+   Changing the velocities to be advected with characteristics when
!+   there are more than 2 sub-iterations.
!
!history  J-M HERVOUET (LNHE)
!+        16/01/2015
!+        V7P0
!+   Block of advected variables with characteristics changed in the
!+   case of more than 2 subiterations.
!
!history  A. LEROY (EDF LAB, LNHE)
!+        28/08/2015
!+        V7P1
!+   Add the option OPTSOU to treat sources as a dirac (OPTSOU=2) or
!+   not (OPTSOU=1).
!
!history  J-M HERVOUET (LNHE)
!+        24/06/2016
!+        V7P2
!+   Adding a banner when TRIDW3 is called, to understand where
!+   information on a solver comes from.
!
!history  J-M HERVOUET (LNHE)
!+        06/10/2017
!+        V7P3
!+   Formula for calling MAMURD matrix simplified, N and PSI are no
!+   longer distinguished.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ISOUSI         |-->| RANK OF CURRENT SUB-ITERATION
!| LT             |-->| CURRENT TIME STEP NUMBER
!| VOLU           |-->| VOLUMES (INTEGRAL OF BASES) AT NEW TIME STEP
!| VOLUN          |-->| VOLUMES (INTEGRAL OF BASES) AT OLD TIME STEP
!| WS             |<->| W VELOCITY IN TRANSFORMED MESH
!| ZPROP          |<->| TRANSFORMED ZPROP, TEMPORARILY PUT IN MESH3D%Z
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_PREADV => PREADV
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, ONLY : MESH3D,FLUINT,FLUEXT,AT,INFOGR,
     &                                   FLUEXTPAR,UCONV,VCONV,T3_01,
     &                                   T3_02,T3_03,T3_04,T3_05,
     &                                   NETAGE,NPLAN,Z,OPTSUP,FN3D,
     &                                   NELEM3,IELM3,IELM2V,
     &                                   SVIDE,MSK,MASKEL,MASK_3D,
     &                                   LIHBOR,NPTFR2,DT,MESH2D,GRAPRD,
     &                                   SIGMAG,T2_01,NPOIN2,NPOIN3,
     &                                   DM1,GRAZCO,FLBOR,PLUIE,RAIN,
     &                                   FLODEL,FLOPAR,FLULIM,MTRA1,
     &                                   N_ADV,BYPASS,WEL,MMURD,MURD_TF,
     &                                   WSCONV,VOLU2D,NONHYD,GRADEB,
     &                                   ITURBV,MTRA2,FC3D,IT1,IT2,IT3,
     &                                   IT4,TRAV3,ZCONV,IKLE2,UCONVC,
     &                                   VCONVC,OPT_HNEG,WCONV,ZCHAR,
     &                                   NELEM2,MSUPG,UNSV2D,NSCE,
     &                                   SOURCES,SEM2D,UNSV3D,GRADZF,
     &                                   SEM3D,DSSUDT,OPTBAN,INFOGR,
     &                                   SLVPRO,S3D_AGGLOW,NGAUSS,
     &                                   OPTCHA,
     &                                   U,UN,V,VN,WN,NSOUSI,OPTSOU,
     &                                   ITURBH
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT)         :: WS,ZPROP
      TYPE(BIEF_OBJ), INTENT(IN)            :: VOLU,VOLUN
      TYPE(BIEF_OBJ), INTENT(INOUT), TARGET :: W
!
      INTEGER, INTENT(IN) :: ISOUSI,LT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ::I,IS,IP,IWS,NSEG3D,OPT_TRID
      CHARACTER(LEN=16) FORMUL
      CHARACTER(LEN=8) OPER
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!=======================================================================
!
!     MESH MODIFIED TO BE EQUIVALENT TO THE DEPTH USED IN THE 2D
!     CONTINUITY EQUATION, TO CALL : FLUX3D, VECTOR, MATRIX
!
!     ZPROP IS TEMPORARILY PUT IN MESH3D%Z
      SAVEZ=>MESH3D%Z%R
      MESH3D%Z%R=>ZPROP%R
      NSEG3D=MESH3D%NSEG
!
!=======================================================================
!
! COMPUTES INTERNAL AND EXTERNAL FLUXES AND ADVECTION FIELDS
!
!=======================================================================
!
      CALL FLUX3D
     & (FLUINT,FLUEXT,FLUEXTPAR,UCONV,VCONV,T3_02,
     &  NPLAN,IELM3,IELM2V,SVIDE,MESH3D,
     &  MSK,MASKEL,MASK_3D,LIHBOR%I,KENT,NPTFR2,DT,VOLU,VOLUN,MESH2D,
     &  SIGMAG,NPOIN2,NPOIN3,DM1,GRAZCO,FLBOR,
     &  PLUIE,RAIN,FLODEL,OPT_HNEG,FLULIM,
     &  (N_ADV(ADV_LPO).GT.0.OR.N_ADV(ADV_LPO_TF).GT.0),
     &  BYPASS,N_ADV,WEL)
!
!=======================================================================
!   COMPUTES (DZW*)JH,IV+1/2 AND ACCUMULATES IN WSCONV
!=======================================================================
!
!     HARDCODED OPTION !!!!!!!!!!!!
!
!     2: DIVERGENCE-FREE FLUXES OBTAINED BY MODIFYING VERTICAL FLUXES
!
!     3: DIVERGENCE-FREE FLUXES OBTAINED BY MODIFYING ALL FLUXES
!        WITH THE HELP OF WCONV AND A PRESSURE EQUATION
!
      OPT_TRID=2
!
      IF(OPT_TRID.EQ.2) THEN
        CALL TRIDW2(WSCONV,VOLU,VOLUN,SEM2D,FLUINT,FLUEXT,SOURCES,
     &              NSCE,NETAGE,NPOIN2,DT,UNSV2D,MESH2D,OPTSOU)
      ELSEIF(OPT_TRID.EQ.3) THEN
!       OTHERWISE WCONV DONE IN WAVE_EQUATION
        IF(LT.EQ.0) CALL OS('X=Y     ',X=WCONV,Y=W)
        IF(INFOGR) CALL MITTIT(22,AT,LT)
        CALL TRIDW3(WSCONV,T3_01,T3_02,T3_03,T3_04,T3_05,MTRA1%D,
     &              VOLU,VOLUN,U,UCONV,VCONV,WCONV,DT,NPOIN3,SIGMAG,
     &              OPTBAN,MESH3D,MTRA1,MASKEL,NPOIN2,T2_01,NPLAN,
     &              FLUEXT,NSCE,SOURCES,RAIN,PLUIE,FLUINT,NETAGE,UNSV2D,
     &              SVIDE,NELEM2,MSK,N_ADV,VOLU2D,INFOGR,DSSUDT,IELM3,
     &              DM1,GRAZCO,MESH2D,SEM3D,NELEM3,GRADZF,OPTSOU)
      ENDIF
!
!=======================================================================
!     FOR DEBUGGING: SUMMARY OF ADVECTED VARIABLES AND THEIR SCHEME
!=======================================================================
!
!     DO I=1,15
!       IF(N_ADV(I).GT.0) THEN
!         DO IS=1,N_ADV(I)
!           WRITE(LU,*) 'ADVECTION OF ',
!    &                  BL_FN%ADR(LIST_ADV(IS,I))%P%NAME,
!    &                  ' BY SCHEME ',I
!         ENDDO
!       ENDIF
!     ENDDO
!
!=======================================================================
!     PREPARES ADVECTION BY MURD METHOD
!     STORAGE IS ALWAYS EBE
!=======================================================================
!
      IF(N_ADV(ADV_NSC).GT.0.OR.N_ADV(ADV_PSI).GT.0) THEN
!
        FORMUL = 'MAMURD 2        '
        CALL MATRIX
!                                                           !!!
     &  (MMURD,'M=N     ',FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,WEL,
     &   UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!       HERE THE BYPASS IS NOT OPTIONAL, OTHERWISE
!       THE SCHEMES ARE NOT MASS-CONSERVATIVE
!       IF(BYPASS) THEN
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.IELM3.EQ.41) THEN
!         NOT PROGRAMMED YET FOR TETRAHEDRA !!!!!!!!!!
          CALL BYPASS_CRUSHED_POINTS_EBE(VOLU,VOLUN,
     &                                   MMURD%X%R,T3_01,MESH3D,
     &                                   NELEM2,NELEM3,
     &                                   MESH3D%NELMAX,NPLAN,
     &                                   MESH3D%IKLE%I)
!         OFF-DIAGONAL TERMS MODIFIED, SO DIAGONAL CHANGED ACCORDINGLY
          CALL DIAG_MURD(MMURD%D%R,MMURD%X%R,NELEM3,MESH3D%NELMAX,
     &                   NPOIN3,MESH3D%IKLE%I,IELM3,
     &                   BIEF_DIM2_EXT(IELM3,IELM3,1,'Q',MESH3D))
        ENDIF
!       ENDIF
!
      ENDIF
!
!=======================================================================
!     PREPARES ADVECTION BY MURD METHOD IN EDGE-BASED FORM
!     STORAGE IS ALWAYS EDGE-BASED
!=======================================================================
!
      IF(N_ADV(ADV_NSC_TF).GT.0) THEN
!
        FORMUL = 'MAMURD 2        '
        CALL MATRIX
!                                                             !!!
     &  (MURD_TF,'M=N     ',FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,WEL,
     &   UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!
!       FROM 30 SEGMENTS WITH POSITIVE FLUXES, WE GO TO 15 WITH
!       POSITIVE OR NEGATIVE FLUXES (CASE OF PRISMS, OR 12 TO 6
!       FOR TETRAHEDRA)
        DO I=1,NSEG3D
          MURD_TF%X%R(I) = MURD_TF%X%R(I) - MURD_TF%X%R(I+NSEG3D)
        ENDDO
!       CALL BYPASS: OPTIONAL BUT SAVES ITERATIONS
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          CALL BYPASS_CRUSHED_POINTS_SEG(VOLU,VOLUN,
     &                                   MURD_TF%X%R,
     &                                   T3_01,MESH3D,
     &                                   ADV_NSC_TF,NPOIN2,
     &                                   MESH3D%GLOSEG%I,
     &                                   MESH3D%GLOSEG%DIM1,
     &                                   MESH2D%NSEG,NPLAN)
        ENDIF
        IF(NCSIZE.GT.1) THEN
!         ASSEMBLED FORM OF FLUXES STORED IN SECOND PART
!         OF MATRIX WHICH OTHERWISE IS NOT USED
          CALL OV('X=Y     ',X=MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                       Y=MURD_TF%X%R(       1:  NSEG3D),
     &                       DIM1=NSEG3D)
          CALL PARCOM2_SEG(MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MESH2D%NSEG,NPLAN,2,1,MESH2D,2,IELM3)
        ENDIF
!
      ENDIF
!
!=======================================================================
!     PREPARES LEO POSTMA ADVECTION SCHEMES (PRISMS ONLY)
!=======================================================================
!
!     RETRIEVES VERTICAL FLUXES FROM WSCONV
!     VERTICAL FLUXES ARE STORED IN FLODEL AFTER
!     THE HORIZONTAL FLUXES (THERE ARE NSEG*NPLAN HORIZONTAL FLUXES)
!     USEFUL SIZE OF WSCONV IS (NPOIN2,NPLAN-1)
!
!     THE HORIZONTAL FLUXES HAVE ALREADY BEEN DONE IN FLUX3D
!
      IF(N_ADV(ADV_LPO).GT.0.OR.N_ADV(ADV_LPO_TF).GT.0) THEN
        IS=MESH2D%NSEG*NPLAN
        DO IP=1,NPLAN-1
          DO I=1,NPOIN2
            IWS=I+(IP-1)*NPOIN2
!           NOTE 1: WSCONV IS ALREADY ASSEMBLED
!                   USING VOLU2D FLODEL WILL BE THE NON ASSEMBLED FORM
!           NOTE 2: WE COULD KEEP THE ORIGINAL RIGHT HAND SIDE IN
!                   TRIDW2
!           NOTE 3: AGAIN CONVENTION REVERSED, HERE FLOW FROM
!                   POINT 2 (UP) TO POINT 1 (DOWN)
            FLODEL%R(IS+IWS)=-WSCONV%R(IWS)*VOLU2D%R(I)
          ENDDO
        ENDDO
!       CALL BYPASS: OPTIONAL BUT SAVES ITERATIONS
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          CALL BYPASS_CRUSHED_POINTS_SEG(VOLU,VOLUN,
     &                                   FLODEL%R,
     &                                   T3_01,MESH3D,
     &                                   ADV_LPO_TF,NPOIN2,
     &                                   MESH3D%GLOSEG%I,
     &                                   MESH3D%GLOSEG%DIM1,
     &                                   MESH2D%NSEG,NPLAN)
        ENDIF
!       FLOPAR = FLODEL ASSEMBLED IN PARALLEL MODE
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=FLOPAR,Y=FLODEL)
          CALL PARCOM2_SEG(FLOPAR%R,FLOPAR%R,FLOPAR%R,
     &                     MESH2D%NSEG,NPLAN,2,1,MESH2D,1,IELM3)
        ELSE
          DO I=1,FLOPAR%DIM1
            FLOPAR%R(I) = FLODEL%R(I)
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!     PREPARES ADVECTION BY SUPG METHOD
!=======================================================================
!
      IF(N_ADV(ADV_SUP).GT.0) THEN
!
        IF(OPTSUP(1).EQ.2) THEN
!         HORIZONTAL UPWIND (HERE UPWIND COEFFICIENT=CFL)
          FORMUL = 'MAUGUG2         '
          CALL MATRIX
     &    (MSUPG,'M=N     ',FORMUL,IELM3,IELM3,0.5D0*DT,SVIDE,SVIDE,
     &     SVIDE,UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!         MSUPG IS SYMMETRICAL
        ELSEIF(OPTSUP(1).EQ.1) THEN
!         HORIZONTAL UPWIND (HERE UPWIND COEFFICIENT=1)
          FORMUL = 'MAUGUG1         '
          CALL MATRIX
     &    (MSUPG,'M=N     ',FORMUL,IELM3,IELM3,1.D0,SVIDE,SVIDE,
     &     SVIDE,UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!         MSUPG IS NOT SYMMETRICAL
        ELSEIF(OPTSUP(1).NE.0) THEN
          WRITE(LU,*) 'UNEXPECTED VALUE OF OPTSUP AND PREADV'
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       MSUPG TRANSFORMED INTO NON SYMMETRICAL MATRIX
        IF(OPTSUP(1).EQ.2) THEN
          CALL OM('M=X(M)  ', M=MSUPG, MESH=MESH3D)
          OPER='M=M+N   '
        ELSEIF(OPTSUP(1).EQ.1) THEN
          OPER='M=M+N   '
        ELSE
          OPER='M=N     '
        ENDIF
!
!       ADDS CENTRED ADVECTION TERM
!
        FORMUL = 'MATVGR          '
        FORMUL(8:8) = '2'
        CALL MATRIX
     &  (MSUPG,OPER,FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,SVIDE,
     &   UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!
!       VERTICAL UPWIND (SUBROUTINE UPWIND EXPECTS SYMMETRICAL MATRICES)
!       HERE UPWIND COEFFICIENT = 1, BUT WSCONV USED INSTEAD OF W.
!       WITH TETRAHEDRONS UPWINDING IS DONE IN 3D, SO NO NEED
!       TO CALL THIS UPWIND ON THE VERTICAL.
!
        IF(IELM3.EQ.41) THEN
          CALL UPWIND(MSUPG,WSCONV,1.D0,MESH2D,MESH3D,NPLAN)
        ENDIF
!
      ENDIF
!
!=======================================================================
!
!     RESTORES MESH3D%Z
!
      MESH3D%Z%R=>SAVEZ
!
!=======================================================================
!
!     COMPUTES DELTAZ*WSTAR (IN WS) AT NODES
!
      CALL WSTAR(WS,WSCONV,Z,NPOIN2,NPLAN)
!
!=======================================================================
!
!     COMPUTES W FROM  (DZW*)JH,IV+1/2
!
!        (WITH HYDROSTATIC ASSUMPTION, W IS NEVER USED,
!                  IT IS DONE HERE FOR OUTPUTS)
!        HOWEVER IT IS ALWAYS USED WITH THE K-EPSILON OR K-OMEGA MODELS
!
      IF(.NOT.NONHYD) THEN
        IF(((LT/GRAPRD)*GRAPRD.EQ.LT.AND.LT.GE.GRADEB).OR.
     &      (ITURBV.EQ.3.OR.ITURBH.EQ.3.OR.
     &       ITURBV.EQ.7.OR.ITURBH.EQ.7)) THEN
          CALL WSTARW(W,WSCONV,T3_03%R,T3_04%R,T3_05%R)
        ENDIF
      ENDIF
!
!=======================================================================
!
! ADVECTION BY METHOD OF CHARACTERISTICS
!
!=======================================================================
!
      IF(N_ADV(ADV_CAR).GT.0) THEN
!
!       NOTES:
!
!       IN BLOCK FN3D THERE IS U,V,W INSTEAD OF UN,VN,WN
!       BECAUSE ADVECTION IS DONE FOR THE NEXT TIME STEP
!
!       IT IS MORE COMPLICATED WITH SUB-ITERATIONS:
!
        IF(NSOUSI.GT.1.AND.FN3D%ADR(1)%P%NAME(1:1).EQ.'U') THEN
!         IF THE VELOCITIES ARE ADVECTED THEY ARE THE FIRST IN THE BLOCK
          IF(ISOUSI.NE.NSOUSI) THEN
!           IN THE INITIALISATION, ONLY U, V AND W ARE BUILT
            IF(LT.GT.0) THEN
              FN3D%ADR(1)%P=>UN
              FN3D%ADR(2)%P=>VN
              IF(NONHYD) FN3D%ADR(3)%P=>WN
            ENDIF
          ELSE
            FN3D%ADR(1)%P=>U
            FN3D%ADR(2)%P=>V
            IF(NONHYD) FN3D%ADR(3)%P=>W
          ENDIF
        ENDIF
!
!       FN3D IS THE BLOCK OF VARIABLES ADVECTED WITH
!       SCHEME ADV_CAR (SEE POINT_TELEMAC3D)
!
!       WITH MERCATOR PROJECTION, UCONVC AND VCONVC HAVE BEEN DIVIDED
!       BY THE COSINUS OF LATITUDE, BECAUSE WE WORK HERE ON X AND Y
!       WHICH ARE DISTORTED BY THE PROJECTION. UNLIKE IN 2D
!       UCONVC AND VCONVC ARE NOT MULTIPLIED BY THE COSINUS AFTER
!       USE BECAUSE THE SINGLE USE IS THERE.
!
        CALL CHARAC(FN3D,FC3D,FC3D%N,UCONVC,VCONVC,WS,WS,ZCHAR,ZCHAR,
     &              DT,MESH3D%IFABOR,IELM3,NPOIN2,NPLAN,1,1,
     &              MSK,MTRA2%X,MTRA2%D,MTRA2%D,TRAV3,
     &              IT1%I,IT2%I,IT2%I,IT3%I,IT4%I,IT2%I,
     &              MESH3D,NELEM2,MESH2D%NELMAX,IKLE2,MESH2D%SURDET,
     &              MTRA1,SEM3D,SLVPRO,S3D_AGGLOW,INFOGR,NGAUSS,UNSV3D,
     &              OPTCHA,SIGMA=.TRUE.)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

