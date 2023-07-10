!                   *****************
                    SUBROUTINE CVDF3D
!                   *****************
!
     &(FD,FC,FN,VISCF,SIGMAF,S0F,YAS0F,S1F,YAS1F,
     & FBORL,FBORF,FBORS,AFBORL,AFBORF,AFBORS,
     & BFBORL,BFBORF,BFBORS,LIFBOL,LIFBOF,LIFBOS,
     & FLUXB,FLUXF,FLUEXT,FLUEXTPAR,FMIN,CLIMIN,FMAX,CLIMAX,
     & SCHCF,SCHDF,SLVDIF,TRBAF,INFOR,NEWDIF,CALFLU,
     & T2_01,T2_03,
     & T3_01,T3_02,T3_03,T3_04,MESH3D,IKLE3,MASKEL,MTRA1,
     & NPTFR3,MMURD,MURD_TF,VOLU,VOLUPAR,VOLUN,VOLUNPAR,
     & NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM3,
     & NPLAN,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     & INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,
     & TRAV3,MESH2D,OPTBAN,TETADI,
     & YAWCHU,WCHU,S3D_WCHU,AGGLOD,NSCE,SOURCES,FSCE,NUMLIQ,DIRFLU,
     & NFRLIQ,VOLUT,ZT,ZPROP,RAIN,PLUIE,PARAPLUIE,TRAIN,
     & FLODEL,FLOPAR,SIGMAG,IPBOT,MAXADV,FLUDPT,FLUDP,FLUER,
     & VOLU2D,V2DPAR,SETDEP,S3D_FLUDPT,S3D_FLUDP,S3D_FLUER,
     & S3D_SETDEP,OPTSOU,ZN,OPTADV,NCO_DIST,
     & NSP_DIST,TB2)
!
!***********************************************************************
! TELEMAC3D   V8P4
!***********************************************************************
!
!brief    SOLVES THE ADVECTION-DIFFUSION STEP.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        01/03/1999
!+        V6P0
!+   FORTRAN95 VERSION
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
!history  J.M. HERVOUET (LNHE)
!+        04/01/2012
!+        V6P2
!+   Call to MURD3D modified
!+
!history  J.M. HERVOUET (LNHE)
!+        17/09/2012
!+        V6P3
!+   S0F%TYPR now restored at the end, for a possible new use if there
!+   iterations for non linearities.
!
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  J.M. HERVOUET (LNHE)
!+        11/03/2014
!+        V6P3
!+   Call to DIFFV removed (other solution prepared to remove
!+   horizontal diffusion if necessary). To remove vertical diffusion
!+   now component TYPR used (with corresponding implementation in
!+   MT02PP.F.
!
!history  J.M. HERVOUET (EDF LAB, LNHE)
!+        29/04/2014
!+        V7P0
!+   Argument S3D_SETDEPadded to diff3d.
!
!history  A. JOLY (EDF LAB, LNHE)
!+        27/08/2015
!+        V7P1
!+   Imposed flowrates on the bed.
!
!history  A. LEROY (EDF LAB, LNHE)
!+        28/08/2015
!+        V7P1
!+   Add the option OPTSOU to treat sources as a dirac (OPTSOU=2) or
!+   not (OPTSOU=1).
!
!history  S. PAVAN & J-M HERVOUET (EDF LAB, LNHE & LHSV)
!+        22/08/2016
!+        V7P2
!+   Adding Predictor-corrector schemes and the LIPS advection scheme.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        16/09/2016
!+        V7P2
!+   Arguments changed in the call to murd3d and murd3d_pos. Work array
!+   TRAV3%ADR(10)%P is already used for S0F2.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        10/09/2017
!+        V7P3
!+   OPTADV transmitted to MURD3D_POS instead of a hardcoded 2.
!+   Adaptations for cases where NELMAX is not equal to NELEM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AFBORF         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE BOTTOM:
!|                |   |  NU*DF/DN = AFBORF*U + BFBORF
!| AFBORL         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE
!|                |   | LATERAL BOUNDARIES:
!|                |   | NU*DF/DN = AFBORL*U + BFBORL
!| AFBORS         |-->| LOGARITHMIC LAW FOR COMPONENT AT THE SURFACE:
!|                |   | NU*DF/DN = AFBORS*U + BFBORS
!| AGGLOD         |-->| MASS-LUMPING IN DIFFUSION
!| BFBORF         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE BOTTOM:
!|                |   |  NU*DF/DN = AFBORF*U + BFBORF
!| BFBORL         |-->| LOGARITHMIC LAW FOR COMPONENT ON THE
!|                |   | LATERAL BOUNDARIES:
!|                |   | NU*DF/DN = AFBORL*U + BFBORL
!| BFBORS         |-->| LOGARITHMIC LAW FOR COMPONENT AT THE SURFACE:
!|                |   | NU*DF/DN = AFBORS*U + BFBORS
!| CALFLU         |-->| INDICATE IF FLUX IS CALCULATED FOR BALANCE
!| CLIMAX         |-->| LOGICAL FOR CLIPPING (MAX VALUE)
!| CLIMIN         |-->| LOGICAL FOR CLIPPING (MIN VALUE)
!| DIRFLU         |-->| TREATMENT OF FLUXES AT THE BOUNDARIES
!| DT             |-->| TIME STEP
!| FBORF          |<->| DIRICHLET CONDITIONS ON F AT THE BOTTOM
!| FBORL          |<->| DIRICHLET CONDITIONS ON F ON LATERAL BOUNDARIES
!| FBORS          |<->| DIRICHLET CONDITIONS ON F AT THE SURFACE
!| FC             |<->| VARIABLE AFTER CONVECTION
!| FD             |<->| VARIABLE AFTER DIFFUSION
!| FLODEL         |-->| FLUXES BY SEGMENT
!| FLOPAR         |-->| FLUXES BY SEGMENT, ASSEMBLED IN PARALLEL
!| FLUDP          |-->| DEPOSITION FLUX (SEDIMENT)
!| FLUDPT         |-->| IMPLICIT DEPOSITION FLUX (SEDIMENT)
!| FLUER          |-->| EROSION FLUX (SEDIMENT)
!| FLUEXT         |-->| OUTPUT FLUX BY NODE
!| FLUEXTPAR      |-->| OUTPUT FLUX BY NODE IN PARALLEL
!| FLUXB          |<->| FLUX FOR F FOR BALANCE
!| FLUXF          |<->| FLUX FOR F
!| FMAX           |-->| MAX CLIPPING VALUE
!| FMIN           |-->| MIN CLIPPING VALUE
!| FN             |<->| VARIABLE F AT TIME N
!| FSCE           |-->| SOURCE TERM OF F
!| IELM2H         |-->| DISCRETISATION TYPE FOR 2D HORIZONTAL MESH
!| IELM2V         |-->| DISCRETISATION TYPE FOR 2D VERTICAL MESH
!| IELM3          |-->| DISCRETISATION TYPE FOR 3D
!| IKLE3          |-->| GLOBAL 3D CONNECTIVITY
!| INCHYD         |-->| IF YES, HYDROSTATIC INCONSISTENCY FILTER
!| INFOR          |-->| INFORMATIONS FOR SOLVERS
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| IT1            |<->| BIEF_OBJ STRUCTURES FOR INTEGER ARRAYS
!| LIFBOF         |<->| TYPE OF BOUNDARY CONDITIONS AT THE BOTTOM
!| LIFBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON LATERAL BOUNDARIES
!| LIFBOS         |<->| TYPE OF BOUNDARY CONDITIONS AT THE SURFACE
!| MASKBR         |-->| 3D MASK ON LATERAL BOUNDARIES
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MAXADV         |-->| MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES
!| MDIFF          |<->| DIFFUSION MATRIX
!| MESH2D         |<->| 2D MESH
!| MESH3D         |<->| 3D MESH
!| MMURD          |<->| NON SYMMETRIC MURD MATRIX
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| MSUPG          |<->| NON SYMMETRIC SUPG MATRIX
!| MTRA1          |<->| 3D WORK MATRIX
!| MTRA2          |<->| 3D WORK MATRIX
!| MURD_TF        |<->| MURD MATRIX FOR TIDAL FLAT
!| NBOR3          |-->| GLOBAL NUMBER OF 3D BOUNDARY POINTS
!| NCO_DIST       |-->| NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NEWDIF         |-->| RECALCULATE OR NOT DIFFUSION MATRIX
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NPTFR3         |-->| NUMBER OF LATERAL BOUNDARY POINTS IN 3D
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| NSP_DIST       |-->| NUMBER OF SUB-STEPS OF DISTRIBUTIVE SCHEMES
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| OPTADV         |-->| ADVECTION SCHEME OPTION, THE MEANING DEPENDS ON
!|                |   | THE SCHEME. IF SCHEME IS SUPG:
!|                |   | 0: NO SUPG UPWIND
!|                |   | 1: CLASSIC SUPG
!|                |   | 2: MODIFIED SUPG
!|                |   | IF SCHEME IS PSI:
!|                |   | 1: EXPLICIT
!|                |   | 2: PREDICTOR-CORRECTOR
!|                |   | 3: SECOND ORDER PREDICTOR-CORRECTOR
!|                |   | 4: LOCALLY IMPLICIT
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| PARAPLUIE      |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
!|                |   | (IN ASSEMBLED MODE IN PARALLEL)
!| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| S0F            |<->| EXPLICIT SOURCE TERM (DIM=F/T)
!| S1F            |<->| IMPLICIT SOURCE TERM (DIM=1/T)
!| S3D_FLUDP      |-->| DEPOSITION FLUX (SEDIMENT) FOR SEDI3D
!| S3D_FLUDPT     |-->| IMPLICIT DEPOSITION FLUX (SEDIMENT) FOR SEDI3D
!| S3D_FLUER      |-->| EROSION FLUX (SEDIMENT) FOR SEDI3D
!| S3D_SETDEP     |-->| INTEGER FOR SETTLING AND DEPOSITION CONVECTION SCHEME FOR SEDI3D
!| SCHCF          |-->| ADVECTION SCHEME OF F
!| SCHDF          |-->| DIFFUSION SCHEME OF F
!| SEM3D          |<->| SECOND MEMBERS (RIGHT HAND SIDE)
!|                |   | FOR THE LINEAR EQUATIONS 3D
!| SIGMAF         |-->| COEFFICIENT OF VISCOSITY REDUCTION
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!| SLVDIF         |-->| SOLVER FOR DIFFUSION OF VELOCITIES
!| SOURCES        |-->| RIGHT HAND SIDE OF CONTINUITY EQUATION WHEN SOURCES
!| SVIDE          |-->| VOID STRUCTURE
!| T2_01          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T2_03          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_01          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_02          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_03          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_04          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| TETADI         |<->| IMPLICITATION RATE FOR DIFFUSION
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| TRAV3          |<->| BLOCK OF 3D BIEF_OBJ STRUCTURES (AT LEAST 10)
!| TRBAF          |-->| TREATMENT ON TIDAL FLATS FOR F
!| VISCF          |<->| VISCOSITY COEFFICIENTS
!|                |   | VISCF(*,1 OR 2) HORIZONTAL VISCOSITY
!|                |   | VISCF(*,3)      VERTICAL VISCOSITY
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| VOLUN          |-->| VOLUME AROUND POINTS AT TIME N
!| VOLUNPAR       |-->| VOLUME AROUND POINTS AT TIME N, IN PARALLEL
!| VOLUPAR        |-->| VOLUME AROUND POINTS AT TIME N+1, IN PARALLEL
!| VOLUT          |<->| VOLUME AFTER SEMI-IMPLICITATION FOR TRACER
!| WCHU           |-->| VELOCITY (NEGATIVE IF SEDIMENT SETTLING VELOCITY)
!| YASEM3D        |-->| IF TRUE, RIGHT HAND SIDE HAS BEEN PARTLY
!|                |   | COMPUTED BEFORE CALLING DIFF3D
!| YAS0F          |-->| LOGICAL TO TAKE INTO ACCOUNT S0F TERM IN DIFF3D
!| YAS1F          |-->| LOGICAL TO TAKE INTO ACCOUNT S1F TERM IN DIFF3D
!| YAWCHU         |-->| LOGICAL TO TAKE INTO ACCOUNT WCHU FOR SEDIMENT
!| ZPROP          |-->| VERTICAL COORDINATES FOR PROPAGATION STEP
!| ZN             |<->| Z SAVED AT TIME T(N)
!| ZT             |<->| Z FOR SUPG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_TELEMAC3D, EX_CVDF3D => CVDF3D
      USE DECLARATIONS_TELEMAC3D, ONLY : KSCE,ISCE,BEDBOU,BEDFLU,MAXFRO
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FD, FC, FN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: S1F, VISCF
      TYPE(BIEF_OBJ), TARGET, INTENT(INOUT)   :: S0F
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: LIFBOL, LIFBOF, LIFBOS
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FBORL, FBORF, FBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: AFBORL, AFBORF, AFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: BFBORL, BFBORF, BFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: FLUEXT,PLUIE,PARAPLUIE
      TYPE(BIEF_OBJ), INTENT(IN)      :: FLUEXTPAR
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAF,FMIN,FMAX,DT,TRAIN
      DOUBLE PRECISION, INTENT(IN)    :: AGGLOD
      DOUBLE PRECISION, INTENT(INOUT) :: FLUXB(*)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUXF,TETADI
      INTEGER, INTENT(IN)             :: SCHCF,SCHDF,TRBAF,NPTFR3,NFRLIQ
      INTEGER, INTENT(IN)             :: NUMLIQ(*),DIRFLU(0:NFRLIQ)
      INTEGER, INTENT(IN)             :: OPTSOU,OPTADV,NCO_DIST,NSP_DIST
      LOGICAL, INTENT(IN)             :: CLIMIN,CLIMAX,RAIN,YAS0F,YAS1F
      LOGICAL, INTENT(IN)             :: INFOR,NEWDIF,CALFLU,MSK,SIGMAG
      TYPE(SLVCFG)                    :: SLVDIF
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,IKLE3,FLODEL,FLOPAR
      TYPE(BIEF_OBJ), INTENT(IN)      :: NBOR3,WCHU,SOURCES,ZPROP
      TYPE(BIEF_OBJ), INTENT(IN)      :: S3D_WCHU
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3_01,T3_02,T3_03,T3_04
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T2_01,T2_03,ZT,TB2
      TYPE(BIEF_OBJ), TARGET, INTENT(INOUT) :: VOLUT
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D
      INTEGER, INTENT(IN)             :: NPOIN3,NPOIN2,MAXADV
      INTEGER, INTENT(IN)             :: IPBOT(NPOIN2)
      INTEGER, INTENT(IN)             :: NPLAN,NELEM3
      INTEGER, INTENT(IN)             :: OPTBAN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MMURD,MURD_TF,MTRA1
      TYPE(BIEF_OBJ), INTENT(IN)      :: VOLUN,VOLUNPAR,VOLUPAR
      TYPE(BIEF_OBJ), TARGET, INTENT(INOUT) :: VOLU
      LOGICAL, INTENT(IN)             :: INCHYD,YASEM3D
      LOGICAL, INTENT(INOUT)          :: YAWCHU
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKPT,MASKBR,SVIDE
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D
      INTEGER, INTENT(IN)             :: IELM3,IELM2H,IELM2V,NSCE
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SEM3D,IT1,TRAV3,MTRA2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MSUPG,MDIFF
      DOUBLE PRECISION, INTENT(IN)    :: FSCE(NSCE)
      TYPE(BIEF_OBJ), INTENT(IN)      :: VOLU2D, V2DPAR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: S3D_FLUDPT,S3D_FLUDP, S3D_FLUER
      INTEGER, INTENT(IN)             :: S3D_SETDEP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUDPT,FLUDP, FLUER
      INTEGER, INTENT(IN)             :: SETDEP
      DOUBLE PRECISION, INTENT(IN)    :: ZN(NPOIN3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,K,IPTFR,IS,I,IIS,PARA,DIM1X,NELMAX,ILIQ
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
      DOUBLE PRECISION STOFD,TETASUPG
      CHARACTER(LEN=1) :: S0FTYPR,NUZTYPR
      TYPE(BIEF_OBJ), POINTER :: VOLUME,S0F2
!
!     FUNCTIONS
!
      DOUBLE PRECISION LAMBDA
!
      LOGICAL YADIRFLU,YASCE,VELOCITY,YARAIN,TRACERS
!
!***********************************************************************
!
      NELMAX=MESH3D%NELMAX
!
!     FOR DIMENSIONING XA AND XB IN MURD3D.
!
      DIM1X=BIEF_DIM2_EXT(IELM3,IELM3,1,'Q',MESH3D)
!
!     SAVING S0F%TYPR AND NUZ%TYPR
!
      S0FTYPR=S0F%TYPR
      NUZTYPR=VISCF%ADR(3)%P%TYPR
!
!     DEALING WITH A VELOCITY ?
!
      VELOCITY=.FALSE.
      IF(FN%NAME(1:1).EQ.'U'.OR.
     &   FN%NAME(1:1).EQ.'V'.OR.
     &   FN%NAME(1:1).EQ.'W') VELOCITY=.TRUE.
!
!     DEALING WITH TRACERS?
!
      TRACERS = .FALSE.
      IF(FN%NAME(1:3).EQ.'TRN') TRACERS = .TRUE.
!
!     EVEN IF.NOT.CALFLU
!
      FLUXF = 0.D0
      DO I=1,MAXFRO+NSCE+1
        FLUXB(I) = 0.D0
      ENDDO
!
!     WITH DISTRIBUTIVE SCHEMES : COMPUTES PRESCRIBED VALUES THAT
!     WILL ENSURE THE CORRECT FLUX (REAL PRESCRIBED VALUES DISCARDED)
!     THESE CORRECTED PRESCRIBED VALUES ARE SET BEFORE ADVECTION
!
!     YADIRFLU=.TRUE. : THERE IS AT LEAST ONE BOUNDARY WITH
!                       TREATMENT OF FLUXES AT BOUNDARIES = 2
      YADIRFLU=.FALSE.
!     DIRFLU DISCARDED FOR VELOCITIES
      IF(NFRLIQ.GT.0.AND..NOT.VELOCITY) THEN
        DO K=1,NFRLIQ
          IF(DIRFLU(K).EQ.2) YADIRFLU=.TRUE.
        ENDDO
      ENDIF
!
!=======================================================================
!
!     FOR TRACERS (=NOT VELOCITY) : DIRICHLET VALUES ARE NOT RESPECTED IF EXIT
!     THERE IS NO NEED TO TEST KENTU OR KADH FOR TRACERS
!
      IF(NPTFR3.GT.0.AND.NFRLIQ.GT.0.AND..NOT.VELOCITY) THEN
        DO IPTFR=1,NPTFR3
          IF(LIFBOL%I(IPTFR).EQ.KENT) THEN
!           EXITS ARE TREATED AS FREE BOUNDARIES
            IP=NBOR3%I(IPTFR)
            IF(FLUEXTPAR%R(IP).GT.0.D0) LIFBOL%I(IPTFR)=KSORT
          ELSEIF(LIFBOL%I(IPTFR).EQ.KSORT) THEN
            IP=NBOR3%I(IPTFR)
            IF(FLUEXTPAR%R(IP).LT.0.D0) THEN
              LIFBOL%I(IPTFR)=KENT
              FBORL%R(IPTFR)=FN%R(IP)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
!
!     A PRIORI CORRECTION OF FN FOR REAL ENTRANCES
!     I.E. LIFBOL STILL KENT DESPITE ABOVE CHANGE
!
      IF((SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI.OR.
     &    SCHCF.EQ.ADV_LPO.OR.SCHCF.EQ.ADV_LPO_TF.OR.
     &    SCHCF.EQ.ADV_NSC_TF).AND.YADIRFLU) THEN
!
        IF(NPTFR3.GT.0) THEN
!
          DO IP=1,NPTFR3
            IF(DIRFLU(NUMLIQ(IP)).EQ.2.AND.LIFBOL%I(IP).EQ.KENT) THEN
              I=NBOR3%I(IP)
              LIFBOL%I(IP)=KSORT
            ENDIF
          ENDDO
!
        ENDIF
!
      ENDIF
!
!     A PRIORI CORRECTION OF FN FOR REAL ENTRANCES
!     I.E. LIFBOL STILL KENT DESPITE ABOVE CHANGE
!
      IF(SCHCF.EQ.ADV_SUP.AND.YADIRFLU.AND.NPTFR3.GT.0) THEN
!
        DO IP=1,NPTFR3
          ILIQ = NUMLIQ(IP)
          IF(ILIQ.GT.0) THEN
            IF(DIRFLU(ILIQ).EQ.2.AND.LIFBOL%I(IP).EQ.KENT) THEN
              I=NBOR3%I(IP)
              LAMBDA=-FLUEXTPAR%R(I)*DT/
     &        (MAX(VOLUNPAR%R(I),1.D-10)-FLUEXTPAR%R(I)*DT)
              FN%R(I)=FN%R(I)+LAMBDA*(FBORL%R(IP)-FN%R(I))
!             CORRECTION OF FLUX
!             IN THE PROOF OF MASS-CONSERVATION, FLUEXT IS MULTIPLIED
!             BY FN INSTEAD OF FBOR, TO INTERPRET THE ADDED MASS AS
!             A FLUX THIS CORRECTION IS NECESSARY
!             HERE IT IS THE FN MODIFIED ABOVE
!             EVEN IF NOT CALFLU (CHEAPER)
              FLUXF=FLUXF+(FBORL%R(IP)-FN%R(I))*FLUEXT%R(I)*DT
              FLUXB(ILIQ)=FLUXB(ILIQ)
     &                   +(FBORL%R(IP)-FN%R(I))*FLUEXT%R(I)*DT
!             AVOIDS A DIRICHLET TREATMENT HEREAFTER AND BY DIFF3D -
!             WILL BE RESTORED AFTER DIFF3D
              LIFBOL%I(IP)=KSORT
            ENDIF
          ENDIF
        ENDDO
!
      ENDIF
!
!=======================================================================
!
!     PUTS DIRICHLET VALUES IN FN
!     MAY HAVE NO EFFECT IF TREATMENT OF FLUXES AT THE BOUNDARIES=2
!     BECAUSE LIFBOL CHANGED ABOVE
!
      IF(NPTFR3.GT.0) THEN
        DO IPTFR=1,NPTFR3
          IF(LIFBOL%I(IPTFR).EQ.KENT .OR.
     &       LIFBOL%I(IPTFR).EQ.KENTU.OR.
     &       LIFBOL%I(IPTFR).EQ.KADH) THEN
            FN%R(NBOR3%I(IPTFR)) = FBORL%R(IPTFR)
          ENDIF
        ENDDO
      ENDIF
!
!     HERE BOTTOM AND FREE SURFACE SHOULD BE TREATED AS WELL
!
!=======================================================================
!
!     3D ADVECTION (OTHER THAN SUPG)
!
!=======================================================================
!
!     WITH DISTRIBUTIVE SCHEMES, RIGHT-HAND SIDE MUST BE
!     IN INTEGRATED FORM
!
      IF(SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI.OR.SCHCF.EQ.ADV_LPO.OR.
     &   SCHCF.EQ.ADV_NSC_TF.OR.SCHCF.EQ.ADV_LPO_TF) THEN
!
!       S0F2 WILL BE MASS-MATRIX * S0F ASSEMBLED IN PARALLEL
!
        S0F2=>TRAV3%ADR(10)%P
        S0F2%TYPR=S0F%TYPR
!
        IF(S0F%TYPR.NE.'0') THEN
          IF(TRACERS) THEN
!           TO PREVENT FROM NEGATIVE CONCENTRATIONS
            CALL VECTOR(T3_01, '=', 'MASBAS          ',IELM3,1.D0,
     &           SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
            CALL OS ( 'X=YZ    ' , X=S0F2 ,Y=S0F , Z=T3_01)
          ELSE
            CALL VECTOR(S0F2,'=','MASVEC          ',IELM3,1.D0,
     &                  S0F,S0F,S0F,S0F,S0F,S0F,MESH3D,MSK,MASKEL)
          ENDIF
          IF(NCSIZE.GT.1) CALL PARCOM(S0F2,2,MESH3D)
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY CHARACTERISTICS
!
      IF(SCHCF.EQ.ADV_CAR) THEN
!
!       THIS IS NOW DONE IN CHARAC CALLED BY PRECON
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY MURD DISTRIBUTIVE SCHEME, OPTION N OR PSI
!
      ELSEIF((SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI)
     &       .AND.OPTADV.NE.4) THEN
!
        CALL MURD3D(FC,FC%R,FN%R,VOLU%R,VOLUN%R,T3_01%R,T3_01,
     &              MMURD,MMURD%D%R,MMURD%X%R,DIM1X,
     &              T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &              IKLE3%I,MESH2D,MESH3D,
     &              NELEM3,NELMAX,NPOIN3,DT,SCHCF,INFOR,
     &              CALFLU,FLUXB,FLUXF,FLUEXT%R,S0F2,NSCE,ISCE,KSCE,
     &              SOURCES,FSCE,RAIN,PLUIE,PARAPLUIE,TRAIN,NPOIN2,
     &              TRAV3%ADR(5)%P,TRAV3%ADR(6)%P,MASKPT%R,OPTBAN,
     &              FLODEL%R,FLOPAR%R,MESH3D%GLOSEG%I,
     &              MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN,IELM3,OPTSOU,
     &              NPTFR3,NBOR3%I,FLUEXTPAR%R,FBORL%R,ZN,
     &              TRAV3%ADR(7)%P,TRAV3%ADR(8)%P%R,TRAV3%ADR(9)%P%R,
!                             10: MAY BE S0F2
     &              TRAV3%ADR(11)%P%R,
     &              T2_01,BEDBOU,BEDFLU,OPTADV,NCO_DIST)
!
!       S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
!       IF DIFF3D IS CALLED AFTER
        S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY UPWIND EXPLICIT FINITE VOLUME SCHEME
!
      ELSEIF(SCHCF.EQ.ADV_LPO) THEN
!
        CALL MURD3D(FC,FC%R,FN%R,VOLU%R,VOLUN%R,T3_01%R,T3_01,
     &              MESH3D%M,MESH3D%M%D%R,MESH3D%M%X%R,DIM1X,
     &              T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &              IKLE3%I,MESH2D,MESH3D,
     &              NELEM3,NELMAX,NPOIN3,DT,SCHCF,INFOR,
     &              CALFLU,FLUXB,FLUXF,FLUEXT%R,S0F2,NSCE,ISCE,KSCE,
     &              SOURCES,FSCE,
     &              RAIN,PLUIE,PARAPLUIE,TRAIN,NPOIN2,
     &              TRAV3%ADR(5)%P,TRAV3%ADR(6)%P,MASKPT%R,OPTBAN,
     &              FLODEL%R,FLOPAR%R,MESH3D%GLOSEG%I,
     &              MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN,IELM3,OPTSOU,
     &              NPTFR3,NBOR3%I,FLUEXTPAR%R,FBORL%R,ZN,
     &              TRAV3%ADR(7)%P,TRAV3%ADR(8)%P%R,TRAV3%ADR(9)%P%R,
!                             10: MAY BE S0F2
     &              TRAV3%ADR(11)%P%R,
     &              T2_01,BEDBOU,BEDFLU,OPTADV,NCO_DIST)
!
!       S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
!       IF DIFF3D IS CALLED AFTER
        S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY LOCALLY IMPLICIT MURD SCHEMES, OPTION N OR PSI
!     note: they are not distinguished at the moment
!
      ELSEIF((SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI)
     &       .AND.OPTADV.EQ.4) THEN
!
        CALL MURD3D_LIPS(FC,FC%R,FN%R,VOLU%R,VOLUN%R,
     &                   MMURD%X%R,DIM1X,
     &                   T3_02%R,T3_03%R,T3_02,T3_03,T3_04,
     &                   IKLE3%I,MESH2D,MESH3D,NELEM3,NELMAX,
     &                   NPOIN3,DT,INFOR,
     &                   CALFLU,FLUXB,FLUXF,FLUEXT%R,S0F2,NSCE,ISCE,
     &                   KSCE,SOURCES,FSCE,RAIN,PLUIE%R,TRAIN,
     &                   NPOIN2,TRAV3%ADR(5)%P,TRAV3%ADR(6)%P,
     &                   MESH3D%GLOSEG%I,MESH3D%GLOSEG%DIM1,
     &                   MESH2D%NSEG,NPLAN,IELM3,OPTSOU,
     &                   NPTFR3,NBOR3%I,FLUEXTPAR%R,FBORL%R,ZN,
     &                   TRAV3%ADR(7)%P,TRAV3%ADR(8)%P,TRAV3%ADR(9)%P%R,
     &                   BEDBOU,BEDFLU,
     &                   NCO_DIST,NSP_DIST,SLVDIF,
     &                   MESH3D%ORISEG%I,MTRA1,
     &                   MESH3D%ELTSEG%I,TB2,TRAV3%ADR(13)%P,
     &                   TRAV3%ADR(14)%P,TRAV3%ADR(15)%P,
     &                   TRAV3%ADR(16)%P,TRAV3%ADR(17)%P%R,
     &                   TRAV3%ADR(18)%P%R)
!
!       S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
!       IF DIFF3D IS CALLED AFTER
        S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY NERD SCHEME WITH LEO POSTMA FLUXES
!
      ELSEIF(SCHCF.EQ.ADV_LPO_TF) THEN
!
        CALL MURD3D_POS(FC%R,FN%R,VOLU%R,VOLUN%R,VOLUN,
     &                  T3_01%R,T3_01,MESH3D%M%X%R,
     &                  T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &                  MESH2D,MESH3D,
     &                  NPOIN3,DT,SCHCF,INFOR,
     &                  CALFLU,FLUXB,FLUXF,FLUEXT%R,FLUEXTPAR%R,FBORL%R,
     &                  NPTFR3,NBOR3%I,S0F2,NSCE,SOURCES,FSCE,
     &                  RAIN,PARAPLUIE%R,TRAIN,NPOIN2,
     &                  FLOPAR%R,MESH3D%GLOSEG%I,
     &                  MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN,
     &                  TRAV3%ADR(6)%P,TRAV3%ADR(7)%P,
     &                  TRAV3%ADR(8)%P,
     &                  TRAV3%ADR(9)%P,OPTADV,IELM3,MAXADV,OPTSOU)
!
!       S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
!       IF DIFF3D IS CALLED AFTER
        S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
!     ADVECTION BY NERD SCHEME WITH N FLUXES
!
      ELSEIF(SCHCF.EQ.ADV_NSC_TF) THEN
!
        PARA=0
        IF(NCSIZE.GT.1) PARA=MESH3D%NSEG
        CALL MURD3D_POS(FC%R,FN%R,VOLU%R,VOLUN%R,VOLUN,
     &                  T3_01%R,T3_01,MESH3D%M%X%R,
     &                  T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &                  MESH2D,MESH3D,
     &                  NPOIN3,DT,SCHCF,INFOR,
     &                  CALFLU,FLUXB,FLUXF,FLUEXT%R,FLUEXTPAR%R,FBORL%R,
     &                  NPTFR3,NBOR3%I,S0F2,NSCE,SOURCES,FSCE,
     &                  RAIN,PARAPLUIE%R,TRAIN,NPOIN2,
     &                  MURD_TF%X%R(1+PARA:MESH3D%NSEG+PARA),
     &                  MESH3D%GLOSEG%I,
     &                  MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN,
     &                  TRAV3%ADR(6)%P,TRAV3%ADR(7)%P,
     &                  TRAV3%ADR(8)%P,
     &                  TRAV3%ADR(9)%P,OPTADV,IELM3,MAXADV,OPTSOU)
!
!       S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
!       IF DIFF3D IS CALLED AFTER
        S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
!     OTHER CASES (SUPG OR NO ADVECTION)
!
      ELSE
!
        CALL OS ( 'X=Y     ' , X=FC , Y=FN )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     IMPLICITT VERTICAL DIFFUSION SCHEME FOR SEDIMENTS ADDED HERE
!
      IF(YAWCHU.AND.SETDEP.EQ.1) THEN
!
        CALL SET_DIF(FC%R,VOLU2D,MESH3D%Z%R,NPOIN2,NPOIN3,DT,FLUXF,
     &           NPLAN,WCHU,FLUDPT,FLUDP,FLUER,IPBOT,VISCF%ADR(3)%P)
!
!       VERTICAL DIFFUSION HAS ALREADY BEEN TREATED THEN IT IS SET
!       TO 0 (VIRTUALLY, WITH COMPONENT TYPR, SEE HOW MT02PP IS WRITTEN).
!       IDEM WITH SETTLING VELOCITY
!
        VISCF%ADR(3)%P%TYPR='0'
        YAWCHU=.FALSE.
!
      ELSEIF(YAWCHU.AND.S3D_SETDEP.EQ.1) THEN

        CALL SET_DIF(FC%R,VOLU2D,MESH3D%Z%R,NPOIN2,NPOIN3,DT,FLUXF,
     &              NPLAN,S3D_WCHU,S3D_FLUDPT,S3D_FLUDP,S3D_FLUER,IPBOT,
     &               VISCF%ADR(3)%P)
!
!       VERTICAL DIFFUSION HAS ALREADY BEEN TREATED THEN IT IS SET
!       TO 0 (VIRTUALLY, WITH COMPONENT TYPR, SEE HOW MT02PP IS WRITTEN).
!       IDEM WITH SETTLING VELOCITY
!
        VISCF%ADR(3)%P%TYPR='0'
        YAWCHU=.FALSE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     RE-ENFORCES DIRICHLET POINTS (MAY CAUSE MASS ERRORS)
!     IN FACT NOT DONE IF LIFBOL HAS BEEN CHANGED ABOVE INTO KSORT
!     HENCE NO EFFECT WHEN YADIRFLU=.TRUE.
!
      IF(NPTFR3.GT.0) THEN
        DO IP=1,NPTFR3
          IF(LIFBOL%I(IP).EQ.KENT .OR.
     &       LIFBOL%I(IP).EQ.KENTU.OR.
     &       LIFBOL%I(IP).EQ.KADH) THEN
            I=NBOR3%I(IP)
            FC%R(I) = FBORL%R(IP)
          ENDIF
        ENDDO
      ENDIF
!
!     BOTTOM AND FREE SURFACE
!
      K = NPOIN3 - NPOIN2
      DO IP = 1,NPOIN2
        IF(LIFBOF%I(IP).EQ.KENT.OR.LIFBOF%I(IP).EQ.KADH) THEN
          FC%R(IP)   = FBORF%R(IP)
        ENDIF
        IF(LIFBOS%I(IP).EQ.KENT.OR.LIFBOS%I(IP).EQ.KADH) THEN
          FC%R(IP+K) = FBORS%R(IP)
        ENDIF
      ENDDO
!
!=======================================================================
!
!  SUPG ADVECTION AND/OR DIFFUSION
! (IN THIS CASE IT IS NECESSARY TO SOLVE A LINEAR SYSTEM)
!
!=======================================================================
!
      IF(SCHCF.EQ.ADV_SUP.OR.SCHDF.NE.0) THEN
!
        IF(SCHCF.EQ.ADV_SUP) THEN
!         IT SEEMS THAT WITH SUBITERATIONS ONLY TETASUPG=1-TETAH WORKS
!         FOR MASS-CONSERVATION. SEE ALSO DIFF3D WITH ANOTHER COMMENT
          TETASUPG=0.55D0
        ELSE
          TETASUPG=1.D0
        ENDIF
!
        IF(SCHCF.EQ.ADV_SUP.AND..NOT.VELOCITY) THEN
          CALL OS('X=CY    ',X=VOLUT,Y=VOLUN    ,C=     TETASUPG)
          CALL OS('X=X+CY  ',X=VOLUT,Y=VOLU     ,C=1.D0-TETASUPG)
          CALL OS('X=CY    ',X=ZT   ,Y=ZPROP    ,C=     TETASUPG)
          CALL OS('X=X+CY  ',X=ZT   ,Y=MESH3D%Z ,C=1.D0-TETASUPG)
!         ZT IS TEMPORARILY PUT IN MESH3D%Z
          SAVEZ=>MESH3D%Z%R
          MESH3D%Z%R=>ZT%R
          VOLUME=>VOLUT
        ELSE
          VOLUME=>VOLU
        ENDIF
!
        IF(SCHCF.EQ.ADV_CAR.OR.SCHCF.EQ.ADV_SUP) THEN
!         SOURCES HAVE TO BE TREATED
          YASCE=.TRUE.
          YARAIN=RAIN
        ELSE
!         SOURCES HAVE ALREADY BEEN TREATED BY DISTRIBUTIVE SCHEMES
          YASCE=.FALSE.
!         RAIN HAS ALREADY BEEN TREATED BY DISTRIBUTIVE SCHEMES
          YARAIN=.FALSE.
        ENDIF
!
        CALL DIFF3D(FD,FC,FN,VISCF,SIGMAF,
     &              S0F,YAS0F,S1F,YAS1F,
     &              FBORL,FBORF,FBORS,AFBORL,AFBORF,AFBORS,
     &              BFBORL,BFBORF,BFBORS,LIFBOF,LIFBOL,LIFBOS,
     &              FMIN,CLIMIN,FMAX,CLIMAX,
     &              SCHCF,SCHDF,SLVDIF,TRBAF,INFOR,NEWDIF,
     &              DT,T2_01,T2_03,T3_01,T3_02,T3_03,T3_04,
     &              NPOIN2,NPOIN3,INCHYD,SEM3D,YASEM3D,IT1,
     &              NPTFR3,NBOR3,MASKPT,TRAV3,MESH2D,
     &              MESH3D,MTRA1,MTRA2,IELM3,MSUPG,IELM2H,IELM2V,
     &              MDIFF,MASKBR,SVIDE,MSK,MASKEL,
     &              NPLAN,OPTBAN,TETADI,YAWCHU,WCHU,S3D_WCHU,AGGLOD,
     &              VOLUME,YASCE,NSCE,FSCE,SOURCES,TETASUPG,
     &              YARAIN,PLUIE%R,TRAIN,SIGMAG,IPBOT,
     &              S3D_SETDEP,OPTSOU,SETDEP)
!
        IF(SCHCF.EQ.ADV_SUP.AND..NOT.VELOCITY) THEN
!         MESH3D%Z RESTORED
          MESH3D%Z%R=>SAVEZ
        ENDIF
!
      ELSE
        CALL OS ( 'X=Y     ', X=FD, Y=FC )
      ENDIF
!
!-----------------------------------------------------------------------
!
!     ADVECTIVE FLUXES AND SOURCES
!
      IF(CALFLU) THEN
!
        IF(SCHCF.EQ.ADV_CAR) THEN
          DO IP = 1,NPOIN3
            FLUXF = FLUXF + FN%R(IP)*FLUEXT%R(IP)*DT
          ENDDO
        ELSEIF(SCHCF.EQ.ADV_SUP) THEN
          DO IP = 1,NPOIN3
            FLUXF = FLUXF + FLUEXT%R(IP)*DT*
     &                      (TETASUPG*FD%R(IP)+(1.D0-TETASUPG)*FN%R(IP))
          ENDDO
        ENDIF
!
        IF(NPTFR3.GT.0) THEN
          IF(SCHCF.EQ.ADV_CAR) THEN
            DO IP = 1,NPTFR3
              ILIQ=NUMLIQ(IP)
              IF(ILIQ.GT.0) THEN
                FLUXB(ILIQ) = FLUXB(ILIQ)
     &                      + FN%R(NBOR3%I(IP))*FLUEXT%R(NBOR3%I(IP))*DT
              ENDIF
            ENDDO
          ELSEIF(SCHCF.EQ.ADV_SUP) THEN
            DO IP = 1,NPTFR3
              ILIQ=NUMLIQ(IP)
              IF(ILIQ.GT.0) THEN
                FLUXB(ILIQ) = FLUXB(ILIQ)
     &                      + FLUEXT%R(NBOR3%I(IP))*DT*
     &                       (        TETASUPG*FD%R(NBOR3%I(IP))
     &                        +(1.D0-TETASUPG)*FN%R(NBOR3%I(IP)))
              ENDIF
            ENDDO
          ENDIF
        ENDIF
!
!
!       CHARACTERISTICS OR SUPG : FLUX DUE TO SOURCES
!       (FOR DISTRIBUTIVE SCHEMES IT IS DONE LOCALLY)
!
        IF(NSCE.GT.0.AND.(SCHCF.EQ.ADV_CAR.OR.SCHCF.EQ.ADV_SUP)) THEN
          IF(OPTSOU.EQ.1) THEN
!           SOURCE NOT CONSIDERED AS A DIRAC
            DO IS=1,NSCE
              IIS=IS
!             HERE IN PARALLEL SOURCES WITHOUT PARCOM
!             ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
              IF(NCSIZE.GT.1) IIS=IIS+NSCE
              DO IP=1,NPOIN3
!                              WITH PARCOM
                IF(SOURCES%ADR(IS)%P%R(IP).GT.0.D0) THEN
                  FLUXF=FLUXF-FSCE(IS)*SOURCES%ADR(IIS)%P%R(IP)*DT
!                                                  WITHOUT PARCOM
                  FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)
     &                            -FSCE(IS)*SOURCES%ADR(IIS)%P%R(IP)*DT
!                                                  WITHOUT PARCOM
                ELSE
!                             FN FOR CHARACTERISTICS ?
                  FLUXF=FLUXF-FD%R(IP)*SOURCES%ADR(IIS)%P%R(IP)*DT
!                                                  WITHOUT PARCOM
                  FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)
     &                            -FD%R(IP)*SOURCES%ADR(IIS)%P%R(IP)*DT
!                                                  WITHOUT PARCOM
                ENDIF
              ENDDO
            ENDDO
          ELSEIF(OPTSOU.EQ.2) THEN
!           SOURCE CONSIDERED AS A DIRAC
            IIS = 1
!           HERE IN PARALLEL SOURCES WITHOUT PARCOM
!           ARE STORED AT ADRESSES 2 (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=2
            DO IS=1,NSCE
              IF(ISCE(IS).GT.0) THEN
                IP=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
!                              WITH PARCOM
                IF(SOURCES%ADR(1)%P%R(IP).GT.0.D0) THEN
                  FLUXF=FLUXF-FSCE(IS)*SOURCES%ADR(IIS)%P%R(IP)*DT
!                                                  WITHOUT PARCOM
                  FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)
     &                            -FSCE(IS)*SOURCES%ADR(IIS)%P%R(IP)*DT
!                                                  WITHOUT PARCOM
                ELSE
!                             FN FOR CHARACTERISTICS ?
                  FLUXF=FLUXF-FD%R(IP)*SOURCES%ADR(IIS)%P%R(IP)*DT
!                                                  WITHOUT PARCOM
                  FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)
     &                            -FD%R(IP)*SOURCES%ADR(IIS)%P%R(IP)*DT
!                                                  WITHOUT PARCOM
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
!
!       CHARACTERISTICS OR SUPG : BED FLUXES
!       (FOR DISTRIBUTIVE SCHEMES IT IS DONE IN MURD3D)
!
        IF(BEDBOU.AND.(SCHCF.EQ.ADV_CAR.OR.SCHCF.EQ.ADV_SUP)) THEN
          DO IP=1,NPOIN2
            IF(BEDFLU%R(IP).LE.0.D0) THEN
!                         FN FOR CHARACTERISTICS ?
              FLUXF=FLUXF-FD%R(IP)*BEDFLU%R(IP)*DT
              FLUXB(MAXFRO+NSCE+1)=FLUXB(MAXFRO+NSCE+1)
     &                            -FD%R(IP)*BEDFLU%R(IP)*DT
            ENDIF
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     A POSTERIORI CORRECTION OF SUPG RESULTS
!
      IF(SCHCF.EQ.ADV_SUP.AND.YADIRFLU) THEN
!
!       CORRECTED VALUE AND CORRESPONDING FLUX CORRECTION
!
        IF(NPTFR3.GT.0) THEN
          DO IP=1,NPTFR3
            ILIQ=NUMLIQ(IP)
            IF(ILIQ.GT.0) THEN
              IF(DIRFLU(ILIQ).EQ.2) THEN
                IF(LIFBOL%I(IP+NPTFR3).EQ.KENT .OR.
     &             LIFBOL%I(IP+NPTFR3).EQ.KENTU.OR.
     &             LIFBOL%I(IP+NPTFR3).EQ.KADH     ) THEN
!                 ONLY ENTRANCES
                  I=NBOR3%I(IP)
                  IF(FLUEXTPAR%R(I).LT.0.D0) THEN
                    STOFD=FD%R(I)
                    LAMBDA=-FLUEXTPAR%R(I)*TETASUPG*DT/
     &                   MAX(VOLUPAR%R(I),1.D-10)
                    FD%R(I)=STOFD+LAMBDA*(FN%R(I)-STOFD)
!                   CORRECTION OF FLUX
!                   A POSTERIORI ADDED MASS DUE TO CORRECTION
                    FLUXF=FLUXF-VOLU%R(I)*(FD%R(I)-STOFD)
                    FLUXB(ILIQ)=FLUXB(ILIQ)-VOLU%R(I)*(FD%R(I)-STOFD)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(CALFLU) THEN
!       NOW RETURNS TO REAL FLUXES, NOT FLUXES*DT
        FLUXF = FLUXF / DT
!       PARALLEL MODE
        IF(NCSIZE.GT.1) FLUXF = P_SUM(FLUXF)
        DO I=1,MAXFRO+NSCE+1
          FLUXB(I) = FLUXB(I) / DT
          IF(NCSIZE.GT.1) FLUXB(I) = P_SUM(FLUXB(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     RESTORES ORIGINAL LIFBOL FROM SECOND DIMENSION
!
      DO IP=1,NPTFR3
        LIFBOL%I(IP)=LIFBOL%I(IP+NPTFR3)
      ENDDO
!
!     S0F%TYPR RESTORED (S0F MAY BE USED ELSEWHERE, E.G. IN A FURTHER
!                        SUB ITERATION)
!
      S0F%TYPR=S0FTYPR
!
!     RESTORING NUZ FOR FURTHER USES
!
      VISCF%ADR(3)%P%TYPR=NUZTYPR
!
!-----------------------------------------------------------------------
!
      RETURN
      END

