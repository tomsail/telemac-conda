!                   *********************************
                    SUBROUTINE SUSPENSION_COMPUTATION
!                   *********************************
!
     &(SLVTRA, HN,HN_TEL,UCONV, VCONV, MU,TOB,FDM, FD90, KSP,KSR,KS,
     & ELAY, AVA, AFBOR, BFBOR, LIMDIF, CLT, MASKEL, MASKTR,
     & MASKPT, IFAMAS, NPOIN, IELM, NPTFR, ITRA, LT, NIT, RESOL,
     & OPTBAN, KENT,KDDL,KDIR,KSORT,KLOG,KNEU,
     & OPTADV, OPDTRA, DEBUG,CSF_SABLE,
     & TETA_SUSP, DT, MASED0, ZERO, XWC, KARMAN, XMVE, XMVS, VCE,GRAV,
     & HMIN, VITCD, PARTHENIADES, ENTETS,
     & BILMA,MSK,CHARR,IMP_INFLOW_C,MESH,ZF,CS,
     & CST,CTILD,CBOR,DISP,IT1,IT2,IT3,IT4,TB,T1,T2,T3,
     & T4, T8, T9, T10, T11, T12, T14, TE1, TE2, TE3, S,
     & AM1_S, AM2_S, MBOR,MASTEN, MASTOU, MASINI, AC,
     & ZFCL_S, FLUDPT, FLUDP, FLUER, HPROP, DISP_C, CSTAEQ, CSRATIO,
     & MASFIN, MASDEPT, MASDEP, MASSOU,QS_C,ICQ, ZREF,
     & CORR_CONV,U2D,V2D,SEDCO,DIFT,DM1,ZCONV,UCONV_TEL,VCONV_TEL,
     & SOLSYS,FLBOR_TEL,FLBOR_SIS,FLBORTRA,CODE,
     & VOLU2D,V2DPAR,UNSV2D,NUMLIQ,NFRLIQ,LICBOR,MIXTE,AVAIL,NSICLA,
     & ES,ES_SABLE,ES_VASE,NOMBLAY,CONC,TOCE_VASE,TOCE_SABLE,
     & FLUER_VASE,TOCE_MIXTE,MS_SABLE,MS_VASE,DIRFLU,QSCLXS,QSCLYS,
     & MAXADV)
!
!***********************************************************************
! SISYPHE   V7P1
!***********************************************************************
!
!brief    MAIN SUBROUTINE FOR THE COMPUTATION OF THE
!+                CONCENTRATION AND THE ELEVATION SOLVING THE EQUATION :
!code
!+      D(ZF)
!+      ----  + DIV(QS) + (E-D)ZA = 0
!+      DT
!
!note     IF COUPLING, DIV(QS) ALREADY COMPUTED
!+         ELSE,        DIV(QS) = 0
!
!history  F. HUVELIN
!+        22/12/2004
!+
!+
!
!history  JMH:
!+        10/11/2010
!+
!+   ENTET CHANGED INTO ENTETS IN THE CALL TO CVDFTR
!
!history
!+        05/05/2008
!+
!+   ADAPTED FOR FINITE VOLUME ADVECTION
!
!history
!+        09/05/2008
!+
!+   FLUDP REMOVED FROM SUSPENSION_FLUX, SUSPENSION_NERBED DELETED
!
!history
!+        28/05/2008
!+
!+   NEW SUSPENSION_BILAN WITH FLUXES THROUGH BOUNDARIES
!
!history
!+        09/06/2008
!+
!+   NEW SUSPENSION_BILAN WITH FLBORTRA GIVEN BY CVDFTR
!
!history
!+        12/06/2008
!+
!+   SECTIONS "TREATING SMALL DEPTHS" AND
!
!history
!+        25/06/2008
!+
!+   CALLS DIFFIN (USED TO BE IN SUSPENSION_MAIN)
!
!history
!+        31/07/2008
!+
!+   CALLS SUSPENSION_FLUX SPLIT IN 2 : DEPOSITION + EROSION
!
!history
!+        16/09/2009
!+
!+   AVAIL(NPOIN,10,NSICLA)
!
!history
!+        05/04/2010
!+        V6P0
!+   CSTAEQ TAKES INTO ACCOUNT THE % OF LAYER QQ OR THE SELECTED FORMULATION
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
!history  J.-M. HERVOUET (LNHE)
!+        19/04/2011
!+        V6P1
!+   Adaptation to the new call of Sisyphe in Telemac-2D, and various
!+   modifications for mass-conservation.
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables
!
!history  MAK (HRW)
!+        31/05/2012
!+        V6P2
!+  Include CSRATIO
!
!history  PAT (P. TASSI) (EDF & LNHE)
!+        18/06/2012
!+        V6P2
!+   updated version with HRW's development Soulsby-van Rijn's concentration
!
!history  C. VILLARET (EDF & LNHE)
!+        18/06/2012
!+        V6P2
!+   modification call to suspension_erosion_coh (simplified)
!+   arguments for M_VASE in double precision
!+   modification for limitation of FLUER with rigid lid
!+   calling to suspension_bilan_coh
!
!history  J.-M. HERVOUET (LNHE)
!+        15/04/2013
!+        V6P3
!+   YAFLULIM was not initialised in one case.
!
!history  J.-M. HERVOUET (LNHE)
!+        18/11/2013
!+        V6P3
!+   Pointers GLOSEG1 and GLOSEG2 added to avoid an unwanted copy of
!+   arrays in call to suspension_conv (after message by Intel compiler)
!
!history  J.-M. HERVOUET (LNHE)
!+        28/04/2014
!+        V7P0
!+   Call to diffin modified.
!+   OPTSUP replaced by OPTADV in the call to cvdftr.
!+   (see keyword SCHME OPTION FOR ADVECTION)
!
!history  J.-M. HERVOUET (LNHE)
!+        28/05/2015
!+        V7P1
!+   Call to cvdftr modified, 3 extra arguments
!+   (for implicit distributive schemes)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AFBOR          |-->| BOUNDARY CONDITION ON F: NU*DF/DN=AFBOR*F+BFBOR
!| AM1_S          |<->| MATRIX OBJECT
!| AM2_S          |<->| MATRIX OBJECT
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| BFBOR          |-->| BOUNDARY CONDITION ON F: NU*DF/DN=AFBOR*F+BFBOR
!| BILMA          |-->| MASS BALANCE
!| CBOR           |<->| IMPOSED SUSPENDED SAND CONCENTRATION AT THE BOUNDARY
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| CHARR          |-->| LOGICAL, BEDLOAD OR NOT
!| CLT            |<->| BOUNDARY CONDITIONS FOR TRACER (MODIFIED LITBOR)
!| CODE           |-->| HYDRODYNAMIC CODE IN CASE OF COUPLING
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| CORR_CONV      |-->| CORRECTION ON CONVECTION VELOCITY
!| CS             |<->| CONCENTRATION AT TIME N
!| CSF_SABLE      |-->| VOLUME CONCENTRATION OF THE SAND BED
!| CSRATIO        |<->| EQUILIBRIUM CONCENTRATION FOR SOULSBY-VAN RIJN EQ.
!| CST            |<->| CONCENTRATION AT TIME T(N+1)
!| CSTAEQ         |<->| EQUILIBRIUM CONCENTRATION
!| CTILD          |<->| CONCENTRATION AFTER ADVECTION
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| DIFT           |-->| DIFFUSION OF SUSPENDED SEDIMENT CONCENTRATION
!| DISP           |-->| VISCOSITY COEFFICIENTS ALONG X,Y AND Z .
!|                |   | IF P0 : PER ELEMENT
!|                |   | IF P1 : PERR POINT
!| DISP_S         |<->| WORK ARRAY FOR SAVING DISPC
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV)
!| DTS            |-->| TIME STEP FOR SUSPENSION
!| ELAY           |<->| THICKNESS OF EACH LAYER
!| ENTET          |<->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION
!| ENTETS         |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION FOR SUSPENSION
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| FLBORTRA       |<->| FLUXES AT BOUNDARIES TRACER
!| FLBOR_SIS      |<->| FLUXES AT BOUNDARIES SISYPHE
!| FLBOR_TEL      |-->| FLUXES AT BOUNDARIES TELEMAC
!| FLUDP          |<->| DEPOSITION FLUX
!| FLUDPT         |<->| DEPOSITION FLUX (IMPLICIT)
!| FLUER          |<->| EROSION FLUX
!| FLUER_VASE     |<->| FOR MIXED SEDIMENTS
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| HN_TEL         |-->| WATER DEPTH AS SENT BY TELEMAC OR CALLING CODE
!| HPROP          |<->| PROPAGATION DEPTH (DONE IN CVDFTR)
!| ICQ            |-->| REFERENCE CONCENTRATION FORMULA
!| IELM           |-->| TYPE OF ELEMENT
!| IFAMAS         |-->| A MODIFIED IFABOR WHEN ELEMENTS ARE MASKED
!| IMP_INFLOW_C   |-->| IMPOSED CONCENTRATION IN INFLOW
!| IT1            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| IT2            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| IT3            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| IT4            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KS             |-->| TOTAL BED ROUGHNESS
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| KSP            |-->| SKIN BED ROUGHNESS
!| KSR            |-->| RIPPLE BED ROUGHNESS
!| KX             |<->| COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!| KY             |<->| COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!| KZ             |<->| COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!| LICBOR         |-->| BOUNDARY CONDITIONS FOR SEDIMENT
!| LIMDIF         |<->| BOUNDARY CONDITIONS FOR DIFFUSION
!| LT             |-->| ITERATION
!| MASDEP         |<--| TOTAL DEPOSITED MASS
!| MASDEPT        |<--| DEPOSITED MASS DURING THE TIME STEP
!| MASED0         |<->| SUSPENDED MASS BALANCE
!| MASFIN         |<--| MASS AT THE END
!| MASINI         |<->| INITIAL MASS
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MASKPT         |-->| MASKING PER POINT
!| MASKTR         |<->| MASKING FOR TRACERS, PER POINT
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!|                |   | SEE DIFSOU
!| MASTCP         |<--| ??? NE SERT A RIEN, A SUPPRIMER
!| MASTEN         |<->| MASS ENTERED THROUGH LIQUID BOUNDARY
!| MASTOU         |<->| MASS CREATED BY SOURCE TERM
!| MAXADV         |<->| MAXIMUM NUMBER OF ITERATIONS OF ADVECTION SCHEMES
!| MBOR           |<->| MATRIX OBJECT
!| MESH           |<->| MESH STRUCTURE
!| MIXTE          |-->| MIXTURE OF COHESIVE AND NON COHESIVE SEDIMENT
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| MU             |-->| CORRECTION FACTOR FOR BED ROUGHNESS
!| NOMBLAY        |-->| NUMBER OF LAYERS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NIT            |-->| TOTAL NUMBER OF ITERATIONS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| OPDTRA         |-->| OPTION FOR THE DIFFUSION OF TRACERS
!| OPTBAN         |-->| OPTION FOR THE TREATMENT OF TIDAL FLATS
!| OPTDIF         |-->| OPTION FOR THE DISPERSION
!| OPTADV         |-->| SCHEME OPTION FOR ADVECTION
!| PARTHENIADES   |-->| CONSTANT OF THE KRONE AND PARTHENIADES EROSION LAW (M/S)
!| PASS           |<->| IN FACT PASS_SUSP IN SISYPHE.F, ARRIVES AS .TRUE.
!|                |   | AT FIRST CALL AND IS CHANGED INTO .FALSE. BELOW
!| QSCLXS         |<->| TRANSPORT RATE FOR EACH CLASS X-DIRECTION
!| QSCLYS         |<->| TRANSPORT RATE FOR EACH CLASS Y-DIRECTION
!| QSCL_S         |<->| SUSPENDED LOAD TRANSPORT RATE
!| QSXS           |<->| SOLID DISCHARGE X (SUSPENSION)
!| QSYS           |<->| SOLID DISCHARGE Y (SUSPENSION)
!| QS_C           |-->| BEDLOAD TRANSPORT RATE
!| QS_S           |<->| SUSPENDED TRANSPORT RATE
!| RESOL          |-->| CHOICE OF ADVECTION SCHEME
!| S              |<->| VOID STRUCTURE
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| SLVTRA         |<->| SLVCFG STRUCTURE
!| SOLSYS         |-->| SLVCFG STRUCTURE
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T10            |<->| WORK BIEF_OBJ STRUCTURE
!| T11            |<->| WORK BIEF_OBJ STRUCTURE
!| T12            |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| T9             |<->| WORK BIEF_OBJ STRUCTURE
!| TASS           |-->| CONSOLIDATION TAKEN INTO ACCOUNT
!| TB             |-->| BLOCK OF WORKING ARRAYS
!| TE1            |<->| WORKING ARRAY FOR ELEMENTS
!| TE2            |<->| WORKING ARRAY FOR ELEMENTS
!| TE3            |<->| WORKING ARRAY FOR ELEMENTS
!| TETA_SUSP      |<->| IMPLICITATION FACTOR FOR THE DEPOSITION FLUX AND DIFFUSION
!| TOB            |-->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOCE_SABLE     |<->| CRITICAL BED SHEAR STRESS OF SAND
!| TOCE_MIXTE     |<->| CRITICAL BED SHEAR STRESS OF THE MIXED SEDUIMENT PER LAYER
!| TOCE_VASE      |<->| CRITICAL EROSION SHEAR STRESS OF THE MUD PER LAYER (N/M2)
!| U2D            |-->| MEAN FLOW VELOCITY X-DIRECTION
!| UCONV          |<->| X-COMPONENT ADVECTION FIELD (SISYPHE)
!| UCONV_TEL      |-->| X-COMPONENT ADVECTION FIELD (TELEMAC)
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| V2D            |-->| MEAN FLOW VELOCITY Y-DIRECTION
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| VCE            |-->| FLOW VISCOSITY
!| VCONV          |<->| Y-COMPONENT ADVECTION FIELD (SISYPHE)
!| VCONV_TEL      |-->| Y-COMPONENT ADVECTION FIELD (TELEMAC)
!| VISC_TEL       |-->| VELOCITY DIFFUSIVITY (TELEMAC)
!| VITCD          |-->| CRITICAL SHEAR VELOCITY FOR MUD DEPOSITION
!| VOLU2D         |-->| INTEGRAL OF BASES
!| W1             |<->| WORKING ARRAY
!| XKX            |-->| COEFFICIENT USED FOR COMPUTING THE DISPERSION
!|                |   | DEPENDS OF OPTIONS
!| XKY            |-->| COEFFICIENT USED FOR COMPUTING THE DISPERSION
!|                |   | DEPENDS OF OPTIONS
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!| XWC            |-->| SETTLING VELOCITIES
!| ZCONV          |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV)
!| ZERO           |-->| ZERO
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZFCL_S         |<->| BED EVOLUTION PER CLASS, DUE TO SUSPENDED SEDIMENT
!| ZF_S           |<->| ACCUMULATED BED EVOLUTION DUE TO SUSPENDED SEDIMENT
!| ZREF           |<->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_SUSPENSION_COMPUTATION => SUSPENSION_COMPUTATION
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : FLULIM,TB2,NCO_DIST,NSP_DIST
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (SLVCFG),    INTENT(INOUT) :: SLVTRA
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZF,VOLU2D,V2DPAR,UNSV2D
      TYPE (BIEF_OBJ),  INTENT(IN), TARGET    :: HN,HN_TEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: UCONV,VCONV
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MU,KSP,KSR,KS
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TOB,LICBOR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ELAY
      TYPE (BIEF_OBJ),  INTENT(IN)    :: AFBOR,BFBOR
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL,MASKPT,IFAMAS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MASKTR,LIMDIF,CLT
      INTEGER,          INTENT(IN)    :: NPOIN,IELM,NPTFR,ITRA,LT
      INTEGER,          INTENT(IN)    :: NIT,RESOL,OPTBAN,KENT,KDDL
      INTEGER,          INTENT(IN)    :: KDIR,OPTADV,OPDTRA,SOLSYS
      INTEGER,          INTENT(IN)    :: KSORT,KLOG,KNEU
      INTEGER,          INTENT(IN)    :: NFRLIQ,NSICLA,NOMBLAY
      INTEGER,          INTENT(IN)    :: DEBUG,DIRFLU,MAXADV
      INTEGER,          INTENT(IN)    :: NUMLIQ(*)
      DOUBLE PRECISION, INTENT(IN)    :: TETA_SUSP, DT, MASED0
      DOUBLE PRECISION, INTENT(IN)    :: XWC,FDM,FD90
      DOUBLE PRECISION, INTENT(IN)    :: CSF_SABLE,AVA(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, XMVE, XMVS,VCE, GRAV
      DOUBLE PRECISION, INTENT(IN)    :: VITCD,PARTHENIADES,HMIN
      LOGICAL,          INTENT(IN)    :: ENTETS,BILMA,MSK,SEDCO
      LOGICAL,          INTENT(IN)    :: CHARR, IMP_INFLOW_C,CORR_CONV
      LOGICAL,          INTENT(IN)    :: DIFT,MIXTE
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: CS,CST,CTILD,CBOR,FLBOR_SIS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DISP,IT1,IT2,IT3,IT4,TB
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T2, T3, T4, T8
      TYPE (BIEF_OBJ),  INTENT(INOUT), TARGET :: T1
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T9, T10, T11, T12, T14, TE1
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: TE2, TE3, S, AM1_S, AM2_S
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MBOR,ZREF
      DOUBLE PRECISION, INTENT(INOUT) :: MASTEN, MASTOU, MASINI, AC
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZFCL_S
      TYPE (BIEF_OBJ),  INTENT(IN)    :: UCONV_TEL,VCONV_TEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUDPT,FLUDP,FLUER,FLBORTRA
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: HPROP, DISP_C, CSTAEQ,CSRATIO
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER_VASE,TOCE_MIXTE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: QSCLXS,QSCLYS
      DOUBLE PRECISION, INTENT(INOUT) :: MS_SABLE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MS_VASE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_SABLE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_VASE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASFIN,MASDEPT,MASDEP
      DOUBLE PRECISION, INTENT(IN)    :: ZERO
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY),TOCE_SABLE
      DOUBLE PRECISION, INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: TOCE_VASE(NOMBLAY)
      TYPE (BIEF_OBJ),  INTENT(IN)    :: QS_C,U2D,V2D,DM1,ZCONV
      TYPE (BIEF_OBJ),  INTENT(IN)    :: FLBOR_TEL
      INTEGER,          INTENT(IN)    :: ICQ
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I,K,SOLSYS_SIS,OPTVF,BID(1),RESOL_MOD,IELMT
      DOUBLE PRECISION :: TETAH,AGGLOT
      LOGICAL          :: YASMI2,YAFLULIM
      TYPE (BIEF_OBJ),  POINTER :: HOLD
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVE_UCONV,SAVE_VCONV
      DOUBLE PRECISION :: MSTOT
      DOUBLE PRECISION :: CONC_SABLE(NPOIN, NOMBLAY)
      INTEGER          :: J
!
      INTEGER, POINTER, DIMENSION(:) :: GLOSEG1,GLOSEG2
!
!-----------------------------------------------------------------------
!
!     IN CHARAC IELMT IS INTENT(INOUT)
      IELMT=IELM
!
!     UCONV POINTER SAVED BEFORE PLAYING WITH IT
!
      SAVE_UCONV=>UCONV%R
      SAVE_VCONV=>VCONV%R
      GLOSEG1=>MESH%GLOSEG%I(1:MESH%GLOSEG%DIM1)
      GLOSEG2=>MESH%GLOSEG%I(MESH%GLOSEG%DIM1+1:2*MESH%GLOSEG%DIM1)
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     TAKES DETAILS OF THE CONTINUITY EQUATION INTO ACCOUNT
!     IN TELEMAC-2D OR 3D, WITH SOLSYS=2, DM1 AND ZCONV ARE USED.
!
      IF(CODE(1:9).EQ.'TELEMAC2D') THEN
        SOLSYS_SIS=SOLSYS
      ELSEIF(LT.GT.1.AND.CODE(1:9).EQ.'TELEMAC3D') THEN
!       CALL TO SISYPHE TO BE MOVED IN THE TELEMAC-3D TIME LOOP
        SOLSYS_SIS=SOLSYS
      ELSE
        SOLSYS_SIS=1
      ENDIF
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 1.  COMPUTES THE REFERENCE ELEVATION  -->  ZREF
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     THREE OPTIONS : ICQ=1: FREDSOE REFERENCE CONC. ZREF = 2.D50
!                     ICQ=2: BIJKER METHOD ZREF = MAX(KSP,KS)
!                     ICQ=3: VAN RIJN ZREF= 0.5 KS
!
      IF(ICQ.EQ.1) THEN
        CALL OS('X=Y     ', X=ZREF, Y=KSP)
      ELSEIF(ICQ.EQ.2) THEN
        CALL OS('X=Y     ', X=ZREF, Y=KSR)
      ELSEIF(ICQ.EQ.3) THEN
        CALL OS('X=CY    ', X=ZREF, Y=KS,C=0.5D0)
      ELSEIF(ICQ.EQ.4) THEN
        CALL OS('X=CY    ', X=ZREF, Y=KS,C=0.5D0)
      ELSE
        WRITE(LU,201) ICQ
201     FORMAT(1X,'SUSPENSION_COMPUTATION:',/,1X,
     &            'REFERENCE CONCENTRATION FORMULA',/,1X,
     &            'UNEXPECTED VALUE:',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 2.  ADVECTION VELOCITY -->  UCONV, VCONV
!     TAKING INTO ACCOUNT THE VERTICAL PROFILE
!     OF CONCENTRATIONS AND VELOCITIES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     OPTVF : TENS                  0 : NORMAL
!                                   1 : ADVECTION FIELD DOES NOT SATISFY
!                                       CONTINUITY
!
!     OPTVF : UNITS                 0 : CONSTANT = 0
!                                   1 : CHI-TUAN CONSTANT
!                                   2 : LEO POSTMA CONSTANT
!                                   SEE CVTRVF IN BIEF AND
!                                   V5.7 RELEASE NOTES
!
      YAFLULIM=.FALSE.
!
      IF(CORR_CONV.AND.(.NOT.SEDCO)) THEN
!
        CALL CPSTVC(U2D,T12)
        CALL SUSPENSION_CONV(TOB,XMVE, KSR,NPOIN,ZREF,U2D,V2D,HN,
     &                       UCONV,VCONV,KARMAN,ZERO,XWC,T12,RESOL,
     &                       GLOSEG1,GLOSEG2,MESH%NSEG,FLULIM,
     &                       YAFLULIM,SOLSYS_SIS,SOLSYS,
     &                       UCONV_TEL,VCONV_TEL)
!
!       ADVECTION FORM WHICH ACCEPTS AN ADVECTION FIELD
!       THAT DOES NOT SATISFY CONTINUITY + LEO-POSTMA CONSTANT
!
!       WITH 12: MASS CONSERVATION BUT NO MONOTONICITY
!                THE CORRECT THEORY
        OPTVF=12
!
!       WITH 2: MONOTONICITY BUT NO MASS CONSERVATION
!               WRONG THEORY
!       OPTVF=2
!
!       OPTVF=2 IS POSSIBLE BUT WITH MASS CONSERVATION SPOILED
!       THE UNIT (HERE 2) IS REDONE IN CVDFTR ACCORDING TO THE
!       VALUE OF RESOL, SO IT IS NOT IMPORTANT HERE.
!
      ELSE
!
!       POINTERS ARE USED TO AVOID COPY
!
        IF(SOLSYS_SIS.EQ.1) THEN
          UCONV%R=>U2D%R
          VCONV%R=>V2D%R
        ELSE
!         HERE UCONV_TEL IS PASSED ON
          UCONV%R=>UCONV_TEL%R
          VCONV%R=>VCONV_TEL%R
        ENDIF
!       ADVECTION FORM THAT REQUIRES AN ADVECTION FIELD
!       THAT SATISFIES CONTINUITY + LEO-POSTMA CONSTANT
        OPTVF=2
!
      ENDIF
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 3.  EROSION FLUX   : FLUER
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
!     SKIN FRICTION TAUP  --> T4
!
      CALL OS('X=CYZ   ', X= T4, Y= TOB, Z= MU, C=1.D0)
      CALL OS('X=+(Y,C)', X=T4, Y=T4, C=ZERO)
!
!     SAND ONLY
!
      IF(.NOT.MIXTE) THEN
        IF(.NOT.SEDCO) THEN
          IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_EROSION'
          CALL SUSPENSION_EROSION(T4,HN,FDM,FD90,AVA,NPOIN,CHARR,XMVE,
     &                            XMVS,VCE,GRAV,XWC,ZERO,
     &                            ZREF,AC,FLUER,CSTAEQ,QS_C,ICQ,U2D,V2D,
     &                            CSRATIO,DEBUG)
          IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_EROSION'
!
!         TODO: NOTE JMH : THIS SHOULD BE INCLUDED IN SUSPENSION_EROSION
!
          DO I=1,NPOIN
            FLUER%R(I)=MIN(FLUER%R(I),ELAY%R(I)*AVA(I)/DT*CSF_SABLE)
          ENDDO
!
!       MUD ONLY
!
        ELSE
          CALL SUSPENSION_EROSION_COH(T4,NPOIN,XMVS,
     &                      PARTHENIADES,FLUER,
     &                      TOCE_VASE,NOMBLAY,DT,MS_VASE)
!
          IF(NOMBLAY.EQ.1) THEN
            DO I=1,NPOIN
              FLUER%R(I)=MIN(FLUER%R(I),MS_VASE(I)/DT/XMVS)
            ENDDO
          ELSE
            DO I=1,NPOIN
              MSTOT=0.D0
              DO J=1,NOMBLAY
                MSTOT=MSTOT+MS_VASE(I+(J-1)*NPOIN)
              ENDDO
              FLUER%R(I)=MIN(FLUER%R(I),MSTOT/DT/XMVS)
            ENDDO
          ENDIF
        ENDIF
!
!       MIXED SEDIMENT
!       FIRST CLASS= SAND, SECOND CLASS = MUD
!
      ELSE
        IF(.NOT.SEDCO) THEN
          IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_FLUX_MIXTE'
          CALL SUSPENSION_FLUX_MIXTE(T4,FDM,NPOIN,CHARR,XMVE,XMVS,
     &                               VCE,GRAV,XWC,ZERO,
     &                               PARTHENIADES,FLUER,FLUER_VASE,
     &                               ZREF,AC,CSTAEQ,QS_C,ICQ,DEBUG,
     &                               AVAIL,NSICLA,ES,TOCE_VASE,
     &                               TOCE_SABLE,NOMBLAY,
     &                               DT,TOCE_MIXTE%R,MS_SABLE,
     &                               MS_VASE)
          IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_FLUX_MOY'
        ENDIF
        IF(SEDCO) CALL OS('X=Y     ',X=FLUER, Y=FLUER_VASE)
      ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  4. DEPOSITION FLUX   : FLUDPT =WC*T2
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!---> FLUDPT: IMPLICIT TERM
! TO ADD ? mak change ---> T2    : RATIO BETWEEN BOTTOM CONCENTRATION AND AVERAGE
!--->  CSRATIO   : RATIO BETWEEN BOTTOM CONCENTRATION AND AVERAGE
!             CONCENTRATION
!
      IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_DEPOT'
      CALL SUSPENSION_DEPOT(TOB,HN,NPOIN,HMIN,XWC,VITCD,ZERO,KARMAN,
     &    FDM,FD90,XMVE,T1,T2,ZREF,FLUDPT,DEBUG,SEDCO)
! TO ADD? mak   &    FDM,FD90,XMVE,T1,CSRATIO,T14,ZREF,FLUDPT,DEBUG,SEDCO,U2D,V2D,
! TO ADD? mak   &    CSTAEQ,DT)
!     &                      XMVE,T1,T2,ZREF,FLUDPT,DEBUG,SEDCO)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  5. DIFFIN A SPECIFIC TREATMENT IS DONE IF THE ADVECTION METHOD
!     IS THE CHARACTERISTICS: FREE OUTPUTS ARE TREATED LIKE DIRICHLET.
!     THIS SPECIFIC TREATMENT IS CANCELLED HERE BY SENDING A MODIFIED
!     VALUE FOR RESOL : RESOL_MOD (IN DIFFIN THE ONLY TEST IS:
!     IF(RESOL.EQ.1) THEN .... ELSE ....  ENDIF)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      RESOL_MOD=RESOL
      IF(RESOL_MOD.EQ.1) RESOL_MOD=2
      IF (DEBUG > 0) WRITE(LU,*) 'DIFFIN'
      CALL DIFFIN(MASKTR,LIMDIF%I,LICBOR%I,CLT%I,U2D%R,V2D%R,
     &            MESH%XNEBOR%R,MESH%YNEBOR%R,
     &            MESH%NBOR%I,NPTFR,
     &            KENT,KSORT,KLOG,KNEU,KDIR,KDDL,RESOL_MOD,
     &            MESH%NELBOR%I,NPOIN,
!                              NFRLIQ
     &            MSK,MASKEL%R,0,
!                  THOMFR FRTYPE
     &            .FALSE.,BID,    CS,CBOR,NUMLIQ,
     &            MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX)
      IF (DEBUG > 0) WRITE(LU,*) 'END DIFFIN'
!
!+++++++++++++++++++++++++++++++++++ELEVATION OF BOTTOM++++++++++++++++++++++++++++++++++++
!  6. BOUNDARY CONDITIONS : CBORCONC_V
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     IMPOSES THE EQUILIBRIUM CONCENTRATION FOR THE INFLOW NODES  !
!     HERE CBOR FROM BOUNDARY CONDITIONS FILE OR SUBROUTINE CONLIT
!     OVERWRITTEN
!
!     T2 = RATIO BETWEEN BOTTOM CONC.
!     AND AVERAGE CONC. MUST BE KEPT UNTIL THIS STAGE
!
      IF (DEBUG > 0) WRITE(LU,*) 'IMP_INFLOW_C'
      IF(IMP_INFLOW_C) THEN
!
        DO K = 1, NPTFR
          IF(CLT%I(K).EQ.KENT) THEN
            I = MESH%NBOR%I(K)
            IF(.NOT.SEDCO) THEN
              CBOR%R(K) = CSTAEQ%R(I)/T2%R(I)
              IF(MIXTE) CBOR%R(K) = FLUER%R(I)/T2%R(I)/XWC
            ELSE
              CBOR%R(K) = FLUER%R(I)/XWC
            ENDIF
!           THIS IS THE CONDITION TO HAVE NO EVOLUTION
!           CS%R(I) MAY BE DIFFERENT FROM CBOR%R(K) IF UNSTEADY FLOW
!           OR IF DIRFLU.EQ.2 (CASE OF PRIORITY TO FLUXES)
            FLUER%R(I)=FLUDPT%R(I)*CS%R(I)
          ENDIF
        ENDDO
!
      ENDIF
      IF (DEBUG > 0) WRITE(LU,*) 'FIN IMP_INFLOW_C'
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  7. SOLVING TRANSPORT EQUATION IF METHOD OF CHARACTERISTICS
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      IF(RESOL == 1) THEN
        IF (DEBUG > 0) WRITE(LU,*) 'CHARAC'
        CALL CHARAC(CS,CTILD,1,UCONV,VCONV,S,S,S,S,DT,IFAMAS,
     &              IELMT,NPOIN,1,1,1,
     &              MSK,AM1_S%X,AM1_S%D,AM1_S%D,
     &              TB,IT1%I,IT2%I,IT2%I,IT3%I,IT4%I,IT2%I,
     &              MESH,MESH%NELEM,MESH%NELMAX,MESH%IKLE,MESH%SURDET,
     &              AM2_S,T14,SLVTRA,1.D0,ENTETS,3,UNSV2D,1)
        IF (DEBUG > 0) WRITE(LU,*) 'END_CHARAC'
      ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  8. SOURCE AND SINKS
!     IMPLICIT SOURCE TERM FOR THE DEPOSITION       : T9
!     EXPLICIT SOURCE TERM WITHOUT PUNCTUAL SOURCES : T11
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      IF(OPTBAN.EQ.2) THEN
        CALL OS('X=XY    ',X=FLUER ,Y=MASKPT)
      ENDIF
!
      CALL OS('X=-Y    ',X=T9,Y=FLUDPT)
      CALL OS('X=Y     ',X=T11,Y=FLUER)
!
      DO I=1,NPOIN
        IF(HN%R(I).GT.HMIN) THEN
          T11%R(I)=T11%R(I)/HN%R(I)
        ELSE
          T11%R(I)=0.D0
!         FLUER WILL BE USED AS T11*HN, SO IT MUST BE
!         CANCELLED ACCORDINGLY, OTHERWISE MASS BALANCE WRONG
          FLUER%R(I)=0.D0
        ENDIF
      ENDDO
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  9. ADVECTION-DISPERSION STEP
!     CONFIGURATION OF ADVECTION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      TETAH  = 1.D0 - TETA_SUSP
      MASSOU = 0.D0
      AGGLOT=1.D0
      YASMI2 = .TRUE.
!
!     BOUNDARY FLUXES MUST BE SPECIFIED TO CVDFTR (FINITE VOLUMES CASE)
!     AND TO SUSPENSION_BILAN
!     SISYPHE ALONE     : THEY MUST BE COMPUTED
!     WHEN COUPLING     : THEY ARE GIVEN BY THE CALLING SUBROUTINE
!                         EXCEPT AT THE 1ST ITERATION
!
!     IF(CODE(1:7).NE.'TELEMAC'.OR.LT.EQ.1) THEN
      IF(CODE(1:7).NE.'TELEMAC'.OR.
     &  (CODE(1:9).EQ.'TELEMAC3D'.AND.LT.EQ.1)) THEN
        IF (DEBUG > 0) WRITE(LU,*) 'VECTOR'
        CALL VECTOR(FLBOR_SIS,'=','FLUBDF          ',IELBOR(IELMT,1),
!                        HPROP (HERE HPROP=HN, INVESTIGATE)
     &              1.D0,HN   ,HN,HN,UCONV,VCONV,VCONV,
     &              MESH,.TRUE.,MASKTR%ADR(5)%P)
!                                          5: MASK OF LIQUID BOUNDARIES
!                                             SEE DIFFIN IN BIEF 6.1
        IF (DEBUG > 0) WRITE(LU,*) 'FIN VECTOR'
      ELSE
        CALL OS('X=Y     ',X=FLBOR_SIS,Y=FLBOR_TEL)
!       MUST ALSO CHANGE BOUNDARY FLUXES IF THE ADVECTION
!       FIELD IS CORRECTED (T12 MUST HAVE BEEN KEPT SINCE
!                           CALL TO SUSPENSION_CONV)
        IF(CORR_CONV.AND..NOT.SEDCO) THEN
          CALL OSBD('X=CXY   ',FLBOR_SIS,T12,T12,1.D0,MESH)
        ENDIF
      ENDIF
!
!     FINITE VOLUMES ADVECTION USES THE TRUE H FROM THE PREVIOUS STEP
      IF(CODE(1:7).EQ.'TELEMAC') THEN
        IF(OPTBAN.NE.0) THEN
          CALL CPSTVC(CST,T1)
!         HN_TEL IS NOT CLIPPED
          DO I=1,NPOIN
            T1%R(I)=MAX(HN_TEL%R(I),HMIN)
          ENDDO
          HOLD=>T1
        ELSE
          HOLD=>HN_TEL
        ENDIF
      ELSE
!       IN THIS CASE H AND HN ARE CONFUNDED
        HOLD=>HN
      ENDIF
!
      IF(DEBUG > 0) WRITE(LU,*) 'APPEL DE CVDFTR'
!
      CALL CVDFTR
     & (CST, CTILD, CS, T2,
!                            H         HTILD
     &  DIFT, RESOL, .TRUE., HN, HOLD, HPROP,
     &  UCONV,VCONV,DM1,ZCONV,SOLSYS_SIS,
!                     TEXP SMH  YASMH   TIMP
     &  DISP, DISP_C, T11, T2, .FALSE., T9,  YASMI2,AM1_S,AM2_S,
     &  ZF, CBOR, AFBOR, BFBOR, LIMDIF, MASKTR, MESH,
     &  TB, T8, T12,  T4, T10, TE1, TE2, TE3,
     &  KDIR,KDDL,DT,ENTETS,TETA_SUSP,
!                      BILAN
     &  AGGLOT,ENTETS,.FALSE.,OPTADV,
     &  1, OPDTRA, OPTBAN, MSK, MASKEL, MASKPT, MBOR, S,
!               OPTSOU
     &  MASSOU, 1,     SLVTRA,FLBOR_SIS,VOLU2D,V2DPAR,UNSV2D,
     &  OPTVF,FLBORTRA,
     &  FLULIM,YAFLULIM,FLULIM,.FALSE.,DIRFLU,.FALSE.,T8    ,0.D0,
!                               RAIN  ,PLUIE ,TRAIN
     &  FLULIM     ,.FALSE.,MAXADV,TB2,NCO_DIST,NSP_DIST)
!       GIVEN_FLUX   FLUX_GIVEN (NOW THE FLUX CAN BE GIVEN, THIS COULD
!       BE AN OPTIMISATION, AS HERE IT IS RECOMPUTED FOR EVERY CLASS...)
!
      IF(DEBUG > 0) WRITE(LU,*) 'END_CVDFTR'
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 10. BED EVOLUTION DUE TO NET EROSION/DEPOSITUON FLUX
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      DO I=1,NPOIN
        FLUDP%R(I)=FLUDPT%R(I)*CST%R(I)
      ENDDO
!
!     COMPUTES EVOLUTION AND UPDATES DATA
!     TASS TO BE PASSED IN ARGUMENT
!
!
      IF(.NOT.SEDCO) THEN
        IF(.NOT.MIXTE) THEN
          CALL OS('X=Y-Z   ', X=ZFCL_S, Y=FLUDP, Z=FLUER)
          CALL OS('X=CX    ', X=ZFCL_S, C=DT/CSF_SABLE)
        ELSE
          DO I=1, NPOIN
            DO J= 1, NOMBLAY
              CONC_SABLE(I,J)=XMVS
            ENDDO
          ENDDO
          CALL SUSPENSION_EVOL(ZFCL_S,FLUDP,FLUER,DT,
     &                NPOIN,XMVS,T3,MS_SABLE,ES_SABLE,
     &                   CONC_SABLE,NOMBLAY)
        ENDIF
      ELSE
        CALL SUSPENSION_EVOL(ZFCL_S,FLUDP,FLUER,DT,
     &      NPOIN,XMVS,T3,MS_VASE,ES_VASE,
     &      CONC,NOMBLAY)
      ENDIF
!
!     WRITES OUT THE MIN/MAX VALUES TO THE LISTING
!
      IF(ENTETS) THEN
        IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_LISTING'
        CALL SUSPENSION_LISTING(MESH,CST,ZFCL_S,UCONV,VCONV,
     &                          MASKEL,IELMT,DT,MSK,T1)
        IF(DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_LISTING'
      ENDIF
!
!     MASS-BALANCE FOR THE SUSPENSION
!
      IF(BILMA) THEN
        IF(SEDCO) THEN
          IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BILAN_COH'
          CALL SUSPENSION_BILAN_COH
     &         (MESH,CST,HN,MASKEL,IELMT,ITRA,LT,NIT,DT,XMVS,
     &         MS_VASE,NOMBLAY,NPOIN,
     &         MASSOU,MASED0,MSK,ENTETS,MASTEN,MASTOU,
     &         MASINI,T1,T2,T3,MASFIN,MASDEPT,MASDEP,AGGLOT,VOLU2D,
     &         NUMLIQ,NFRLIQ,NPTFR,FLBORTRA,SEDCO)
          IF(DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_BILAN_COH'
        ELSE
!Modifs CVL
          IF(MIXTE) THEN
            IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BILAN_COH'
            CALL SUSPENSION_BILAN_COH
     &           (MESH,CST,HN,MASKEL,IELMT,ITRA,LT,NIT,DT,XMVS,
     &           MS_SABLE,NOMBLAY,NPOIN,
     &           MASSOU,MASED0,MSK,ENTETS,MASTEN,MASTOU,
     &           MASINI,T1,T2,T3,MASFIN,MASDEPT,MASDEP,AGGLOT,VOLU2D,
     &           NUMLIQ,NFRLIQ,NPTFR,FLBORTRA,SEDCO)
            IF(DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_BILAN_COH'
          ELSE
! fin modifs CVL
            IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BILAN'
            CALL SUSPENSION_BILAN
     &          (MESH,CST,HN,ZFCL_S,MASKEL,IELMT,ITRA,LT,NIT,
     &           DT,CSF_SABLE,MASSOU,MASED0,MSK,ENTETS,MASTEN,MASTOU,
     &           MASINI,T2,T3,MASFIN,MASDEPT,MASDEP,AGGLOT,VOLU2D,
     &           NUMLIQ,NFRLIQ,NPTFR,FLBORTRA)
            IF(DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_BILAN'
          ENDIF
        ENDIF
      ENDIF
!
!
      CALL OS('X=Y     ', X=CS, Y=CST)
!
!     NOTE: ARE QSCLXS AND QSCLYS USEFUL ????????
!
      CALL OS('X=YZ    ', X=T1, Y=UCONV, Z=HN)
      CALL OS('X=YZ    ', X=T2, Y=VCONV, Z=HN)
      CALL OS('X=YZ    ', X=QSCLXS, Y=CS, Z=T1)
      CALL OS('X=YZ    ', X=QSCLYS, Y=CS, Z=T2)
!
!     RESTORING UCONV POINTERS
!
      UCONV%R=>SAVE_UCONV
      VCONV%R=>SAVE_VCONV
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
