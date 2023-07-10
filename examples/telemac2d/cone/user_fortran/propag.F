!                   *****************
                    SUBROUTINE PROPAG
!                   *****************
!
     &(U,V,H,UCONV,VCONV,CONVV,H0,PATMOS,ATMOS,
     & HPROP,UN,VN,HN,UTILD,VTILD,HTILD,DH,DU,DV,DHN,VISC,VISC_S,FU,FV,
     & SMH,MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,A23,A32,MBOR,
     & CV1,CV2,CV3,UBOR,VBOR,AUBOR,HBOR,DIRBOR,
     & TE1,TE2,TE3,TE4,TE5,T1,T2,T3,T4,T5,T6,T8,
     & LIMPRO,MASK,GRAV,ROEAU,CF,DIFVIT,IORDRH,IORDRU,LT,DT,
     & TETAH,TETAU,TETAD,
     & AGGLOH,AGGLOU,KDIR,INFOGR,KFROT,ICONVF,
     & ISOUSI,BILMAS,MASSES,MASS_RAIN,YASMH,OPTBAN,CORCON,
     & OPTSUP,MSK,MASKEL,MASKPT,RO,ROVAR,
     & MAT,RHS,UNK,TB,S,PRECCU,SOLSYS,OPDVIT,OPTSOU,
     & NFRLIQ,SLVPRO,EQUA,VERTIC,ADJO,ZFLATS,TETAZCOMP,UDEL,VDEL,DM1,
     & ZCONV,FLBOR,BM1S,BM2S,CV1S,VOLU2D,V2DPAR,UNSV2D,
     & NDGA1,NDGB1,NWEIRS,NPSING,HFROT,FLULIM,YAFLULIM,
     & FLULIMEBE,YAFLULIMEBE,RAIN,PLUIE,
     & MAXADV,OPTADV_VI,IKLE,NELMAX)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    PROPAGATION - DIFFUSION - SOURCE TERMS STEP TO SOLVE
!+                THE SAINT-VENANT EQUATIONS.
!+
!+
!+      BOUNDARY CONDITIONS:
!+
!+
!+      ==>   NEUMANN CONDITION
!+
!+
!+            * DIFFUSION   : NU DU/DN = AUBOR . U;
!+                            TREATS THE DIFFUSION MATRIX DIRECTLY
!+
!+
!+            * PROPAGATION : THE BOUNDARY TERMS ARE TREATED IN
!+                            THE SECOND MEMBERS (IMPLICIT)
!+
!+
!+      ==>   DIRICHLET CONDITION
!+
!+
!+            * DIFFUSION, PROPAGATION :
!+                            TREATED USING MODIFIED EQUATIONS IN " PROCLI "
!code
!+      IN MATRIX FORM:
!+
!+                   N+1          N+1          N+1
!+             AM1  H     +  BM1 U     +  BM2 V     =  CV1
!+
!+            T     N+1           N+1
!+          -  CM1 H      +  AM2 U                  =  CV2
!+
!+            T     N+1                        N+1
!+          -  CM2 H                   +  AM3 V     =  CV3
!
!note     BM* REPRESENT DIVERGENCE MATRICES;
!+            BM1: DERIVATION RELATIVE TO X;
!+            BM2: DERIVATION RELATIVE TO Y.
!note
!+THE TRANSPOSE OF MATRICES BM* IS EQUAL TO THE OPPOSITE
!+            OF GRADIENT. SOME SIGNS ARE THEREFORE OPPOSITE IN
!+            THE EQUATIONS OF SPEED.
!note
!+THE LAPLACIAN MATRIX (TM1) HAS BEEN INTEGRATED IN PART.
!+            THE SIGN IS THEREFORE OPPOSITE IN THE EQUATIONS OF
!+            SPEED.
!
!history  JMH
!+        07/05/2007
!+
!+   MODIFICATION ON THE SOURCES IN CASE OF MASS-LUMPING
!
!history
!+        10/06/2008
!+
!+   FINITE VOLUME ADVECTION FOR SPEEDS
!
!history
!+        02/10/2008
!+
!+   CALL TO CVTRVF (ONE MORE WORKING ARRAY)
!
!history
!+        20/07/2009
!+
!+   ICONVF (2) = 5 MANDATORY, ALL OTHER CASES ERASED
!
!history
!+        22/07/2009
!+
!+   EQUALITY OF FLUXES IMPOSED ON EITHER SIDE OF A WEIR
!
!history  J-M HERVOUET (LNHE)
!+        09/10/2009
!+        V6P0
!+   PARAMETERISED ADVECTION OPTIONS
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
!+        09/08/2011
!+        V6P2
!+   Adaptation to parallelism
!
!history  J-M HERVOUET (LNHE)
!+        249/02/2012
!+        V6P2
!+   Rain and evaporation added
!
!history  J-M HERVOUET (LNHE)
!+        09/04/2013
!+        V6P3
!+   DIMGLO=MESH%GLOSEG%DIM1 used in call to CVTRVF_POS_2. Strangely
!+   avoids an "array temporary created" with Intel compiler.
!
!history  J-M HERVOUET (LNHE)
!+        12/04/2013
!+        V6P3
!+   Value of NELBOR controlled for allowing bound checking.
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        12/06/2013
!+        V6P3
!+   Adaptation to the dynamic allocation of weirs
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments. Arguments C0 and COTOND removed. Incident wave
!+   removed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        17/04/2014
!+        V7P0
!+   Correction for weirs. The function P_DSUM for weirs must be called
!+   in all processors, so it must be called outside the test:
!+   IF(MESH%NPTFR.GT0) where it was in previous versions.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        16/05/2014
!+        V7P0
!+   A copy of LIMPRO is done to be sent to cvtrvf (that may change it).
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7
!+        Compensated assembly of the interface node
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        15/03/2016
!+        V7P2
!+   Enabling advection solver 15 (ERIA) for velocities with a double
!+   to cvtrvf_pos. Advection sschemes ADV_PSI_NC and ADV_NSC_NC removed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/04/2016
!+        V7P2
!+   A23%STOX and A32%STOX set to 1 to enable a call by MATVEC because
!+   these matrices are not built by MATRIX.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        07/09/2016
!+        V7P2
!+   Adaptation to splitting of cvtrvf_pos into cvtrvf_nerd and
!+   cvtrvf_eria.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A23            |<->| MATRIX
!| A32            |<->| MATRIX
!| ADJO           |-->| IF YES : ADJOINT MODE
!| AGGLOH         |-->| KEYWORD: 'MASS-LUMPING ON H'
!| AGGLOU         |-->| KEYWORD: 'MASS-LUMPING ON VELOCITY'
!| AM1            |<->| MATRIX APPLYING TO H
!| AM2            |<->| MATRIX APPLYING TO U
!| AM3            |<->| MATRIX APPLYING TO V
!| ATMOS          |-->| IF YES, ATMOSPHERIC PRESSURE IN PATMOS
!| AUBOR          |<--| LAW OF FRICTION ON BOUNDARIES
!|                |   | NUT*DU/DN=AUBOR*U+BUBOR
!| BILMAS         |-->| LOGICAL TRIGGERING A MASS BALANCE INFORMATION
!| BM1S           |<->| MATRIX
!| BM2            |<->| MATRIX
!| BM2S           |<->| MATRIX
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| CM1            |<->| MATRIX
!| CM2            |<->| MATRIX
!| CONVV          |-->| ARRAY OF LOGICAL GIVING THE VARIABLES TO BE
!|                |   | ADVECTED
!|                |   | CONVV(1):U,V CONVV(2):H
!| CORCON         |-->| CONTINUITY CORRECTION ON POINTS WITH
!|                |   | IMPOSED DEPTH (COMPATIBLE FLUX IS COMPUTED)
!| CV1            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV2            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV3            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV1S           |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| DH             |<--| H(N+1)-H(N)
!| DHN            |<--| H(N)-H(N-1)
!| DIFVIT         |-->| IF YES, DIFFUSION OF VELOCITY
!| DIRBOR         |<--| BLOCK WITH DIRICHLET BOUNDARY CONDITIONS
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| DT             |-->| TIME STEP
!| DU             |<--| U(N+1)-U(N)
!| DV             |<--| V(N+1)-V(N)
!| EQUA           |-->| KEYWORD: 'EQUATIONS'
!| FLBOR          |<--| FLUXES AT BOUNDARY POINTS
!| FLULIM         |-->| FLUX LIMITATION
!| FU             |<->| SOURCE TERMS ON VELOCITY U
!| FV             |<->| SOURCE TERMS ON VELOCITY V
!| GRAV           |-->| GRAVITY
!| H0             |-->| REFERENCE DEPTH
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| HN             |-->| DEPTH AT TIME T(N)
!| HFROT          |-->| KEYWORD: 'DEPTH IN FRICTION TERMS'
!| HPROP          |-->| PROPAGATION DEPTH
!| HTILD          |-->| DEPTH AFTER ADVECTION
!| ICONVF         |-->| TYPE OF ADVECTION: 4 INTEGERS
!|                |   | ICONVF(1) : U AND V
!|                |   | ICONVF(2) : H (MANDATORY VALUE = 5)
!|                |   | ICONVF(3) : TRACERS
!|                |   | ICONVF(4) : K AND EPSILON
!| IKLE           |-->| CONNECTIVITY TABLE
!| INFOGR         |-->| IF YES, INFORMATION ON GRADIENT
!| IORDRH         |-->| ORDER OF INITIAL GUESS OF H
!| IORDRU         |-->| ORDER OF INITIAL GUESS OF U
!| ISOUSI         |-->| NUMBER OF SUB-ITERATION IN THE TIME-STEP
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KFROT          |-->| KEYWORD: 'LAW OF BOTTOM FRICTION'
!| LIMPRO         |<->| BOUNDARY CONDITIONS FOR H, U V PER POINTS
!|                |   | AND SEGMENTS
!| LT             |-->| ITERATION NUMBER
!| MASK           |-->| BLOCK OF MASKS FOR SEGMENTS :
!|                |   | MASK(MSK1): 1. IF KDIR ON U 0. ELSE
!|                |   | MASK(MSK2): 1. IF KDIR ON V 0. ELSE
!|                |   | MASK(MSK3): 1. IF KDDL ON U 0. ELSE
!|                |   | MASK(MSK4): 1. IF KDDL ON V 0. ELSE
!|                |   | MASK(MSK6): 1. IF KNEU ON V 0. ELSE
!|                |   | MASK(MSK7): 1. IF KOND 0. ELSE
!|                |   | MASK(MSK9): 1. IF KDIR ON H (POINT)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MASSES         |-->| MASS OF WATER ADDED BY SOURCE TERM
!| MASS_RAIN      |-->| MASS ADDED BY RAIN OR EVAPORATION
!| MAXADV         |-->| MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES
!| MAT            |<--| BLOCK OF MATRICES
!| MBOR           |<--| BOUNDARY MATRIX
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPSING         |-->| NUMBER OF POINTS FOR EVERY SINGULARITY.
!| NDGA1          |-->| NDGA1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side1)
!| NDGB1          |-->| NDGB1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side2)
!| NWEIRS         |-->| NUMBER OF SINGULARITIES
!| OPDVIT         |-->| OPTION FOR DIFFUSION OF VELOCITIES
!| OPTADV_VI      |-->| OPTION FOR THE ADVECTION SCHEME OF VELOCITIES
!| OPTBAN         |-->| KEYWORD: 'OPTION FOR THE TREATMENT OF TIDAL FLATS'
!| OPTSOU         |-->| KEYWORD: 'TYPE OF SOURCES'
!| OPTSUP         |-->| KEYWORD: 'SUPG OPTION'
!| PATMOS         |-->| ATMOSPHERIC PRESSURE
!| PLUIE          |-->| RAIN OR EVAPORATION IN M/S IN A BIEF_OBJ
!| PRECCU         |-->| KEYWORD: 'C-U PRECONDITIONING'
!| RAIN           |-->| IF YES, RAIN OR EVAPORATION
!| RHS            |<->| BLOCK OF PRIVATE BIEF_OBJ STRUCTURES
!| RO             |-->| WATER DENSITY IF VARIABLE
!| ROEAU          |-->| WATER DENSITY
!| ROVAR          |-->| IF YES, VARIABLE WATER DENSITY.
!| S              |-->| VOID STRUCTURE
!| SLVPRO         |-->| SOLVER STRUCTURE FOR PROPAGATION
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SOLSYS         |-->| KEYWORD: 'TREATMENT OF THE LINEAR SYSTEM'
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| TB             |<->| BLOCK WITH T1,T2,...
!| TE1            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE2            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE3            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE4            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE5            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TETAD          |-->| IMPLICITATION ON DIFFUSION
!| TETAH          |-->| IMPLICITATION OF H IN U EQUATION
!| TETAU          |-->| IMPLICITATION OF U AND
!| TM1            |<->| MATRIX
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| UCONV          |-->| X-COMPONENT OF ADVECTION VELOCITY FIELD
!| VCONV          |-->| Y-COMPONENT OF ADVECTION VELOCITY FIELD
!| UDEL           |<--| COMPATIBLE X-COMPONENT OF ADVECTION VELOCITY FIELD
!| UN             |<->| X-COMPONENT OF VELOCITY AT TIME T(N)
!| VN             |<->| Y-COMPONENT OF VELOCITY AT TIME T(N)
!| UNK            |<->| BLOCK OF UNKNOWNS
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| UTILD          |-->| VELOCITY U IF ADVECTED BY CHARACTERISTICS
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| VBOR           |-->| CONDITIONS AUX LIMITES SUR V.
!| VDEL           |<--| COMPATIBLE Y-COMPONENT OF ADVECTION VELOCITY FIELD
!| VERTIC         |-->| IF YES, THERE ARE VERTICAL STRUCTURES
!| VISC           |-->| VISCOSITY COEFFICIENTS ALONG X,Y AND Z .
!|                |   | IF P0 : PER ELEMENT
!|                |   | IF P1 : PERR POINT
!| VISC_S         |<->| WORK ARRAY FOR SAVING VISC
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS, NOT ASSEMBLED IN PARALLEL
!| VTILD          |-->| VELOCITY V IF ADVECTED BY CHARACTERISTICS
!| W1             |<->| WORK ARRAY
!| YAFLULIM       |-->| IF, YES, FLULIM TAKEN INTO ACCOUNT
!| YASMH          |-->| IF YES, SMH TAKEN INTO ACCOUNT
!| ZCONV          |<--| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZFLATS         |<--| ELEVATION OF BOTTOM, MODIFIED FOR TIDAL FLATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : ADV_CAR,ADV_SUP,ADV_NSC,ADV_PSI,
     &                                 ADV_LPO,
     &                                 ADV_NSC_TF,ADV_PSI_TF,ADV_LPO_TF,
     &                                 KDDL
      USE DECLARATIONS_TELEMAC2D, ONLY : TYPSEUIL,IT1,IT2,TB2,NCO_DIST,
     &                                   NSP_DIST,OPT_HNEG
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE INTERFACE_TELEMAC2D, EX_PROPAG => PROPAG
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LT,OPTSUP(4),KDIR,KFROT,ICONVF(4),NWEIRS
      INTEGER, INTENT(IN) :: NELMAX
      INTEGER, INTENT(IN) :: IORDRH,IORDRU,ISOUSI,OPTBAN,OPTSOU,SOLSYS
      INTEGER, INTENT(IN) :: OPDVIT,NFRLIQ,HFROT,MAXADV,OPTADV_VI
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: TETAU,TETAD,TETAH,AGGLOH,AGGLOU
      DOUBLE PRECISION, INTENT(IN)    :: DT,GRAV,ROEAU
      DOUBLE PRECISION, INTENT(IN)    :: TETAZCOMP
      DOUBLE PRECISION, INTENT(INOUT) :: MASSES,MASS_RAIN
      LOGICAL, INTENT(IN) :: BILMAS,ATMOS,DIFVIT,INFOGR,CONVV(4),MSK
      LOGICAL, INTENT(IN) :: YASMH,ROVAR,PRECCU,VERTIC,ADJO,CORCON
      LOGICAL, INTENT(IN) :: YAFLULIM,RAIN,YAFLULIMEBE
      TYPE(SLVCFG), INTENT(INOUT)     :: SLVPRO
      CHARACTER(LEN=20),  INTENT(IN)  :: EQUA
!
!  STRUCTURES OF VECTORS
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: NPSING,NDGA1,NDGB1,FLULIMEBE
      TYPE(BIEF_OBJ), INTENT(IN)    :: UCONV,VCONV,SMH,UN,VN,HN
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU2D,V2DPAR,UNSV2D,FLULIM
      TYPE(BIEF_OBJ), INTENT(INOUT) :: RO,UDEL,VDEL,DM1,ZCONV,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: UTILD,VTILD,PATMOS,CF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: U,V,H,CV1,CV2,CV3,DH,DHN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CV1S
      TYPE(BIEF_OBJ), INTENT(INOUT) :: DU,DV,FU,FV,VISC,VISC_S,HTILD
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UBOR,VBOR,HBOR,AUBOR,LIMPRO
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,MASKPT,ZF,PLUIE
      TYPE(BIEF_OBJ), INTENT(IN)    :: HPROP,H0
!
!     TE : BY ELEMENT               TE4,TE5 ONLY IF OPTBAN=3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TE1,TE2,TE3,TE4,TE5,ZFLATS
!     T  : BY POINT
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T8
!     DUMMY STRUCTURE
      TYPE(BIEF_OBJ), INTENT(IN)    :: S
!
!-----------------------------------------------------------------------
!
!  STRUCTURES OF MATRICES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A23,A32,MBOR,BM1S,BM2S
!
!-----------------------------------------------------------------------
!
!  STRUCTURES OF BLOCKS
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASK,MAT,RHS,UNK,TB,DIRBOR
!
!-----------------------------------------------------------------------
!
!  STRUCTURE OF MESH
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      CALL OS('X=Y     ',X=UDEL,Y=U)
      CALL OS('X=Y     ',X=VDEL,Y=V)
      CALL OS('X=0     ',X=DM1)
      DO I=1,3*MESH%NELMAX
        ZCONV%R(I)=0.D0
      ENDDO
!
!     TO AVOID CALLING CORRECTION_DEPTH_2D
!
      OPT_HNEG=1
!
!-----------------------------------------------------------------------
!
      RETURN
      END
