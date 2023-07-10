!                   ********************************
                    SUBROUTINE BEDLOAD_SOLIDISCHARGE
!                   ********************************
!
     &(MESH,U2D,V2D,UNORM,HN,TW,UW,MU,TOB,CF,TOBW,FW,THETAW,
     & AVA,MASKPT,MASKEL,ACLADM,UNLADM,KSP,KSR,LIQBOR,
     & DEBUG,NPOIN,NPTFR,IELMT,ICF,KENT,OPTBAN,
     & HIDFAC,GRAV,DM,D90,XWC,XMVE,XMVS,VCE,HMIN,
     & HIDI,KARMAN,ZERO,PI,KARIM_HOLLY_YANG,
     & SUSP,MSK,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     & T11,T12,AC,HIDING,QSC,QSS,
     & SLOPEFF,COEFPN,PHISED,CALFA,SALFA,BETA,ZF_C,S,
     & DEVIA,BETA2,SECCURRENT,
     & BIJK,HOULE,UNSV2D,U3D,V3D,CODE,SANFRA)
!
!***********************************************************************
! SISYPHE   V8P0                                   12/09/2018
!***********************************************************************
!
!brief
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        20/05/1995
!+        V5P1
!+
!
!history  B. MINH DUC
!+        **/12/2001
!+        V5P2
!+
!
!history  M. GONZALES DE LINARES
!+        **/07/2002
!+        V5P3
!+
!
!history  C. VILLARET
!+        **/10/2003
!+        V5P4
!+
!
!history  F. HUVELIN
!+        14/09/2004
!+        V5P5
!+
!
!history  J.-M. HERVOUET
!+        11/03/2008
!+        V5P9
!+   MODIFICATIONS FOR PARALLEL MODE
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!history  J-M HERVOUET (EDF-LNHE)
!+        22/02/2012
!+        V6P2
!+  Dirichlet treatment of QBOR removed
!+  It is now done in bedload_solvs_ef and vf
!+
!history  R KOPMANN (BAW)
!+        10/05/2016
!+        V7P2
!+ CALFA,SALFA dependent of grain classes
!
!history  F.CORDIER & P.TASSI (EDF-LNHE)
!+        12/09/2018
!+        V8P0
!+  PARSING of SANFRA for the formula of WILCOCK and CROWE (2003)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AVA            |-->| PERCENT AVAILABLE
!| BETA           |-->| COEFFICIENT FOR SLOPING BED EFFECT ( KOCH AND FLOKSTRA)
!| BETA2          |-->| COEFFICIENT FOR THE DEVIATION  (TALMON ET AL.)
!| BIJK           |-->| COEFFICIENT OF THE BIJKER FORMULA
!| CALFA          |<->| COSINUS OF THE ANGLE BETWEEN MEAN FLOW AND TRANSPORT
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| COEFPN         |<->| CORRECTION OF TRANSORT FOR SLOPING BED EFFECT
!| D90            |-->| D90
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| DEVIA          |-->| SLOPE EFFECT FORMULA FOR DEVIATION
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| FW             |---| QUADRATIC FRICTION COEFFICIENT (WAVE)
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HIDFAC         |-->| HIDING FACTOR FORMULAS
!| HIDI           |-->| HIDING FACTOR FOR PARTICULAR SIZE CLASS (HIDFAC =0)
!| HIDING         |-->| HIDING FACTOR CORRECTION
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| HOULE          |-->| LOGICAL, FOR WAVE EFFECTS
!| ICF            |-->| BED-LOAD OR TOTAL LOAD TRANSPORT FORMULAS
!| IELMT          |-->| NUMBER OF ELEMENTS
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KSP            |-->| BED SKIN ROUGHNESS
!| KSR            |-->| RIPPLE BED ROUGHNESS
!| LIQBOR         |-->| TYPE OF BOUNDARY CONDITION FOR QS
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MASKPT         |-->| MASKING PER POINT
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| MU             |<->| CORRECTION FACTOR FOR BED ROUGHNESS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS
!| PHISED         |-->| ANGLE OF REPOSE OF THE SEDIMENT
!| PI             |-->| PI
!| QSC            |<->| BED LOAD TRANSPORT
!| QSS            |<->| SUSPENDED LOAD TRANSPORT RATE
!| S              |-->| VOID STRUCTURE
!| SALFA          |<->| SINUS OF THE ANGLE BETWEEN TRANSPORT RATE AND CURRENT
!| SECCURRENT     |-->| LOGICAL, PARAMETRISATION FOR SECONDARY CURRENTS
!| SLOPEFF        |-->| LOGICAL, SLOPING BED EFFECT OR NOT
!| SUSP           |-->| LOGICAL, SUSPENSION
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T10            |<->| WORK BIEF_OBJ STRUCTURE
!| T11            |<->| WORK BIEF_OBJ STRUCTURE
!| T12            |<->| WORK BIEF_OBJ STRUCTURE
!| T13            |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| T9             |<->| WORK BIEF_OBJ STRUCTURE
!| THETAW         |-->| ANGLE BETWEEN WAVE AND CURRENT
!| TOB            |<->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOBW           |-->| WAVE INDUCED SHEAR STRESS
!| TW             |-->| WAVE PERIOD
!| U2D            |<->| MEAN FLOW VELOCITY X-DIRECTION
!| UNLADM         |-->| MEAN DIAMETER OF ACTIVE STRATUM LAYER
!| UNORM          |<->| NORM OF THE MEAN FLOW VELOCITY
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| UW             |-->| ORBITAL WAVE VELOCITY
!| V2D            |<->| MEAN FLOW VELOCITY Y-DIRECTION
!| VCE            |-->| WATER VISCOSITY
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| WATER DENSITY
!| XWC            |-->| SETTLING VELOCITY
!| ZERO           |-->| ZERO
!| ZF_C           |<->| BEDLOAD EVOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_SOLIDISCHARGE => BEDLOAD_SOLIDISCHARGE
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D, V2D,  HN, TW, UW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UNORM ,MU, KSR ,KSP
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, CF, TOBW, FW, THETAW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKPT, MASKEL
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, UNLADM, LIQBOR
      INTEGER,          INTENT(IN)    :: DEBUG
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR, IELMT, ICF
      INTEGER,          INTENT(IN)    :: KENT, OPTBAN,HIDFAC
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, DM, D90, XWC, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: VCE, HMIN
      DOUBLE PRECISION, INTENT(IN)    :: HIDI
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO, PI
      DOUBLE PRECISION, INTENT(IN)    :: KARIM_HOLLY_YANG
      LOGICAL,          INTENT(IN)    :: SUSP, MSK,SECCURRENT,HOULE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T7,T8,T9,T10,T11,T12
      DOUBLE PRECISION, INTENT(INOUT) :: AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC,QSS
!
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: PHISED,BETA,BETA2
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF_C,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CALFA,SALFA,COEFPN
!
      DOUBLE PRECISION, INTENT(IN)    :: BIJK,AVA(NPOIN)
!
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
!
      DOUBLE PRECISION, INTENT(IN)    :: SANFRA(NPOIN)
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION U3DNORM
!
      INTEGER          :: I
!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_EFFPNT'
!
!     SLOPE EFFECT
!
      IF(CODE(1:9).EQ.'TELEMAC3D') THEN
        DO I=1,NPOIN
          U3DNORM=SQRT(U3D%R(I)**2+V3D%R(I)**2)
          IF(U3DNORM.GE.1.D-12) THEN
            CALFA%R(I)=U3D%R(I)/U3DNORM
            SALFA%R(I)=V3D%R(I)/U3DNORM
          ELSE
            CALFA%R(I)=1.D0
            SALFA%R(I)=0.D0
          ENDIF
        ENDDO
      ELSE
        CALL OS('X=Y/Z   ',X=CALFA, Y=U2D, Z=UNORM, C=0.D0,
     &                     IOPT=2, INFINI=1.D0, ZERO=1.D-12)
        CALL OS('X=Y/Z   ',X=SALFA, Y=V2D, Z=UNORM, C=0.D0,
     &                     IOPT=2, INFINI=0.D0, ZERO=1.D-12)
      ENDIF
!
      IF(SLOPEFF.EQ.0) CALL OS('X=C     ',X=COEFPN,C=1.D0)
!
      IF(SLOPEFF.NE.0.OR.DEVIA.NE.0) THEN
        CALL BEDLOAD_EFFPNT
     &     (MASKEL,LIQBOR,S,ZF_C,NPOIN,NPTFR,IELMT,
     &      KENT,BETA,PI,MSK,MESH,T1,T2,T3,T4,
     &      COEFPN,CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2,
     &      TOB,XMVS,XMVE,DM,GRAV,UNSV2D)
      ENDIF
!
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_EFFPNT'
!
!     MASKING/EXPOSURE COEFFICIENT
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_HIDING_FACTOR'
!
!     WITH HUNZIKER FORMULATION (6), THE HIDING FACTOR IS COMPUTED
!     WITH THE SOLID DISCHARGE (SEE BEDLOAD_HUNZ_MEYER.F)
!
      IF(ICF.NE.6) THEN
        CALL BEDLOAD_HIDING_FACTOR
     &     (ACLADM, HIDFAC, NPOIN, HIDI, DM, KARIM_HOLLY_YANG, HIDING)
      ENDIF
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_HIDING_FACTOR'
!
!     QSC COMPUTED USING EMPIRICAL FORMULATION : T1 = DQSC/DH                           !
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_FORMULA'
!
      CALL BEDLOAD_FORMULA
     &  (U2D,V2D, UNORM,HN, CF, MU,TOB, TOBW, UW, TW, THETAW, FW,
     &   ACLADM, UNLADM, KSP,KSR,AVA, NPOIN, ICF, HIDFAC, XMVS, XMVE,
     &   DM, GRAV, VCE, HMIN, XWC, D90, KARMAN, ZERO,
     &   PI, SUSP, AC, HIDING, T1, T2, T3, T4, T5, T6, T7, T8, T9,
     &   T10, T11, T12, QSC, QSS, IELMT,SECCURRENT,
     &   SLOPEFF, COEFPN, CALFA, SALFA, BIJK, HOULE, SANFRA)
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_FORMULA'
!
!     TIDAL FLATS
!
      IF(OPTBAN.EQ.2) THEN
        IF (DEBUG > 0) WRITE(LU,*) 'TIDAL_FLATS_TREATMENT'
        CALL OS('X=XY    ', X=QSC, Y=MASKPT)
        IF (DEBUG > 0) WRITE(LU,*) 'END_TIDAL_FLATS_TREATMENT'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
