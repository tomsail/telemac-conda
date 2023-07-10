!             *************************************
              SUBROUTINE BEDLOAD_SOLIDISCHARGE_GAIA
!             *************************************
!
     &(MESH,U2D,V2D,UNORM,HN,TW,UW,MU,TOB,CF,TOBW,FW,THETAW,
     & RATIO_SAND,MASKPT,MASKEL,ACLADM,UNLADM,KSP,KSR,LIQBOR,
     & DEBUG,NPOIN,NPTFR,IELMT,ICF,KENT,OPTBAN,
     & HIDFAC,GRAV,DCLA,XWC,XMVE,XMVS,VCE,HMIN,
     & HIDI,KARMAN,ZERO,PI,K_H_Y,
     & SUSP,MSK,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     & T11,T12,T13,AC,HIDING,QSC,QSS,
     & SLOPEFF,COEFPN,COEFCR,PHISED,CALFA,SALFA,BETA,ZF,S,
     & DEVIA,BETA2,SECCURRENT,
     & BIJK,HOULE,UNSV2D,U3D,V3D,CODE,H_TEL,
     & HW,THETAC,TOBCW_MEAN,TOBCW_MAX,CSTAEQ,SANFRA)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief computation of solid discharge: qsc.
!        According to the formula used, it also computes qss
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] AC          Critical shields parameter
!>@param[in]     ACLADM      Mean diameter of active layer
!>@param[in]     BETA        Coefficient for sloping bed effect ( koch and flokstra)
!>@param[in]     BETA2       Coefficient for the deviation  (talmon et al.)
!>@param[in]     BIJK        Coefficient of the bijker formula
!>@param[in,out] CALFA       Cosinus of the angle between mean flow and transport
!>@param[in]     CF          Quadratic friction coefficient
!>@param[in,out] COEFPN      Correction of transport for sloping bed effect
!>@param[in,out] COEFCR      Correction of critical Shields for sloping bed effec
!>@param[in,out] CSTAEQ      Equilibrium sediment concentration
!>@param[in]     DEBUG       Flag for debugging
!>@param[in]     DEVIA       Slope effect formula for deviation
!>@param[in]     DCLA         Sediment grain diameter
!>@param[in]     FW          Quadratic friction coefficient (wave)
!>@param[in]     GRAV        Acceleration of gravity
!>@param[in]     HIDFAC      Hiding factor formulas
!>@param[in]     HIDI        Hiding factor for particular size class (hidfac =0)
!>@param[in]     HIDING      Hiding factor correction
!>@param[in]     HMIN        Minimum value of water depth
!>@param[in]     HN          Water depth
!>@param[in]     HOULE       Logical, for wave effects
!>@param[in]     ICF         Bed-load or total load transport formulas
!>@param[in]     IELMT       Number of elements
!>@param[in]     K_H_Y       Karim, holly & yang constant
!>@param[in]     KARMAN      Von karman constant
!>@param[in]     KENT        Convention for liquid input with prescribed value
!>@param[in]     KSP         Bed skin roughness
!>@param[in]     KSR         Ripple bed roughness
!>@param[in]     LIQBOR      Type of boundary condition for qs
!>@param[in]     MASKEL      Masking of elements
!>@param[in]     MASKPT      Masking per point
!>@param[in,out] MESH        Mesh structure
!>@param[in]     MSK         If yes, there is masked elements
!>@param[in,out] MU          Correction factor for bed roughness
!>@param[in]     NPOIN       Number of points
!>@param[in]     NPTFR       Number of boundary points
!>@param[in]     OPTBAN      Option for tidal flats
!>@param[in]     PHISED      Angle of repose of the sediment
!>@param[in]     PI          Pi
!>@param[in,out] QSC         Bed load transport rate [m2/s]->[kg*(m-1*s-1)]
!>@param[in,out] QSS         Suspended load transport rate
!>@param[in]     RATIO_SAND  Mass fraction of sand
!>@param[in]     S           Void structure
!>@param[in,out] SALFA       Sinus of the angle between transport rate and current
!>@param[in]     SANFRA      Sand fraction for Wilcock & Crowe formula
!>@param[in]     SECCURRENT  Logical, parametrisation for secondary currents
!>@param[in]     SLOPEFF     Formula for slope effect
!>@param[in]     SUSP        Logical, suspension
!>@param[in,out] T1          Work bief_obj structure
!>@param[in,out] T10         Work bief_obj structure
!>@param[in,out] T11         Work bief_obj structure
!>@param[in,out] T12         Work bief_obj structure
!>@param[in,out] T13         Work bief_obj structure
!>@param[in,out] T2          Work bief_obj structure
!>@param[in,out] T3          Work bief_obj structure
!>@param[in,out] T4          Work bief_obj structure
!>@param[in,out] T5          Work bief_obj structure
!>@param[in,out] T6          Work bief_obj structure
!>@param[in,out] T7          Work bief_obj structure
!>@param[in,out] T8          Work bief_obj structure
!>@param[in,out] T9          Work bief_obj structure
!>@param[in]     THETAW      Wave direction (deg wrt ox axis)
!>@param[in,out] TOB         Bed shear stress (total friction)
!>@param[in]     TOBW        Wave induced shear stress
!>@param[in]     TW          Wave period
!>@param[in,out] U2D         Mean flow velocity x-direction
!>@param[in]     UNLADM      Mean diameter of active stratum layer
!>@param[in,out] UNORM       Norm of the mean flow velocity
!>@param[in]     UNSV2D      Inverse of integrals of test functions
!>@param[in]     UW          Orbital wave velocity
!>@param[in,out] V2D         Mean flow velocity y-direction
!>@param[in]     VCE         Water viscosity
!>@param[in]     XMVE        Fluid density
!>@param[in]     XMVS        Water density
!>@param[in]     XWC         Settling velocity
!>@param[in]     ZERO        Zero
!>@param[in]     ZF          Elevation of bottom
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,
     &    EX_BEDLOAD_SOLIDISCHARGE => BEDLOAD_SOLIDISCHARGE_GAIA
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
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
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, DCLA, XWC, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: VCE, HMIN
      DOUBLE PRECISION, INTENT(IN)    :: HIDI
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO, PI
      DOUBLE PRECISION, INTENT(IN)    :: K_H_Y
      LOGICAL,          INTENT(IN)    :: SUSP, MSK,SECCURRENT,HOULE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T7,T8,T9,T10,T11,T12,T13
      DOUBLE PRECISION, INTENT(INOUT) :: AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC,QSS
!
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: PHISED,BETA,BETA2
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CALFA,SALFA,COEFPN,COEFCR
!
      DOUBLE PRECISION, INTENT(IN)    :: BIJK,RATIO_SAND(NPOIN)
!
      TYPE(BIEF_OBJ),    INTENT(IN)   :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: H_TEL
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HW, THETAC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBCW_MEAN, TOBCW_MAX
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CSTAEQ
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
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_EFFPNT_GAIA'
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
      IF(SLOPEFF.EQ.0) THEN
        CALL OS('X=C     ',X=COEFPN,C=1.D0)
        CALL OS('X=C     ',X=COEFCR,C=1.D0)
      ENDIF
!
      IF(SLOPEFF.NE.0.OR.DEVIA.NE.0) THEN
        CALL BEDLOAD_EFFPNT_GAIA
     &     (MASKEL,LIQBOR,S,ZF,NPOIN,NPTFR,IELMT,
     &      KENT,BETA,PI,MSK,MESH,T1,T2,T3,T4,
     &      COEFPN,COEFCR,CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2,
     &      TOB,XMVS,XMVE,DCLA,GRAV,UNSV2D)
      ENDIF
!
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_EFFPNT'
!
!     MASKING/EXPOSURE COEFFICIENT
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_HIDING_FACTOR_GAIA'
!
!     WITH HUNZIKER FORMULATION (6), THE HIDING FACTOR IS COMPUTED
!     WITH THE SOLID DISCHARGE (SEE BEDLOAD_HUNZ_MEYER_GAIA.F)
!
      IF(ICF.NE.6) THEN
        CALL BEDLOAD_HIDING_FACTOR_GAIA
     &     (ACLADM, HIDFAC, NPOIN, HIDI, DCLA, K_H_Y, HIDING)
      ENDIF
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_HIDING_FACTOR'
!
!     QSC COMPUTED USING EMPIRICAL FORMULATION : T1 = DQSC/DH                           !
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_FORMULA_GAIA'
!
      CALL BEDLOAD_FORMULA_GAIA
     &  (U2D,V2D, UNORM,HN, CF, MU,TOB, TOBW, UW, TW, THETAW, FW,
     &   ACLADM, UNLADM, KSP,KSR,RATIO_SAND, NPOIN, ICF, HIDFAC, XMVS,
     &   XMVE, DCLA, GRAV, VCE, HMIN, XWC, KARMAN, ZERO,
     &   PI, SUSP, AC, HIDING, T1, T2, T3, T4, T5, T6, T7, T8, T9,
     &   T10, T11, T12, T13, QSC, QSS, IELMT,SECCURRENT,
     &   SLOPEFF, COEFCR, CALFA, SALFA, BIJK, HOULE, H_TEL,
     &   HW,THETAC,TOBCW_MEAN,TOBCW_MAX,CSTAEQ,SANFRA)
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
