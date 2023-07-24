!                   *******************************
                    SUBROUTINE BEDLOAD_FORMULA_GAIA
!                   *******************************
!
     &(U2D,V2D,UNORM,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,
     & ACLADM, UNLADM,KSP,KSR,RATIO_SAND,NPOIN,ICF,HIDFAC,XMVS,XMVE,
     & DCLA,GRAV,VCE,HMIN,XWC,KARMAN,ZERO,
     & PI,SUSP, AC, HIDING, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10,
     & T11,TETAP, T13, QSC, QSS,IELMT,SECCURRENT,SLOPEFF,
     & COEFCR, CALFA,SALFA,BIJK,HOULE,H_TEL,
     & HW,THETAC,TOBCW_MEAN,TOBCW_MAX,CSTAEQ,SANFRA)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the bed-load transport.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     U2D        Longitudinal velocity (m/s)
!>@param[in]     V2D        Transversal velocity (m/s)
!>@param[in]     UNORM      Norm of the mean flow velocity (m/s)
!>@param[in]     HN         Water depth
!>@param[in]     CF         Quadratic friction coefficient
!>@param[in]     MU         Correction for skin friction
!>@param[in]     TOB        Total bed shear stress (n/m2)
!>@param[in]     TOBW       Wave induced bed shear stress (n/m2)
!>@param[in]     UW         Wave orbital velocity (m/s)
!>@param[in]     TW         Wave period (s)
!>@param[in]     THETAW     Wave direction (deg wrt ox axis)
!>@param[in]     FW         Quadratic friction coefficient (wave)
!>@param[in]     ACLADM     Mean diameter of active layer
!>@param[in]     UNLADM     Diameter of layer 2 (m)
!>@param[in]     KSP        Bed skin roughness (m)
!>@param[in]     KSR        Ripple bed roughness (m)
!>@param[in]     RATIO_SAND Mass fraction of sand
!>@param[in]     NPOIN      Number of points
!>@param[in]     ICF        Choice of formula
!>@param[in]     HIDFAC     Hiding factor formula
!>@param[in]     XMVS       Sediment density (kg/m3)
!>@param[in]     XMVE       Fluid density (kg/m3)
!>@param[in]     DCLA       Diameter of the class
!>@param[in]     GRAV       Gravity
!>@param[in]     VCE        Fluid kinematic viscosity (m2/s)
!>@param[in]     HMIN       Mininmum water depth
!>@param[in]     XWC        Settling velocity (m/s)
!>@param[in]     KARMAN     Von karman coefficient
!>@param[in]     ZERO       Zero
!>@param[in]     PI         Pi
!>@param[in]     SUSP       Suspension treatment
!>@param[in,out] AC         Shields parameter
!>@param[in,out] HIDING     Hiding factor correction
!>@param[in,out] T1         Working arrays
!>@param[in,out] T2         --
!>@param[in,out] T3         --
!>@param[in,out] T4         --
!>@param[in,out] T5         --
!>@param[in,out] T6         --
!>@param[in,out] T7         --
!>@param[in,out] T8         --
!>@param[in,out] T9         --
!>@param[in,out] T10        --
!>@param[in,out] T11        --
!>@param[in,out] T13        --
!>@param[in,out] TETAP      Wave direction (deg wrt ox axis)
!>@param[in,out] QSC        Bed load transport rate (kg*(m-1*s-1))
!>@param[in,out] QSS        Suspended load transport rate (m2/s)
!>@param[in]     IELMT      Type of element
!>@param[in]     SECCURRENT Effect of secundary currents
!>@param[in]     SLOPEFF    Formula for slope effect
!>@param[in,out] COEFPN     Correction of transort for sloping bed effect
!>@param[in,out] CALFA      Cosinus of the angle between transport rate and x-axis
!>@param[in,out] SALFA      Sinus of the angle between transport rate and current
!>@param[in]     BIJK       Empirical coefficient
!>@param[in]     HOULE      Effect of wave
!>@param[in]     H_TEL      Water depth from tel h (n+1)
!>@param[in]     HW         Significant wave height (from tomawac)
!>@param[in]     THETAC     Current direction (deg trigo)
!>@param[in]     TOBCW_MEAN Mean of total current + wave shear stress
!>@param[in]     TOBCW_MAX  Maximum of total current + wave shear stress
!>@param[in]     CSTAEQ     Sediment equilibrium concentration
!>@param[in]     SANFRA     Sand fraction (Wilcock & Crowe, 2003)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_FORMULA => BEDLOAD_FORMULA_GAIA
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY : SUSP_SAND
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D, V2D, UNORM,HN, CF, TOB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MU,TOBW, UW, TW, THETAW, FW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM,UNLADM,KSR,KSP
      INTEGER,          INTENT(IN)    :: NPOIN, ICF, HIDFAC,IELMT
      DOUBLE PRECISION, INTENT(IN)    :: XMVS, XMVE, DCLA, GRAV, VCE
      DOUBLE PRECISION, INTENT(IN)    :: HMIN, XWC
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO, PI
      LOGICAL,          INTENT(IN)    :: SUSP,SECCURRENT,HOULE
      DOUBLE PRECISION, INTENT(INOUT) :: AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1, T2, T3, T4, T5, T6, T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8, T9, T10,T11, T13
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETAP ! WORK ARRAY T12
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      TYPE(BIEF_OBJ),   INTENT(INOUT) ::  COEFCR, CALFA, SALFA
      INTEGER,          INTENT(IN)    :: SLOPEFF
!
      DOUBLE PRECISION, INTENT (IN)   :: BIJK,RATIO_SAND(NPOIN)
      TYPE(BIEF_OBJ),   INTENT(IN)    :: H_TEL
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HW, THETAC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBCW_MEAN, TOBCW_MAX
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CSTAEQ
      DOUBLE PRECISION, INTENT (IN) :: SANFRA(NPOIN)
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                     :: I
      DOUBLE PRECISION            :: DENS,DSTAR
      DOUBLE PRECISION, PARAMETER :: ZERO_LOCAL = 1.D-6
      DOUBLE PRECISION            :: C1
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!     ***************************
!     I - ADIMENSIONAL PARAMETERS
!     ***************************
!
!     RELATIVE DENSITY OF SEDIMENT
      DENS  = (XMVS - XMVE )/ XMVE
!     NON-DIMENSIONAL DIAMETER
      DSTAR = DCLA*(GRAV*DENS/VCE**2)**(1.D0/3.D0)
!
!     ************************
!     II -  SKIN FRICTION
!     ************************
!
      C1 = 1.D0/(DENS*XMVE*GRAV*DCLA)
      CALL OS('X=CYZ   ', X=TETAP, Y=TOB,Z=MU,  C=C1)
      CALL OS('X=+(Y,C)', X= TETAP,Y=TETAP, C=ZERO_LOCAL)
!
      IF(SECCURRENT) CALL BEDLOAD_SECCURRENT_GAIA(IELMT,CALFA,SALFA)
!     ******************************************
!     IV - COMPUTES 2 TRANSPORT TERMS
!          QSS : SUSPENSION
!          QSC : BEDLOAD
!     ******************************************
!     =====================================
!     IV(1) - MEYER-PETER-MULLER FORMULATION
!             FOR BEDLOAD ONLY
!     =====================================
      IF(ICF == 1) THEN
!
        CALL BEDLOAD_MEYER_GAIA(TETAP,HIDING,HIDFAC,DENS,GRAV,DCLA,AC,
     &                     T1,QSC,SLOPEFF,COEFCR,XMVS)
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)
        ENDDO
!
!     ===========================
!     IV(2) - EINSTEIN FORMULATION
!             FOR BEDLOAD ONLY
!     ===========================
      ELSEIF(ICF == 2) THEN
        CALL BEDLOAD_EINST_GAIA(TETAP,NPOIN,DENS,GRAV,DCLA,DSTAR,QSC,
     &                         XMVS)
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)*HIDING%R(I)
        ENDDO
!
!     ===================================
!     IV(30) - ENGELUND-HANSEN FORMULATION
!              FOR TOTAL TRANSPORT
!     ===================================
      ELSEIF(ICF == 30) THEN
!       V6P0 MU IS USED INSTEAD OF CF
!       BEWARE: DIFFERENCES
!               CALL BEDLOAD_ENGEL_GAIA(TETAP,DENS,GRAV,DCLA,QSC)
!       BACK TO EARLIER VERSION OF BEDLOAD_ENGEL_GAIA
        CALL BEDLOAD_ENGEL_GAIA(TOB,CF,DENS,GRAV,DCLA,XMVE,T1,QSC,XMVS)
!       ARBITRARY DISTRIBUTION
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)*HIDING%R(I)
        ENDDO
!
!     ========================================
!     IV(3) - ENGELUND-HANSEN FORMULATION
!             MODIFIED: CHOLLET ET CUNGE
!             FOR TOTAL TRANSPORT
!     ========================================
      ELSEIF(ICF == 3) THEN
!       KSP IS USED INSTEAD OF CFP
        CALL BEDLOAD_ENGEL_CC_GAIA
     &       (TETAP,CF,NPOIN,GRAV,DCLA,DENS,T1,QSC,XMVS)
!       ARBITRARY DISTRIBUTION
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)*HIDING%R(I)
        ENDDO
!
!     ==============================
!     IV(4) - BIJKER FORMULATION
!             FOR BEDLOAD + SUSPENSION
!     ==============================
      ELSEIF (ICF == 4) THEN
        CALL BEDLOAD_BIJKER_GAIA
     &   (TOBW,TOB,MU,KSP,KSR,HN,NPOIN,DCLA,DENS,XMVE,GRAV,
     &    XWC,KARMAN,ZERO,T4,T7,T8,T9,QSC,QSS,BIJK,HOULE,XMVS)
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)*HIDING%R(I)
          QSS%R(I)=QSS%R(I)*RATIO_SAND(I)*HIDING%R(I)
        ENDDO
!
!     ==============================
!     IV(5) - SOULSBY FORMULATION
!             FOR BEDLOAD + SUSPENSION
!     ==============================
      ELSEIF (ICF == 5) THEN
        CALL BEDLOAD_SOULSBY_GAIA
     &       (UNORM,HN,UW,NPOIN,DENS,GRAV,DCLA,DSTAR,
     &        QSC,QSS,XMVS,T1,COEFCR,SLOPEFF)
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)*HIDING%R(I)
          QSS%R(I)=QSS%R(I)*RATIO_SAND(I)*HIDING%R(I)
        ENDDO
!
!     ==================================================
!     IV(6) - HUNZIKER / MEYER-PETER & MULLER FORMULATION
!             FOR BEDLOAD ONLY
!     ==================================================
      ELSEIF (ICF == 6) THEN
        CALL BEDLOAD_HUNZ_MEYER_GAIA
     &       (TOB, MU, ACLADM, UNLADM, NPOIN, DENS, XMVE, GRAV,
     &        DCLA, AC, TETAP, T1, T2, HIDING, QSC, XMVS, SLOPEFF,
     &        COEFCR)
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)
        ENDDO
!
!     ===========================
!     IV(7) - VAN RIJN FORMULATION
!             FOR BEDLOAD ONLY
!     ===========================
      ELSEIF (ICF == 7) THEN
!
        CALL BEDLOAD_VANRIJN_GAIA
!     &       (TOB,MU,NPOIN,DCLA,DENS,GRAV,DSTAR,AC,QSC)
     &  (TETAP,NPOIN,DCLA,DENS,GRAV,DSTAR,AC,T1,QSC,XMVS,SLOPEFF,
     &   COEFCR)
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)*HIDING%R(I)
        ENDDO
!
!     ==============================
!     IV(8) - BAILARD FORMULATON
!             FOR BEDLOAD + SUSPENSION
!     ==============================
      ELSEIF (ICF == 8) THEN
!
        CALL BEDLOAD_BAILARD_GAIA
     &       (U2D,V2D,UNORM,TOB,TOBW,THETAW,UW,FW,CF,NPOIN,
     &        PI,XMVE,GRAV,DENS,XWC,T1,T2,T3,T4,T5,T6,T7,
     &        T8,T9,T10,T11,T13,QSC,QSS,HOULE,XMVS,THETAC)
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)*HIDING%R(I)
          IF(.NOT.SUSP) THEN
            QSS%R(I)=QSS%R(I)*RATIO_SAND(I)*HIDING%R(I)
          ELSE
!           RATIO_SAND IS TAKE IN ACCOUNT IN SUSPENSION_ERODE
!           AFTER COMPUTATION OF CAE
            QSS%R(I)=QSS%R(I)*HIDING%R(I)
          ENDIF
        ENDDO
!
!     =======================================
!     IV(9) - DIBAJNIA AND WATANABE FORMULATION
!             FOR TOTAL TRANSPORT
!     =======================================
      ELSEIF(ICF == 9) THEN
!
        CALL BEDLOAD_DIBWAT_GAIA
     &       (U2D,V2D,UNORM, CF, TOB, TOBW, UW, TW, FW, THETAW,
     &        NPOIN, XMVE, DENS, GRAV, DCLA, XWC, PI, T1, T2, T3, T4,
     &        T5, T6, T7, T8, T9, T10, T11, T13, QSC, XMVS, THETAC,
     &        COEFCR, SLOPEFF)
!       ARBITRARY DISTRIBUTION
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)*HIDING%R(I)
        ENDDO
!
!       =======================================
!       IV(10) - WILCOCK AND CROWE 2003 FORMULATION
!               NON-UNIFORM TRANSPORT
!       =======================================
      ELSEIF(ICF == 10) THEN
!
        CALL BEDLOAD_WILCOCK_CROWE_GAIA
     & (TOB, MU, ACLADM, DCLA, RATIO_SAND, GRAV, XMVE, XMVS, SANFRA,
     &  QSC, T1, SLOPEFF, COEFCR)
!
!     ============================================
!     IV(0) - USER-DEFINED FORMULATION
!     ============================================
      ELSEIF (ICF == 0) THEN
        CALL USER_BEDLOAD_QB
     &       (HN, U2D, V2D, THETAC, HOULE, HW, TW, THETAW,
     &        TOB,TOBW,TOBCW_MEAN,TOBCW_MAX, DCLA, DENS, GRAV, DSTAR,
     &        AC,XMVE, XMVS, TETAP, MU, NPOIN, QSC, QSS, CSTAEQ)
!
        DO I=1,NPOIN
          QSC%R(I)=QSC%R(I)*RATIO_SAND(I)*HIDING%R(I)
          QSS%R(I)=QSS%R(I)*RATIO_SAND(I)*HIDING%R(I)
        ENDDO
!     =================
!     IV(ELSE) - ERROR
!     =================
      ELSE
        WRITE(LU,201) ICF
201     FORMAT(1X,'TRANSP : TRANSPORT FORMULA UNKNOWN:',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     WHEN SUSPENSION IS NOT ASKED SPECIFICALLY, SOME BEDLOAD TRANSPORT
!     FORMULAS GIVE A VALUE
!
      IF(.NOT.SUSP_SAND) THEN
        IF(ICF.EQ.4.OR.ICF.EQ.5.OR.ICF.EQ.8.OR.ICF.EQ.0) THEN
          IF(.NOT.SUSP)THEN
!           BEDLOAD IS TOTAL LOAD IN THIS CASE
            DO I = 1,NPOIN
              QSC%R(I) = QSC%R(I) + QSS%R(I)
            ENDDO
          ELSE
      WRITE(LU,*)'WARNING, WITH COHESIVE SEDIMENT IN SUSPENSION'
      WRITE(LU,*)'AND ONLY BEDLOAD FOR SAND:'
      WRITE(LU,*)'IT IS ASSUMED THAT:'
      WRITE(LU,*)'1- BEDLOAD OF SAND IS NOT TOTAL BEDLOAD (QSS = 0).'
      WRITE(LU,*)'2- EROSION FLUX OF SAND USED TO COMPUTE EROSION FLUX'
      WRITE(LU,*)'   OF MIXED SEDIMENT IN SUSPENSION IS EQUAL TO 0.'
            DO I = 1,NPOIN
              QSS%R(I) = 0.D0
            ENDDO
          ENDIF
        ELSE
!         NOTE JMH: IS THIS REALLY USEFUL ???
          DO I = 1,NPOIN
            QSS%R(I) = 0.D0
          ENDDO
        ENDIF
      ENDIF
!     NO BEDLOAD IF H_TEL < HMIN  (NOT USE HN BECAUSE HN = HMIN IN THIS CASE)
      DO I = 1,NPOIN
        IF(H_TEL%R(I).LT.HMIN) THEN
          QSC%R(I) = 0.D0
          QSS%R(I) = 0.D0
        ENDIF
      ENDDO
!
!=======================================================================
!=======================================================================
!
      RETURN
      END
