!                 ****************************
                  SUBROUTINE BEDLOAD_MAIN_GAIA
!                 ****************************
!
     &(ACLADM,KSP,KSR, V2DPAR,UNSV2D,CF,EBOR,FW,HN,LIQBOR,
     & MASK, MASKEL, MASKPT, QBOR, U2D,
     & V2D, S,UNLADM,UW,THETAW,MU,TOB,TOBW,TW,ZF,
     & DEBUG, HIDFAC, ICF, IELMT, KDDL, KDIR,
     & KENT, KLOG, KNEU, KSORT,
     & NPOIN, NPTFR, NSICLA, OPTBAN, BETA, DCLA,
     & GRAV, HIDI, HMIN, VCE, XMVE, XMVS0, XWC,
     & PI, KARMAN, ZERO, KARIM_HOLLY_YANG,MSK, SUSP, VF,
     & MESH, LIEBOR, LIMTEC, MASKTR,
     & IT1, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11,
     & T12,T13,UNORM,AC, DT,
     & BREACH, CALFA_CL, COEFPN,COEFCR,
     & HIDING, QSCL_C, QSCL_S, QS_C,
     & QSCLXC, QSXC, QSCLYC, QSYC, SALFA_CL,
     & ENTETS, SECCURRENT, SLOPEFF,
     & PHISED, DEVIA, BETA2, BIJK,SEDCO,HOULE,
     & U3D,V3D,CODE,FLBCLA,MAXADV,RATIO_SAND,H_TEL,
     & HW, THETAC, TOBCW_MEAN, TOBCW_MAX, CSTAEQ)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Main subroutine for the bedload transport.
!!       Update of sand mass due to bedload transport.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     ACLADM      Mean diameter of active layer
!>@param[in]     KSP         Bed skin roughness
!>@param[in]     KSR         Ripple bed roughness
!>@param[in]     V2DPAR      Integral of test functions, assembled in parallel
!>@param[in]     UNSV2D      Inverse of integrals of test functions
!>@param[in]     CF          Quadratic friction coefficient
!>@param[in,out] EBOR        Imposed boundary condition for bed evolution (Dirichlet)
!>@param[in]     FW          Quadratic friction coefficient (wave)
!>@param[in]     HN          Water depth
!>@param[in]     LIQBOR      Type of boundary condition for qs
!>@param[in]     MASK        Block of masks, every one for a type of boundary
!!                           See diffin.f in library bief.
!>@param[in]     MASKEL      Masking of elements
!>@param[in]     MASKPT      Masking per point
!>@param[in]     QBOR        Boundary condition for transport rate
!>@param[in,out] U2D         Mean flow velocity x-direction
!>@param[in,out] V2D         Mean flow velocity y-direction
!>@param[in]     S           Void Structure
!>@param[in]     UNLADM      Mean diameter of active stratum layer
!>@param[in]     UW          Orbital wave velocity
!>@param[in]     THETAW      Wave direction (deg wrt ox axis)
!>@param[in,out] MU          Correction factor for bed roughness
!>@param[in,out] TOB         Bed shear stress (total friction)
!>@param[in]     TOBW        Wave induced shear stress
!>@param[in]     TW          Wave period
!>@param[in]     ZF          Elevation of bottom
!>@param[in]     DEBUG       Flag for debugging
!>@param[in]     HIDFAC      Hiding factor formulas
!>@param[in]     ICF         Bed-load or total load transport formulas
!>@param[in]     IELMT       Type of element
!>@param[in]     KDDL        Convention for degree of freedom
!>@param[in]     KDIR        Convention for dirichlet point
!>@param[in]     KENT        Convention for liquid input with prescribed value
!>@param[in]     KLOG        Convention for solid boundary
!>@param[in]     KNEU        Convention for neumann condition
!>@param[in]     KSORT       Convention for free output
!>@param[in]     NPOIN       Number of points
!>@param[in]     NPTFR       Number of boundary points
!>@param[in]     NSICLA      Number of size classes for bed materials
!>@param[in]     OPTBAN      Option for tidal flats
!>@param[in]     BETA        Coefficient for sloping bed effect (Koch and Flokstra)
!>@param[in]     DCLA        Diameter dm for each class
!>@param[in]     GRAV        Acceleration of gravity
!>@param[in]     HIDI        Hiding factor for particular size class (hidfac =0)
!>@param[in]     HMIN        Minimum value of water depth
!>@param[in]     VCE         Water viscosity
!>@param[in]     XMVE        Fluid density
!>@param[in]     XMVS0       Sediment density
!>@param[in]     XWC         Settling velocity
!>@param[in]     PI          Pi
!>@param[in]     KARMAN      Von karman constant
!>@param[in]     ZERO        Zero
!>@param[in]     KARIM_HOLLY_YANG TODO
!>@param[in]     MSK         If yes, there is masked elements
!>@param[in]     SUSP        Logical, suspension
!>@param[in]     VF          Logical, finite volumes or not
!>@param[in,out] MESH        Mesh structure
!>@param[in,out] LIEBOR      Type of boundary conditions for bed evolution
!>@param[in,out] LIMTEC      Technical boundary condition (neuman...)
!>@param[in,out] MASKTR      Masking for tracers, per point
!>@param[in,out] IT1         Integer work array in a bief_obj structure
!>@param[in,out] T1          Work bief_obj structure
!>@param[in,out] T2          Work bief_obj structure
!>@param[in,out] T3          Work bief_obj structure
!>@param[in,out] T4          Work bief_obj structure
!>@param[in,out] T5          Work bief_obj structure
!>@param[in,out] T6          Work bief_obj structure
!>@param[in,out] T7          Work bief_obj structure
!>@param[in,out] T8          Work bief_obj structure
!>@param[in,out] T9          Work bief_obj structure
!>@param[in,out] T10         Work bief_obj structure
!>@param[in,out] T11         Work bief_obj structure
!>@param[in,out] T12         Work bief_obj structure
!>@param[in,out] T13         Work bief_obj structure
!>@param[in,out] UNORM       Norm of the mean flow velocity
!>@param[in,out] AC          Critical shields parameter
!>@param[in,out] DT          Time step
!>@param[in,out] BREACH      Indicator for non erodible bed (finite volumes shemes)
!>@param[in,out] CALFA_CL    Cosinus of the angle between mean flow and transport
!>@param[in,out] COEFPN      Correction of transport for sloping bed effect
!>@param[in,out] COEFCR      Correction of critical Shields for sloping bed effect
!>@param[in]     HIDING      Hiding factor correction
!>@param[in,out] QSCL_C      Bedload transport rate
!>@param[in,out] QSCL_S      Suspended load transport rate
!>@param[in,out] QS_C        Bedload transport rate
!>@param[in,out] QSCLXC      Transport rate for each class x-direction
!>@param[in,out] QSXC        Bedload transport rate x-direction
!>@param[in,out] QSCLYC      Transport rate for each class y-direction
!>@param[in,out] QSYC        Bedload transport rate y-direction
!>@param[in,out] ENTETS      Logical, if yes information is given on mass conservation for suspension
!>@param[in]     SECCURRENT  Logical, parametrisation for secondary currents
!>@param[in]     SLOPEFF     Formula for slope effect
!>@param[in]     PHISED      Angle of repose of the sediment
!>@param[in]     DEVIA       Slope effect formula for deviation
!>@param[in]     BETA2       Coefficient for the deviation  (talmon et al.)
!>@param[in]     BIJK        Coefficient of the bijker formula
!>@param[in]     SEDCO       Logical, sediment cohesive or not
!>@param[in]     HOULE       Logical, for wave effects
!>@param[in]     U3D         Three-dimensional velocity x-direction
!>@param[in]     V3D         Three-dimensional velocity y-direction
!>@param[in]     CODE        Hydrodynamic code in case of coupling
!>@param[in]     FLBCLA      Block of fluxes at boundary for each class
!>@param[in]     MAXADV      TODO
!>@param[in,out] RATIO_SAND  Mass fraction of sand
!>@param[in]     H_TEL       Water depth from tel h (n+1)
!>@param[in]     HW          Significant wave height (from tomawac)
!>@param[in]     THETAC      Current direction (deg trigo)
!>@param[in]     TOBCW_MEAN  Mean of total current + wave shear stress
!>@param[in]     TOBCW_MAX   Maximum of total current + wave shear stress
!>@param[in,out] CSTAEQ      Equilibrium sediment concentration
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_GAIA, EX_BEDLOAD_MAIN => BEDLOAD_MAIN_GAIA
      USE DECLARATIONS_GAIA, ONLY : NOMBLAY,MASS_SAND,NSAND,
     &              NUM_ICLA_ISAND,EVCL_MB,EVOL_MB,MIN_SED_MASS_COMP,
     &              MASS_SAND_TOT,RATIO_MUD_SAND,
     &              NUM_ICLA_IMUD,MASS_MUD,MASS_SAND_ACTIVE_LAYER,
     &              MASS_SAND_MASKED, EVCL_M_TOT_SAND,
     &              RATIO_EVOL_TOT_SAND,MOFAC_BED,MUDB,F_MUDB,
     &              NESTOR,XKV0,VOLU2D,MASSNESTOR,
     &              NMUD,NUM_ISAND_ICLA,NUM_IMUD_ICLA,
     &              SANFRA
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY: P_SUM
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, KSR,V2DPAR,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CF,FW,KSP,HN,LIQBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASK, MASKEL, MASKPT
      TYPE(BIEF_OBJ),   INTENT(IN)    :: QBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: U2D,V2D,TOB,MU,UNORM,EBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,UNLADM
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UW, THETAW,  TOBW, TW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HW, THETAC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TOBCW_MEAN, TOBCW_MAX
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF
      INTEGER,          INTENT(IN)    :: DEBUG, HIDFAC, ICF,MAXADV
      INTEGER,          INTENT(IN)    :: IELMT, KDDL, KDIR, KENT
      INTEGER,          INTENT(IN)    :: KLOG, KNEU, KSORT
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR
      INTEGER,          INTENT(IN)    :: NSICLA, OPTBAN
      DOUBLE PRECISION, INTENT(IN)    :: BETA
      DOUBLE PRECISION, INTENT(IN)    :: DCLA(NSICLA),GRAV
      DOUBLE PRECISION, INTENT(IN)    :: HIDI(NSICLA),HMIN,VCE
      DOUBLE PRECISION, INTENT(IN)    :: XMVE,XMVS0(NSICLA),XWC(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: PI,KARMAN,ZERO
      DOUBLE PRECISION, INTENT(IN)    :: KARIM_HOLLY_YANG
      LOGICAL,          INTENT(IN)    :: MSK, SUSP, VF
      LOGICAL,          INTENT(IN)    :: SECCURRENT
      LOGICAL,          INTENT(IN)    :: SEDCO(NSICLA),HOULE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLBCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: LIEBOR, LIMTEC, MASKTR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: IT1,T1,T2,T3,T4,T5,T6,T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8,T9,T10,T11,T12,T13
      DOUBLE PRECISION, INTENT(INOUT) :: AC(NSICLA), DT
      DOUBLE PRECISION, INTENT(INOUT) :: RATIO_SAND(NSAND,NOMBLAY,NPOIN)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, CALFA_CL, COEFPN
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING, COEFCR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCL_C,QSCL_S
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS_C, QSCLXC, QSXC, QSCLYC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSYC, SALFA_CL
      LOGICAL,          INTENT(INOUT) :: ENTETS
      DOUBLE PRECISION,   INTENT(IN)  :: BETA2, PHISED
      INTEGER, INTENT (IN)            :: SLOPEFF, DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: BIJK
      TYPE(BIEF_OBJ),    INTENT(IN)   :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: H_TEL
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,IPOIN,ICLA,IMUD,ISAND
      DOUBLE PRECISION, ALLOCATABLE :: TMP_RATIO(:)
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!
!
!     Calculation of sand fraction content at each node (Wilcock and Crowe, 2003)
      IF (ICF == 10) THEN
        DO K = 1, NPOIN
          SANFRA(K) = 0.0D0
          DO I = 1, NSAND
            IF (DCLA(I).LT.2.D-3) THEN
              SANFRA(K) = SANFRA(K) + RATIO_SAND(I,1,K)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!

!     INITIALISES TECHNICAL BOUNDARY CONDITIONS
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_DIFFIN_GAIA'
      CALL BEDLOAD_DIFFIN_GAIA
     &        (U2D, V2D, MESH%NBOR, MESH%XNEBOR, MESH%YNEBOR,
     &         MASKEL, MESH%NELBOR, NPTFR, KENT, KSORT, KLOG,
     &         KDIR, KDDL, KNEU, MSK, IT1, LIEBOR, MASKTR,LIMTEC,
     &         MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX)
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_DIFFIN'
!
      ! Memory optimisation (from intel debug)
      ALLOCATE(TMP_RATIO(NPOIN))
      DO I = 1, NSICLA
        K=NUM_ICLA_ISAND(I)
!
!       FOR SAND
        IF(.NOT.SEDCO(I)) THEN
          IF (DEBUG > 0) WRITE(LU,*)
     &      'BEDLOAD_SOLIDISCHARGE_GAIA : ',I,'/',NSICLA
          TMP_RATIO = RATIO_SAND(K,1,1:NPOIN)
          CALL BEDLOAD_SOLIDISCHARGE_GAIA
     &       (MESH, U2D, V2D, UNORM,HN, TW, UW, MU,TOB,CF,
     &         TOBW,FW,THETAW,
     &         TMP_RATIO,
     &         MASKPT, MASKEL, ACLADM,
     &         UNLADM,KSP,KSR, LIQBOR, DEBUG, NPOIN,
     &         NPTFR, IELMT, ICF, KENT, OPTBAN, HIDFAC, GRAV,
     &         DCLA(I), XWC(I), XMVE, XMVS0(I), VCE, HMIN,
     &         HIDI(I),KARMAN,ZERO,PI,
     &         KARIM_HOLLY_YANG,SUSP,MSK,T1,T2,
     &         T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, AC(I),
     &         HIDING,QSCL_C%ADR(I)%P,QSCL_S%ADR(I)%P,
     &         SLOPEFF,COEFPN,COEFCR,PHISED,
     &         CALFA_CL%ADR(I)%P,SALFA_CL%ADR(I)%P,
     &         BETA,ZF,S,
     &         DEVIA, BETA2 , SECCURRENT, BIJK,HOULE,UNSV2D,
     &         U3D,V3D,CODE, H_TEL,
     &         HW,THETAC,TOBCW_MEAN,TOBCW_MAX,CSTAEQ%ADR(I)%P,
     &         SANFRA)
          IF(DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_SOLIDISCHARGE'
!
        ELSE
!         FOR COHESIVE SEDIMENT: ZERO BEDLOAD TRANSPORT RATE
          CALL OS('X=0     ',X=QSCL_C%ADR(I)%P)
          CALL OS('X=0     ',X=QSCLXC%ADR(I)%P)
          CALL OS('X=0     ',X=QSCLYC%ADR(I)%P)
        ENDIF
!
!       MORPHOLOGICAL FACTOR on evolution bed is applicated
!       on QSC directly to take in account rigid bed
!       After evolution of bed, QSC take the good value
        DO IPOIN=1,NPOIN
          QSCL_C%ADR(I)%P%R(IPOIN)=
     &    QSCL_C%ADR(I)%P%R(IPOIN)*MOFAC_BED
        ENDDO
!
      ENDDO
!
!
      DO IPOIN=1,NPOIN
        EVCL_M_TOT_SAND(IPOIN)= 0.D0
      ENDDO
!
!     COMPUTES THE EVOLUTION FOR EACH CLASS
!
      DO I = 1, NSICLA
!
        K=NUM_ICLA_ISAND(I)
!
        IF(.NOT.SEDCO(I)) THEN
!
          DO IPOIN=1,NPOIN
!           IF MORE THAN 30% OF MUD, BEDLOAD IS NOT ACTIVE
            IF(RATIO_MUD_SAND(1,IPOIN).LE.0.3D0)THEN
              MASS_SAND_ACTIVE_LAYER(IPOIN)= MASS_SAND(K,1,IPOIN)
              MASS_SAND_MASKED(IPOIN)=0.D0
            ELSE
              MASS_SAND_ACTIVE_LAYER(IPOIN)= 0.D0
              MASS_SAND_MASKED(IPOIN)= MASS_SAND(K,1,IPOIN)
            ENDIF
          ENDDO
!
          IF (DEBUG>0) WRITE(LU,*) 'BEDLOAD_EVOL_GAIA : ',I,'/',NSICLA
          TMP_RATIO = RATIO_SAND(K,1,1:NPOIN)
          CALL BEDLOAD_EVOL_GAIA(S,COEFPN,CALFA_CL%ADR(I)%P,
     &                      SALFA_CL%ADR(I)%P, LIMTEC,
     &                      EBOR%ADR(K)%P,MASKEL,MASK,
     &                      V2DPAR,UNSV2D,DEBUG,NPOIN,NPTFR,IELMT,
     &                      KENT,KDIR,KDDL,DT,XMVS0(I),
     &                      VF,ENTETS,MSK,
     &                      MESH,QSCL_C%ADR(I)%P,
     &                      T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,
     &                      T13,BREACH,QSCLXC%ADR(I)%P,
     &                      QSCLYC%ADR(I)%P,SLOPEFF,
     &                      I,FLBCLA,LIQBOR,QBOR%ADR(I)%P,MAXADV,
     &                      MASS_SAND_ACTIVE_LAYER,
     &                      TMP_RATIO,EVCL_MB%ADR(I)%P)
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_BEDLOAD_EVOL'
!
          DO IPOIN=1,NPOIN
            MASS_SAND(K,1,IPOIN)= MASS_SAND_ACTIVE_LAYER(IPOIN)
     &                           + MASS_SAND_MASKED(IPOIN)
          ENDDO
!
          DO IPOIN=1,NPOIN
            EVCL_M_TOT_SAND(IPOIN)= EVCL_M_TOT_SAND(IPOIN)+
     &                            EVCL_MB%ADR(I)%P%R(IPOIN)
          ENDDO
!
        ENDIF
      ENDDO
      DEALLOCATE(TMP_RATIO)
!
!============  run Nestor ==============================================!
!
      IF(NESTOR) THEN
        IF(NSAND.GT.0) THEN
          MASSNESTOR = 0.D0
          DO IPOIN=1,NPOIN
            DO ISAND=1,NSAND
              K = NUM_ISAND_ICLA(ISAND)
              MASSNESTOR(K) = MASSNESTOR(K) +
     &         EVCL_MB%ADR(K)%P%R(IPOIN) *VOLU2D%R(IPOIN)
            ENDDO
          ENDDO
        ENDIF
        IF(NMUD.GT.0) THEN
          DO IPOIN=1,NPOIN
            DO IMUD=1,NMUD
              K = NUM_IMUD_ICLA(IMUD)
              MASSNESTOR(K) = MASSNESTOR(K) +
     &         EVCL_MB%ADR(K)%P%R(IPOIN) *VOLU2D%R(IPOIN)
            ENDDO
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          DO ICLA=1,NSICLA
            MASSNESTOR(ICLA) = P_SUM(MASSNESTOR(ICLA))
          ENDDO
        ENDIF
!
        CALL NESTOR_INTERFACE_GAIA(2,123,XMVS0,XKV0(1),VOLU2D)
!
        IF(NSAND.GT.0) THEN
          T8%R = 0.D0
          DO IPOIN=1,NPOIN
            DO ISAND=1,NSAND
              K = NUM_ISAND_ICLA(ISAND)
              T8%R(K) = T8%R(K) +
     &         EVCL_MB%ADR(K)%P%R(IPOIN) *VOLU2D%R(IPOIN)
            ENDDO
          ENDDO
        ENDIF
        IF(NMUD.GT.0) THEN
          DO IPOIN=1,NPOIN
            DO IMUD=1,NMUD
              K = NUM_IMUD_ICLA(IMUD)
              T8%R(K) = T8%R(K) +
     &         EVCL_MB%ADR(K)%P%R(IPOIN) *VOLU2D%R(IPOIN)
            ENDDO
          ENDDO
        ENDIF
        IF(NCSIZE.GT.0) THEN
          DO ICLA = 1,NSICLA
            T8%R(ICLA) = P_SUM(T8%R(ICLA))
            MASSNESTOR(ICLA) = T8%R(ICLA) - MASSNESTOR(ICLA)
          END DO
        ENDIF
      ENDIF
!
!======================================================================!
!
!     EVOLUTIONS AND QS FOR EACH CLASS ARE ADDED
!
!     INITIALISES
!
      CALL OS('X=0     ',X=QS_C)
      CALL OS('X=0     ',X=QSXC)
      CALL OS('X=0     ',X=QSYC)
      CALL OS('X=0     ',X=MUDB)
      CALL OS('X=0     ',X=F_MUDB)
!     FIXME: COMPUTE EVOL_MB IS NOT REALLY USEFULL BECAUSE MASS BALANCE
!     IS THEN COMPUTED FOR EACH CLASS AND THE SUM IS LOCALLY COMPUTED
      CALL OS('X=0     ',X=EVOL_MB)
!
!
      DO I=1,NSICLA
!       After evolution of bed, QSC take the good value
!       see before for multiplcation by MOFAC_BED
        DO IPOIN=1,NPOIN
          QSCL_C%ADR(I)%P%R(IPOIN)=
     &    QSCL_C%ADR(I)%P%R(IPOIN)/MOFAC_BED
        ENDDO
!
        K=NUM_ICLA_ISAND(I)
        IF(.NOT.SEDCO(I)) THEN
          CALL OS('X=X+Y   ', X=QS_C, Y=QSCL_C%ADR(I)%P)
          CALL OS('X=X+YZ  ', X=QSXC, Y=QSCL_C%ADR(I)%P,
     &                              Z=CALFA_CL%ADR(I)%P)
          CALL OS('X=X+YZ  ', X=QSYC, Y=QSCL_C%ADR(I)%P,
     &                              Z=SALFA_CL%ADR(I)%P)
          CALL OS('X=X+Y   ', X=EVOL_MB, Y=EVCL_MB%ADR(I)%P)
        ENDIF
      ENDDO
!
!     UPDATE OF SAND MASS WITH EVOLUTION DUE TO BEDLOAD
!     (OF THE FIRST LAYER)
!
      DO I=1,NSICLA
        K=NUM_ICLA_ISAND(I)
        IF(.NOT.SEDCO(I)) THEN
          DO IPOIN=1,NPOIN
            MASS_SAND(K,1,IPOIN) = MASS_SAND(K,1,IPOIN) +
     &                         EVCL_MB%ADR(I)%P%R(IPOIN)
          ENDDO
        ENDIF
      ENDDO
!
!     POURCENTAGE OF MASS SAND IN THE ACTIVE LAYER THAT HAVE MOVED
      DO IPOIN=1,NPOIN
!       MASS_SAND_TOT IS COMING FROM BED UPDATE ACTIVE HIRANO BEFORE BEDLOAD
        IF(MASS_SAND_TOT(1,IPOIN).GE.MIN_SED_MASS_COMP)THEN
          RATIO_EVOL_TOT_SAND(IPOIN)= EVCL_M_TOT_SAND(IPOIN)/
     &    MASS_SAND_TOT(1,IPOIN)
          RATIO_EVOL_TOT_SAND(IPOIN)=
     &                    MIN(RATIO_EVOL_TOT_SAND(IPOIN),1.D0)
          RATIO_EVOL_TOT_SAND(IPOIN)=
     &                    MAX(RATIO_EVOL_TOT_SAND(IPOIN),-1.D0)
        ELSE
          RATIO_EVOL_TOT_SAND(IPOIN)= 0.D0
        ENDIF
      ENDDO
!     THE SAME RATIO OF MUD MOVED AND IS ADDED IN SUSPENSION FLUX
      DO I = 1, NSICLA
        K=NUM_ICLA_IMUD(I)
        IF(SEDCO(I)) THEN
          DO IPOIN =1,NPOIN
            IF(RATIO_EVOL_TOT_SAND(IPOIN).LE.0.D0)THEN
              MUDB%ADR(I)%P%R(IPOIN)= RATIO_EVOL_TOT_SAND(IPOIN)*
     &                                MASS_MUD(K,1,IPOIN)
            ELSE
              MUDB%ADR(I)%P%R(IPOIN) = 0.D0
            ENDIF
            MASS_MUD(K,1,IPOIN)= MASS_MUD(K,1,IPOIN)+
     &                           MUDB%ADR(I)%P%R(IPOIN)
            F_MUDB%ADR(I)%P%R(IPOIN)=
     &                         - MUDB%ADR(I)%P%R(IPOIN)/DT
!           F_MUDB IS ADDED FOR SUPENSION FLUX AFTER
!           IN BED1_SUSPENSION_ERODE
          ENDDO
        ENDIF
      ENDDO
!
!     TIDAL FLATS WITH MASKING
!
      IF(OPTBAN.EQ.2) CALL OS('X=XY    ',X=EVOL_MB,Y=MASKPT)
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
