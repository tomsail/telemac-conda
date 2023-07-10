!                   **********************************
                    MODULE INTERFACE_GAIA_BEDLOAD !
!                   **********************************
!
!
!***********************************************************************
! GAIA   V7P2                                   03/06/2014
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTERFACE
!         INTERFACE FOR THE GAIA SUBROUTINE   !
!         FOR BEDLOAD TRANSPORT           !
      !================================================================!
      SUBROUTINE BEDLOAD_BAILARD_GAIA !
      !----------------------------------------------------------------!
     &(U2D,V2D,UCMOY,TOB,TOBW,THETAW,UW,FW,CF,NPOIN,PI,
     & XMVE,GRAV,DENS,XWC,ALPHAW,QSCX,QSCY,QSSX,QSSY,
     & UC3X,UC3Y,US4X,US4Y,T10,FCW,T13,QSC,QSS,HOULE,XMVS,THETAC)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,UCMOY, TOB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, THETAW, UW, FW, CF
      TYPE(BIEF_OBJ),   INTENT(IN)    :: THETAC
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      DOUBLE PRECISION, INTENT(IN)    :: PI, XMVE, GRAV, DENS, XWC,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ALPHAW
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCX, QSCY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSSX, QSSY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UC3X, UC3Y
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: US4X, US4Y
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: FCW, T10, T13
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC, QSS
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_BAILARD_GAIA
!
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_BIJKER_GAIA
      !----------------------------------------------------------------!
     &  (TOBW,TOB,MU,KSP,KSR,HN,NPOIN,DCLA,DENS,XMVE,GRAV,XWC,
     &   KARMAN,ZERO,T4,T7,T8,T9,QSC,QSS,BIJK,HOULE,XMVS)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, TOB, KSR,KSP, HN,MU
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      DOUBLE PRECISION, INTENT(IN)    :: DCLA, DENS, XMVE, GRAV, XWC
      DOUBLE PRECISION, INTENT(IN)    :: XMVS
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO
      DOUBLE PRECISION, INTENT(IN)    :: BIJK
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T4
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T7, T8, T9
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC, QSS
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_BIJKER_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_CALCDW_GAIA
      !----------------------------------------------------------------!
     &  (UCW, UW, TW, NPOIN, PI, UW1, UW2, TW1, TW2)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)  :: UCW, UW, TW
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: PI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UW1, UW2, TW1, TW2
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_CALCDW_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_DIBWAT_GAIA !
      !----------------------------------------------------------------!
!
     &(U2D,V2D,UNORM, CF, TOB, TOBW, UW, TW, FW, THETAW, NPOIN,
     & XMVE, DENS, GRAV, DCLA, XWC, PI, ALPHAW, T2, T3, UCW, UCN,
     &     UW1, UW2, TW1, TW2, T10, FCW, T13, QSC, XMVS, THETAC,
     &     COEFCR,SLOPEFF)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_GAIA, ONLY : T14
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,UNORM, CF, TOB, TOBW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UW, TW, FW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: THETAW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: THETAC,COEFCR
      INTEGER,          INTENT(IN)    :: NPOIN,SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, DENS, GRAV, DCLA, XWC, PI
      DOUBLE PRECISION, INTENT(IN)    :: XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ALPHAW
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2, T3
      TYPE(BIEF_OBJ),   INTENT(INOUT) ::  UCW, UCN
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UW1, UW2, TW1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TW2, T10, T13
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FCW, QSC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_DIBWAT_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_DIFFIN_GAIA
!     *************************
!
     &(U, V, NBOR, XNEBOR, YNEBOR, MASKEL, NELBOR, NPTFR,
     & KENT, KSORT, KLOG, KDIR, KDDL, KNEU, MSK, CLT, LITBOR,
     & MASKTR, LIMTRA,IKLBOR,NELEB,NELEBX)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ), INTENT(IN)    :: U,V,NBOR,XNEBOR,YNEBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,NELBOR
      INTEGER,        INTENT(IN)    :: NPTFR,KENT,KSORT,KLOG
      INTEGER,        INTENT(IN)    :: KDIR,KDDL,KNEU,NELEB,NELEBX
      INTEGER,        INTENT(IN)    :: IKLBOR(NELEBX,2)
      LOGICAL,        INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CLT
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LITBOR, MASKTR, LIMTRA
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_DIFFIN_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_DIRECTION_GAIA
      !----------------------------------------------------------------!
     &  (U2D, V2D, NPOIN, PI, THETAC)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)  :: U2D, V2D
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: PI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: THETAC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_DIRECTION_GAIA
      !================================================================!
!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_EFFPNT_GAIA !
      !----------------------------------------------------------------!
!
     &(MASKEL,LIQBOR,S,ZF,NPOIN,NPTFR,IELMT,
     & KENT,BETA,PI,MSK,MESH,DZFDX,DZFDY,CTETA,STETA,
     & COEF,COEFCR, CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2,
     & TOB,XMVS,XMVE,DCLA,GRAV,UNSV2D)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,LIQBOR,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF, TOB
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR, IELMT, KENT
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: BETA, PI, PHISED, BETA2
      DOUBLE PRECISION, INTENT(IN)    :: XMVS, XMVE, GRAV, DCLA
      LOGICAL,          INTENT(IN)    :: MSK
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DZFDX, DZFDY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CTETA,STETA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: COEF, COEFCR, CALFA, SALFA
!
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_EFFPNT_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_EINST_GAIA
      !----------------------------------------------------------------!
     &(TETAP, NPOIN, DENS, GRAV, DCLA, DSTAR, QSC,XMVS)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DCLA, DSTAR, XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_EINST_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_ENGEL_GAIA
      !----------------------------------------------------------------!
     &(TOB,CF,DENS,GRAV,DCLA,XMVE,TETA,QSC,XMVS)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, CF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DCLA, XMVE, XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETA ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_ENGEL_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_ENGEL_CC_GAIA
      !----------------------------------------------------------------!
     &(TETAP,CF,NPOIN,GRAV,DCLA,DENS,TETA,QSC,XMVS)
      !----------------------------------------------------------------!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP,CF
      INTEGER, INTENT(IN)             :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, DCLA, DENS,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETA! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_ENGEL_CC_GAIA
      !================================================================!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_EVOL_GAIA
      !----------------------------------------------------------------!
!
     &(S,COEFPN,CALFA,SALFA,LIMTEC,EBOR,MASKEL,MASK,V2DPAR,
     & UNSV2D,DEBUG,NPOIN,NPTFR,IELMT,KENT,KDIR,KDDL,
     & DT,XMVS,VF,ENTETS,MSK,MESH,QS,
     & T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,BREACH,QSX,QSY,
     & SLOPEFF,ICLA,FLBCLA,LIQBOR,QBOR,MAXADV,MASS_SAND,
     & RATIO_SAND,EVCL_MB)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: COEFPN,CALFA,SALFA
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,MASK,V2DPAR
      INTEGER,          INTENT(IN)    :: DEBUG,SLOPEFF,NPOIN,NPTFR,ICLA
      INTEGER,          INTENT(IN)    :: IELMT,KENT,KDIR,KDDL
      INTEGER,          INTENT(IN)    :: MAXADV
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(IN)    :: XMVS
      DOUBLE PRECISION, INTENT(IN)    :: RATIO_SAND(NPOIN)
      LOGICAL,          INTENT(IN)    :: VF,ENTETS,MSK
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS,EBOR,FLBCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1, T2, T3, T4, T5, T6, T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8, T9, T10, T11, T12, T13
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, QSX, QSY, LIMTEC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: LIQBOR,QBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: EVCL_MB
      DOUBLE PRECISION, INTENT(IN)    :: MASS_SAND(NPOIN)
!
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_EVOL_GAIA
      !================================================================!
!
!
      !============================!
        SUBROUTINE BEDLOAD_FORMULA_GAIA !
      !----------------------------------------------------------------!
!
     &(U2D,V2D,UNORM,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,
     & ACLADM, UNLADM,KSP,KSR,RATIO_SAND,NPOIN,ICF,HIDFAC,XMVS,XMVE,
     & DCLA,GRAV,VCE,HMIN,XWC,KARMAN,ZERO,
     & PI,SUSP, AC, HIDING, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10,
     & T11,TETAP, T13, QSC, QSS,IELMT,SECCURRENT,SLOPEFF,
     & COEFCR,CALFA,SALFA,BIJK,HOULE,H_TEL,
     & HW,THETAC,TOBCW_MEAN,TOBCW_MAX,CSTAEQ,SANFRA)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
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
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8, T9, T10, T11, T13
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
      DOUBLE PRECISION, INTENT(IN)    :: SANFRA(NPOIN)
!
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_FORMULA_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_HIDING_FACTOR_GAIA
      !----------------------------------------------------------------!
     &(ACLADM, HIDFAC, NPOIN, HIDI, DCLA, K_H_Y, HIDING)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM
      INTEGER,          INTENT(IN)    :: HIDFAC, NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: HIDI, DCLA, K_H_Y
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_HIDING_FACTOR_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_HUNZ_MEYER_GAIA
      !----------------------------------------------------------------!
     &  (TOB, MU, ACLADM, UNLADM, NPOIN, DENS, XMVE, GRAV, DCLA, AC,
     &    TETAP, AHUNZI, ACP, HIDING, QSC, XMVS, SLOPEFF, COEFCR)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, MU, ACLADM, UNLADM, TETAP
      TYPE(BIEF_OBJ),   INTENT(IN)    :: COEFCR      
      INTEGER,          INTENT(IN)    :: NPOIN, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, XMVE, GRAV, DCLA, AC,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: AHUNZI, ACP ! WORK ARRAY T1, T2
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING, QSC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_HUNZ_MEYER_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_INTERACT_GAIA
      !----------------------------------------------------------------!
     &  (UCMOY, TOBW, TOB, ALPHAW, FW, CF, UW, NPOIN, XMVE, FCW)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)  :: UCMOY, TOBW, TOB, ALPHAW
      TYPE(BIEF_OBJ),   INTENT(IN)  :: FW, CF, UW
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: XMVE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FCW
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_INTERACT_GAIA
      !================================================================!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_MAIN_GAIA !
      !----------------------------------------------------------------!
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
     & BREACH, CALFA_CL, COEFPN, COEFCR,
     & HIDING, QSCL_C, QSCL_S, QS_C,
     & QSCLXC, QSXC, QSCLYC, QSYC, SALFA_CL,
     & ENTETS, SECCURRENT, SLOPEFF,
     & PHISED, DEVIA, BETA2, BIJK,SEDCO,HOULE,
     & U3D,V3D,CODE,FLBCLA,MAXADV,RATIO_SAND,H_TEL,
     & HW, THETAC, TOBCW_MEAN, TOBCW_MAX, CSTAEQ)
!
      USE BIEF_DEF
      USE DECLARATIONS_GAIA, ONLY : NOMBLAY,NSAND
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
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
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING,COEFCR
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
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_MAIN_GAIA
      !================================================================!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_MEYER_GAIA !
      !----------------------------------------------------------------!
     &(TETAP,HIDING,HIDFAC,DENS,GRAV,DCLA,AC,ACP,QSC,SLOPEFF,COEFCR,
     & XMVS)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, HIDING, COEFCR
      INTEGER,          INTENT(IN)    :: HIDFAC, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DCLA, AC, XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_MEYER_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_NERBED_VF_GAIA !
     &(MESH,LIEBOR,KSORT,V2DPAR,QSX,QSY,NPOIN,NSEG,NPTFR,
     & DT,QS,T1,T2,T3,BREACH,NUBO,VNOIN,MASS_SAND)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: LIEBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: QSX, QSY
      INTEGER,          INTENT(IN)    :: NPOIN, NSEG, NPTFR,KSORT
      DOUBLE PRECISION, INTENT(IN)    :: DT
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS, T1, T2, T3
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH
      DOUBLE PRECISION, INTENT(IN)    :: V2DPAR(NPOIN)
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: VNOIN(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: MASS_SAND(NPOIN)
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_NERBED_VF_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE USER_BEDLOAD_QB !
     & (HN, U2D, V2D, THETAC, HOULE, HW, TW, THETAW,
     &  TOB,TOBW,TOBCW_MEAN,TOBCW_MAX, DCLA, DENS, GRAV, DSTAR, AC,
     &  XMVE, XMVS, TETAP, MU, NPOIN, QSC, QSS, CSTAEQ)
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HN,U2D,V2D,THETAC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HW, TW, THETAW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB,TOBW,TOBCW_MEAN,TOBCW_MAX
      DOUBLE PRECISION, INTENT(IN)    :: DCLA, DENS, GRAV, DSTAR, AC
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, MU
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CSTAEQ
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      !----------------------------------------------------------------!
      END SUBROUTINE USER_BEDLOAD_QB
      !================================================================!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_SOLIDISCHARGE_GAIA !
      !----------------------------------------------------------------!
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
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
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
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: PHISED,BETA,BETA2
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CALFA,SALFA,COEFPN,COEFCR
      DOUBLE PRECISION, INTENT(IN)    :: BIJK,RATIO_SAND(NPOIN)
      TYPE(BIEF_OBJ),    INTENT(IN)   :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: H_TEL
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HW, THETAC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBCW_MEAN, TOBCW_MAX
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CSTAEQ
      DOUBLE PRECISION, INTENT(IN)    :: SANFRA(NPOIN)
!
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_SOLIDISCHARGE_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_SOULSBY_GAIA !
      !----------------------------------------------------------------!
     &  (UNORM,HN, UW, NPOIN, DENS, GRAV, DCLA, DSTAR, QSC,
     &   QSS,XMVS, UCRP, COEFCR, SLOPEFF)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)  :: HN, UNORM, UW, COEFCR
      INTEGER,          INTENT(IN)  :: NPOIN, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)  :: DENS, GRAV, DCLA, DSTAR,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UCRP ! WORK ARRAY T1      
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_SOULSBY_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_SOLVS_FE_GAIA
      !----------------------------------------------------------------!
     &(MESH,S,EBOR,MASKEL,MASK,
     & QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,KDDL,LIMTEC,DT,
     & MSK,ENTET,T1,T2,T3,T4,T8,
     & HZ,HZN,GLOSEG,DIMGLO,FLODEL,FLULIM,NSEG,UNSV2D,
     & ICLA,FLBCLA,RATIO_SAND,LIQBOR,QBOR,MAXADV,EVCL_MB,XMVS)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,MASKEL,MASK,QSX,QSY
      INTEGER,          INTENT(IN)    :: IELMT,NPOIN,NPTFR,KENT,KDIR
      INTEGER,          INTENT(IN)    :: DIMGLO,NSEG,ICLA,KDDL,MAXADV
      INTEGER,          INTENT(IN)    :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN)    :: DT,RATIO_SAND(NPOIN),XMVS
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(NSEG)
      LOGICAL,          INTENT(IN)    :: MSK,ENTET
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLODEL,T1,T2,T3,T4,T8
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HZ,EBOR,LIMTEC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLBCLA,EVCL_MB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HZN,UNSV2D,LIQBOR,QBOR
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_SOLVS_FE_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_SOLVS_VF_GAIA
     &(MESH,QSX,QSY,LIMTEC,UNSV2D,EBOR,BREACH,NSEG,NPTFR,NPOIN,
     & KENT,KDIR,KDDL,DT,FLUX,FLBCLA,
     & LIQBOR,QBOR,NUBO,VNOIN,EVCL_MB,RATIO_SAND,XMVS)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: QSX,QSY,LIMTEC,UNSV2D,EBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: BREACH,LIQBOR,QBOR
      INTEGER,          INTENT(IN)    :: NSEG,NPTFR,NPOIN,KENT,KDIR,KDDL
      DOUBLE PRECISION, INTENT(IN)    :: DT
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLBCLA,FLUX
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: EVCL_MB
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: VNOIN(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: RATIO_SAND(NPOIN),XMVS
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_SOLVS_VF_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_VANRIJN_GAIA
     & (TETAP, NPOIN, DCLA, DENS, GRAV, DSTAR, AC, ACP, QSC,
     &  XMVS, SLOPEFF, COEFCR)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)  :: TETAP, COEFCR
      INTEGER,          INTENT(IN)  :: NPOIN, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)  :: DCLA, DENS, GRAV, DSTAR, AC,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1      
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
!
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_VANRIJN_GAIA
      !================================================================!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_WILCOCK_CROWE_GAIA
     &(TOB, MU, ACLADM, DCLA, RATIO_SAND, GRAV, XMVE, XMVS, SANFRA, QSC,
     & ACP, SLOPEFF, COEFCR)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: SLOPEFF
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, GRAV, DCLA,
     &                                   RATIO_SAND(QSC%DIM1)
      DOUBLE PRECISION, INTENT(IN)    :: SANFRA(QSC%DIM1)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, COEFCR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, MU
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_WILCOCK_CROWE_GAIA
      !================================================================!
!
!> CVSM
!
      !================================================================!
      SUBROUTINE CVSP_MAIN_GAIA
     &(ZFCL_W,ZF,NSICLA,NPOIN)
      USE BIEF_DEF
      IMPLICIT NONE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZFCL_W,ZF
      INTEGER,          INTENT(IN)    :: NSICLA,NPOIN
      END SUBROUTINE CVSP_MAIN_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_INIT_GAIA
      IMPLICIT NONE
      END SUBROUTINE CVSP_INIT_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_ADD_FRACTION_GAIA
     &(J, I, DZFCL)
      IMPLICIT NONE
      INTEGER,          INTENT(IN) :: J
      INTEGER,          INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(IN) :: DZFCL
      END SUBROUTINE CVSP_ADD_FRACTION_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_ADD_SECTION_GAIA
     &(J)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: J
      END SUBROUTINE CVSP_ADD_SECTION_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_CHECK_ANYTHING_GAIA
      END SUBROUTINE CVSP_CHECK_ANYTHING_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_CHECK_MASS_BILAN_GAIA
      END SUBROUTINE CVSP_CHECK_MASS_BILAN_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_CHECK_STEADY_GAIA
     &(J)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: J
      END SUBROUTINE CVSP_CHECK_STEADY_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_COMPRESS_BRUT_GAIA(J)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: J
      END SUBROUTINE CVSP_COMPRESS_BRUT_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_COMPRESS_CLEAN_GAIA
     &(J)
      IMPLICIT NONE
      INTEGER,           INTENT(IN) :: J
      END SUBROUTINE CVSP_COMPRESS_CLEAN_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_COMPRESS_DP_GAIA
     &(J, THRESHOLD)
      IMPLICIT NONE
      INTEGER,           INTENT(IN)    :: J
      DOUBLE PRECISION,  INTENT(IN)    :: THRESHOLD
      END SUBROUTINE CVSP_COMPRESS_DP_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_INIT_FROM_LAYERS_GAIA
      END SUBROUTINE CVSP_INIT_FROM_LAYERS_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_MAKE_ACTLAY_GAIA
      END SUBROUTINE CVSP_MAKE_ACTLAY_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_OUTPUT_INIT_GAIA
      END SUBROUTINE CVSP_OUTPUT_INIT_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_P_GAIA
     &(PATH_PRE,FILE_PRE,JG)
      IMPLICIT NONE
      INTEGER     , INTENT(IN)  :: JG
      CHARACTER(*), INTENT(IN) :: PATH_PRE
      CHARACTER(*), INTENT(IN) :: FILE_PRE
      END SUBROUTINE CVSP_P_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_RM_FRACTION_GAIA
     &(J,I,DZFCL)
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: J
      INTEGER,          INTENT(IN)    :: I
      DOUBLE PRECISION, INTENT(IN)    :: DZFCL
      END SUBROUTINE CVSP_RM_FRACTION_GAIA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE CVSP_WRITE_PROFILE_GAIA
      END SUBROUTINE CVSP_WRITE_PROFILE_GAIA
      !================================================================!
!
!
      !================================================================!
      LOGICAL FUNCTION CVSP_DB_GAIA
     &(J_GLOBAL, TIMESTAMP)
      INTEGER, INTENT(IN)    :: J_GLOBAL
      INTEGER, INTENT(IN)    :: TIMESTAMP
      END FUNCTION CVSP_DB_GAIA
      !================================================================!
!
!
      !================================================================!
      LOGICAL RECURSIVE FUNCTION CVSP_CHECK_F_GAIA
     &(J,K, SOMETEXT)
      INTEGER,          INTENT(IN) :: J
      INTEGER,          INTENT(IN) :: K
      CHARACTER(LEN=10),INTENT(IN) :: SOMETEXT
      END FUNCTION CVSP_CHECK_F_GAIA
      !================================================================!
!
!
      !================================================================!
      LOGICAL RECURSIVE FUNCTION CVSP_CHECK_L_GAIA
     &(J,K, SOMETEXT)
      INTEGER,          INTENT(IN) :: J
      INTEGER,          INTENT(IN) :: K
      CHARACTER(LEN=10),INTENT(IN) :: SOMETEXT
      END FUNCTION CVSP_CHECK_L_GAIA
      !================================================================!
!
!
      !================================================================!
      DOUBLE PRECISION FUNCTION CVSP_ALT_GAIA
     &(J, FORMULA)
      USE BIEF
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: J
      INTEGER, INTENT(IN) :: FORMULA
      END FUNCTION CVSP_ALT_GAIA
      !================================================================!
!
!
      !================================================================!
      DOUBLE PRECISION FUNCTION CVSP_INTEGRATE_VOLUME_GAIA
     &(J,I,Z_HIGH,Z_LOW,A)
      USE DECLARATIONS_GAIA
      IMPLICIT NONE
      INTEGER,          INTENT(IN)  :: J
      INTEGER,          INTENT(IN)  :: I
      DOUBLE PRECISION, INTENT(IN)  :: Z_HIGH
      DOUBLE PRECISION, INTENT(IN)  :: Z_LOW
      DOUBLE PRECISION, INTENT(OUT) :: A(NSICLA)
      END FUNCTION CVSP_INTEGRATE_VOLUME_GAIA
      !================================================================!

! ENDFC
!======================================================================!
!======================================================================!
!
      END INTERFACE
      END MODULE INTERFACE_GAIA_BEDLOAD
