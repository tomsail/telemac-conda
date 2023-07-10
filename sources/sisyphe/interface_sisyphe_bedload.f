!                   **********************************
                    MODULE INTERFACE_SISYPHE_BEDLOAD !
!                   **********************************
!
!
!***********************************************************************
! SISYPHE   V8P0                                   12/09/2018
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTERFACE
!
!**********************************************************************C
! SISYPHE RELEASE 5.6  12/01/05  F. HUVELIN                            C
!**********************************************************************C
!
!history  F.CORDIER (EDF-LNHE)
!+        15/12/2017
!+        V7P2
!+ NEW SUBROUTINE - BEDLOAD_WILCOCK_CROWE
!
! ======================================= !
!  INTERFACE FOR THE SISYPHE SUBROUTINE   !
!         FOR BEDLOAD TRANSPORT           !
! ======================================= !
!
! COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT
!**********************************************************************C
!                                                                      C
!                 SSSS I   SSSS Y   Y PPPP  H   H EEEEE                C
!                S     I  S      Y Y  P   P H   H E                    C
!                 SSS  I   SSS    Y   PPPP  HHHHH EEEE                 C
!                    S I      S   Y   P     H   H E                    C
!                SSSS  I  SSSS    Y   P     H   H EEEEE                C
!                                                                      C
!----------------------------------------------------------------------C
!======================================================================!
!======================================================================!
!
      !================================================================!
      SUBROUTINE BEDLOAD_BAILARD !
      !----------------------------------------------------------------!
     &(U2D,V2D,UCMOY,TOB,TOBW,THETAW,UW,FW,CF,NPOIN,PI,
     & XMVE,GRAV,DENS,XWC,ALPHAW,QSCX,QSCY,QSSX,QSSY,
     & UC3X,UC3Y,US4X,US4Y,THETAC,FCW,QSC,QSS,HOULE)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,UCMOY, TOB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, THETAW, UW, FW, CF
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      DOUBLE PRECISION, INTENT(IN)    :: PI, XMVE, GRAV, DENS, XWC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ALPHAW  ! WORK ARRAY BEDLOAD_EB
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCX, QSCY    ! WORK ARRAY T3 AND T4
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSSX, QSSY    ! WORK ARRAY T5 AND T6
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UC3X, UC3Y    ! WORK ARRAY T7 AND T8
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: US4X, US4Y    ! WORK ARRAY T9 AND T10
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: THETAC, FCW   ! WORK ARRAY T11 AND T12
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC, QSS
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_BAILARD
!
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_BIJKER
      !----------------------------------------------------------------!
     &  (TOBW,TOB,MU,KSP,KSR,HN,NPOIN,DM,DENS,XMVE,GRAV,XWC,
     &     KARMAN,ZERO,T4,T7,T8,T9,QSC,QSS,BIJK,HOULE)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, TOB, MU,KSP,KSR,HN
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DM, DENS, XMVE, GRAV, XWC
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO
      LOGICAL,          INTENT(IN)    :: HOULE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T4
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T7, T8, T9
      TYPE(BIEF_OBJ),   INTENT(INOUT)  :: QSC, QSS
      DOUBLE PRECISION, INTENT(IN)     :: BIJK
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_BIJKER
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_CALCDW
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
      END SUBROUTINE BEDLOAD_CALCDW
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_DIBWAT !
      !----------------------------------------------------------------!
!
     &  (U2D,V2D,UCMOY, CF, TOB, TOBW, UW, TW, FW, THETAW, NPOIN,
     &   XMVE, DENS, GRAV, DM, XWC, PI, ALPHAW, T2, T3, UCW, UCN,
     &   UW1, UW2, TW1, TW2, THETAC, FCW, QSC)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,UCMOY, CF, TOB, TOBW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UW, TW, FW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: THETAW
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, DENS, GRAV, DM, XWC, PI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ALPHAW          ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2, T3          !
      TYPE(BIEF_OBJ),   INTENT(INOUT) ::  UCW, UCN ! WORK ARRAY T4, T5, T6
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UW1, UW2, TW1   ! WORK ARRAY T7, T8, T9
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TW2, THETAC     ! WORK ARRAY T10, T11
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FCW, QSC        ! WORK ARRAY T12
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_DIBWAT
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_DIFFIN
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
      END SUBROUTINE BEDLOAD_DIFFIN
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_DIRECTION
      !----------------------------------------------------------------!
     &  (QU, QV, NPOIN, PI, THETAC)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)  :: QU, QV
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: PI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: THETAC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_DIRECTION
      !================================================================!
!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_EFFPNT !
      !----------------------------------------------------------------!
!
     &(MASKEL,LIQBOR,S,ZF,NPOIN,NPTFR,IELMT,
     & KENT,BETA,PI,MSK,MESH,DZFDX,DZFDY,EPSIX,EPSIY,
     & COEF,CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2,
     & TOB,XMVS,XMVE,DM,GRAV,UNSV2D)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,LIQBOR,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF,TOB
      INTEGER,          INTENT(IN)    :: NPOIN,NPTFR,IELMT,KENT
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: BETA,PI,PHISED,BETA2
      DOUBLE PRECISION, INTENT(IN)    :: XMVS,XMVE,GRAV,DM
      LOGICAL,          INTENT(IN)    :: MSK
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DZFDX,DZFDY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: EPSIX,EPSIY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: COEF,CALFA,SALFA
!
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_EFFPNT
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_EINST
      !----------------------------------------------------------------!
     &  (TETAP, NPOIN, DENS, GRAV, DM, DSTAR, QSC)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, DSTAR
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_EINST
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_ENGEL
      !----------------------------------------------------------------!
     &        (TOB, CF, DENS, GRAV, DM, XMVE, QSC)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, CF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, XMVE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_ENGEL
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_ENGEL_CC
      !----------------------------------------------------------------!
     &   (TETAP,CF,NPOIN,GRAV,DM,DENS,TETA,QSC)
      !----------------------------------------------------------------!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, CF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, DM, DENS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETA ! WORK ARRAY T1, T2
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_ENGEL_CC
      !================================================================!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_EVOL
      !----------------------------------------------------------------!
!
     &(S,ELAY,AVA,COEFPN,CALFA,SALFA,LIMTEC,EBOR,
     & MASKEL,MASK,V2DPAR,UNSV2D,DEBUG,NPOIN,NPTFR,
     & IELMT,KENT,KDIR,KDDL,
     & DTS,
     & VF,ENTET,MSK,MESH,
     & QS,T1,T2,T3,T4,T8,
     & T11,T12,T13,CSF_SABLE,BREACH,QSX,QSY,ZFCL,SLOPEFF,ICLA,
     & FLBCLA,LIQBOR,QBOR,MAXADV)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,UNSV2D,ELAY
      TYPE(BIEF_OBJ),   INTENT(IN)    :: COEFPN,CALFA,SALFA
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,MASK,V2DPAR
      INTEGER,          INTENT(IN)    :: DEBUG,SLOPEFF,NPOIN,NPTFR,ICLA
      INTEGER,          INTENT(IN)    :: IELMT,KENT,KDIR,KDDL
      INTEGER,          INTENT(IN)    :: MAXADV
      DOUBLE PRECISION, INTENT(IN)    :: DTS
      DOUBLE PRECISION, INTENT(IN)    :: AVA(NPOIN)
      LOGICAL,          INTENT(IN)    :: VF,ENTET,MSK
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS,EBOR,FLBCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1, T2, T3, T4
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8,T11, T12, T13
      DOUBLE PRECISION, INTENT(IN)    :: CSF_SABLE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, QSX, QSY, ZFCL,LIMTEC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: LIQBOR,QBOR
!
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_EVOL
      !================================================================!
!
!
      !============================!
        SUBROUTINE BEDLOAD_FORMULA !
      !----------------------------------------------------------------!
!
     &  (U2D, V2D,UCMOY, HN, CF, MU,TOB, TOBW, UW, TW, THETAW, FW,
     &   ACLADM, UNLADM,KSP,KSR, AVA,  NPOIN, ICF, HIDFAC, XMVS, XMVE,
     &   DM, GRAV, VCE,  HMIN, XWC, D90, KARMAN, ZERO,
     &   PI, SUSP, AC, HIDING, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10,
     &   T11,CFP, QSC, QSS,IELMT,SECCURRENT,SLOPEFF,
     &   COEFPN, CALFA, SALFA,BIJK,HOULE, SANFRA)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D, V2D, UCMOY,HN, CF, TOB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MU,TOBW, UW, TW, THETAW, FW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, UNLADM, KSP,KSR
      INTEGER,          INTENT(IN)    :: NPOIN, ICF, HIDFAC,IELMT
      DOUBLE PRECISION, INTENT(IN)    :: XMVS, XMVE, DM, GRAV, VCE
      DOUBLE PRECISION, INTENT(IN)    :: HMIN, XWC, D90
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO, PI
      LOGICAL,          INTENT(IN)    :: SUSP,SECCURRENT,HOULE
      DOUBLE PRECISION, INTENT(INOUT) :: AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1, T2, T3, T4, T5, T6, T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8, T9, T10,T11
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CFP ! WORK ARRAY T12
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      TYPE(BIEF_OBJ),   INTENT(INOUT) ::  COEFPN,CALFA,SALFA
      INTEGER,          INTENT(IN)    :: SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: BIJK,AVA(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SANFRA(NPOIN)
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_FORMULA
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_HIDING_FACTOR
      !----------------------------------------------------------------!
     &  (ACLADM, HIDFAC, NPOIN, HIDI, DM, KARIM_HOLLY_YANG, HIDING)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)  :: ACLADM
      INTEGER,          INTENT(IN)  :: HIDFAC, NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: HIDI, DM, KARIM_HOLLY_YANG
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_HIDING_FACTOR
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_HUNZ_MEYER
      !----------------------------------------------------------------!
     &  (TOB,MU, ACLADM, UNLADM, NPOIN, DENS, XMVE, GRAV, DM, AC,
     &    TETAP, AHUNZI, ACP, HIDING, QSC)
      !----------------------------------------------------------------!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, MU, ACLADM, UNLADM
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DENS, XMVE, GRAV, DM, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETAP, AHUNZI ! WORK ARRAY T1, T2
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP           ! WORK ARRAY T3
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: HIDING, QSC
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_HUNZ_MEYER
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_INTERACT
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
      END SUBROUTINE BEDLOAD_INTERACT
      !================================================================!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_MAIN !
      !----------------------------------------------------------------!
!
     &(ACLADM,KSP,KSR, V2DPAR,UNSV2D,CF,EBOR,FW,HN,LIQBOR,
     & MASK, MASKEL, MASKPT, QBOR, U2D,
     & V2D, S,UNLADM,UW,THETAW,MU,TOB,TOBW,TW,ZF,
     & DEBUG, HIDFAC, ICF, IELMT, KDDL, KDIR,
     & KENT, KLOG, KNEU, KSORT,
     & NPOIN, NPTFR, NSICLA, OPTBAN, BETA, FD90, FDM,
     & GRAV, HIDI, HMIN, VCE, CSF_SABLE, XMVE, XMVS, XWC,
     & PI, KARMAN, ZERO, KARIM_HOLLY_YANG,MSK, SUSP, VF,
     & MESH,
     & ELAY, LIEBOR, LIMTEC, MASKTR,
     & IT1, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11,
     & T12,T13,UNORM,AC, DTS,
     & AVAIL, BREACH, CALFA_CL, COEFPN,
     & HIDING, QSCL_C, QSCL_S, QS_C,
     & QSCLXC, QSXC, QSCLYC, QSYC, SALFA_CL, ZF_C, ZFCL_C,
     & ENTETS, SECCURRENT, SLOPEFF,
     & PHISED, DEVIA, BETA2, BIJK,SEDCO,HOULE,
     & U3D,V3D,CODE,FLBCLA,MAXADV)
!
      USE BIEF_DEF
      USE DECLARATIONS_SISYPHE, ONLY : NOMBLAY
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
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF
      INTEGER,          INTENT(IN)    :: DEBUG, HIDFAC, ICF,MAXADV
      INTEGER,          INTENT(IN)    :: IELMT, KDDL, KDIR, KENT
      INTEGER,          INTENT(IN)    :: KLOG, KNEU, KSORT
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR
      INTEGER,          INTENT(IN)    :: NSICLA, OPTBAN
      DOUBLE PRECISION, INTENT(IN)    :: BETA, FD90(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: FDM(NSICLA),GRAV
      DOUBLE PRECISION, INTENT(IN)    :: HIDI(NSICLA),HMIN,VCE
      DOUBLE PRECISION, INTENT(IN)    :: CSF_SABLE,XMVE,XMVS,XWC(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: PI,KARMAN,ZERO
      DOUBLE PRECISION, INTENT(IN)    :: KARIM_HOLLY_YANG
      LOGICAL,          INTENT(IN)    :: MSK, SUSP, VF
      LOGICAL,          INTENT(IN)    :: SECCURRENT
      LOGICAL,          INTENT(IN)    :: SEDCO(NSICLA),HOULE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ELAY,FLBCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: LIEBOR, LIMTEC, MASKTR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: IT1,T1,T2,T3,T4,T5,T6,T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8,T9,T10,T11,T12,T13
      DOUBLE PRECISION, INTENT(INOUT) :: AC(NSICLA), DTS
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, CALFA_CL, COEFPN
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCL_C,QSCL_S
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS_C, QSCLXC, QSXC, QSCLYC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSYC, SALFA_CL, ZF_C, ZFCL_C
      LOGICAL,          INTENT(INOUT) :: ENTETS
      DOUBLE PRECISION,   INTENT(IN)  :: BETA2, PHISED
      INTEGER, INTENT (IN)            :: SLOPEFF, DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: BIJK
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
!
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_MAIN
      !================================================================!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_MEYER !
      !----------------------------------------------------------------!
     &  (TETAP, HIDING, HIDFAC, DENS, GRAV, DM, AC,
     &   ACP, QSC, SLOPEFF, COEFPN)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, HIDING
      INTEGER,          INTENT(IN)    :: HIDFAC, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC, COEFPN
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_MEYER
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_NEQ_BED_LOAD !
      !----------------------------------------------------------------!
!
     &  (HN, Q, S, CALFA, SALFA, MASK,LOADMETH, NPOIN, IELMT,
     &   LS0, DM, XMVE, XMVS, GRAV, VCE, D90, MESH, QSLO, T1, T2, T3,
     &   T4, T5, T6, T7, T8, T9, T10,MSK,MASKEL)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE (BIEF_OBJ),   INTENT(IN)    :: HN, Q, S, CALFA, SALFA, MASK
      TYPE (BIEF_OBJ),   INTENT(IN)    :: MASKEL
      INTEGER,           INTENT(IN)    :: LOADMETH, NPOIN, IELMT
      DOUBLE PRECISION,  INTENT(IN)    :: LS0
      DOUBLE PRECISION,  INTENT(IN)    :: DM, XMVE, XMVS, GRAV, VCE, D90
      TYPE (BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE (BIEF_OBJ),   INTENT(INOUT) :: QSLO
      TYPE (BIEF_OBJ),   INTENT(INOUT) :: T1,T2,T3,T4,T5
      TYPE (BIEF_OBJ),   INTENT(INOUT) :: T6,T7,T8,T9,T10
      LOGICAL, INTENT(IN)              :: MSK
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_NEQ_BED_LOAD
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_NERBED_VF !
     &(MESH,LIEBOR,KSORT,ELAY,V2DPAR,QSX,QSY,AVA,NPOIN,NSEG,NPTFR,
     & DT,QS,T1,T2,T3,BREACH,CSF_SABLE,NUBO,VNOIN)
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
      DOUBLE PRECISION, INTENT(IN)    :: ELAY(NPOIN),V2DPAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AVA(NPOIN), CSF_SABLE
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: VNOIN(3,NSEG)
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_NERBED_VF
      !================================================================!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_SOLIDISCHARGE !
      !----------------------------------------------------------------!
!
     &  (MESH, U2D, V2D, UNORM, HN,  TW, UW, MU,TOB, CF, TOBW, FW,
     &   THETAW, AVA,  MASKPT, MASKEL, ACLADM, UNLADM,KSP, KSR,LIQBOR,
     &   DEBUG, NPOIN, NPTFR, IELMT, ICF, KENT, OPTBAN,
     &   HIDFAC, GRAV, DM, D90, XWC, XMVE, XMVS, VCE, HMIN,
     &   HIDI,KARMAN, ZERO, PI, KARIM_HOLLY_YANG,
     &   SUSP, MSK,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10,
     &   T11,T12, AC,HIDING, QSC, QSS,
     &   SLOPEFF, COEFPN, PHISED, CALFA, SALFA, BETA, ZF_C, S,
     &   DEVIA, BETA2, SECCURRENT,BIJK,HOULE,UNSV2D,
     &   U3D,V3D,CODE,SANFRA)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D, V2D,  HN, TW, UW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UNORM,MU,TOB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: KSP, CF, TOBW, FW, THETAW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKPT, MASKEL
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, UNLADM,KSR,LIQBOR
      INTEGER,          INTENT(IN)    :: DEBUG
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR, IELMT, ICF
      INTEGER,          INTENT(IN)    :: KENT, OPTBAN,HIDFAC
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, DM, D90, XWC, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    ::  VCE, HMIN,HIDI
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO, PI
      DOUBLE PRECISION, INTENT(IN)    :: KARIM_HOLLY_YANG
      LOGICAL,          INTENT(IN)    :: SUSP, MSK,SECCURRENT,HOULE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T7,T8,T9,T10,T11,T12
      DOUBLE PRECISION, INTENT(INOUT) :: AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING,QSC,QSS
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: PHISED,BETA,BETA2
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF_C,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CALFA,SALFA,COEFPN
      DOUBLE PRECISION, INTENT(IN)    :: BIJK,AVA(NPOIN)
!
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
      DOUBLE PRECISION, INTENT(IN)    :: SANFRA(NPOIN)
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_SOLIDISCHARGE
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_SOULSBY !
      !----------------------------------------------------------------!
     &(UCMOY,HN,UW,NPOIN,DENS,GRAV,DM,DSTAR,D90,QSC,QSS)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)  :: HN, UCMOY, UW
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: DENS, GRAV, DM, DSTAR, D90
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_SOULSBY
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_SOLVS_FE
      !----------------------------------------------------------------!
     &(MESH,S,EBOR,MASKEL,MASK,
     & QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,KDDL,LIMTEC,DT,
     & MSK,ENTET,T1,T2,T3,T4,T8,
     & ZFCL,HZ,HZN,GLOSEG,DIMGLO,FLODEL,FLULIM,NSEG,UNSV2D,CSF_SABLE,
     & ICLA,FLBCLA,AVA,LIQBOR,QBOR,MAXADV)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,MASKEL,MASK,QSX,QSY
      INTEGER,          INTENT(IN)    :: IELMT,NPOIN,NPTFR,KENT,KDIR
      INTEGER,          INTENT(IN)    :: DIMGLO,NSEG,ICLA,KDDL,MAXADV
      INTEGER,          INTENT(IN)    :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN)    :: DT,CSF_SABLE,AVA(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(NSEG)
      LOGICAL,          INTENT(IN)    :: MSK,ENTET
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLODEL,T1,T2,T3,T4,T8
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HZ,EBOR,LIMTEC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ZFCL,FLBCLA
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HZN,UNSV2D,LIQBOR,QBOR
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_SOLVS_FE
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_SOLVS_VF
     &(MESH,QSX,QSY,LIMTEC,UNSV2D,EBOR,BREACH,NSEG,NPTFR,NPOIN,
     & KENT,KDIR,KDDL,DT,ZFCL,FLUX,CSF_SABLE,FLBCLA,AVA,LIQBOR,QBOR,
     & NUBO,VNOIN)
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: QSX,QSY,LIMTEC,UNSV2D,EBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: BREACH,LIQBOR,QBOR
      INTEGER,          INTENT(IN)    :: NSEG,NPTFR,NPOIN,KENT,KDIR,KDDL
      DOUBLE PRECISION, INTENT(IN)    :: DT,CSF_SABLE
      DOUBLE PRECISION, INTENT(IN)    :: AVA(NPOIN)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLBCLA,ZFCL,FLUX
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: VNOIN(3,NSEG)
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_SOLVS_VF
      !================================================================!
!
!
      !================================================================!
      SUBROUTINE BEDLOAD_VANRIJN
     &  (TOB, NPOIN, DM, DENS, GRAV, DSTAR, AC, QSC)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)  :: TOB
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: DM, DENS, GRAV, DSTAR, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
!
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_VANRIJN
      !================================================================!
!
!
      !================================================================!
        SUBROUTINE BEDLOAD_WILCOCK_CROWE !
      !----------------------------------------------------------------!
     &(TOB, MU, ACLADM, DM, AVA, GRAV, XMVE, XMVS, SANFRA, QSC, AC, ACP,
     & SLOPEFF, COEFPN)
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: SLOPEFF
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, COEFPN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, GRAV, DM, AC,
     &                                   AVA(QSC%DIM1)
      DOUBLE PRECISION, INTENT(IN)    :: SANFRA(QSC%DIM1)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, MU
      !----------------------------------------------------------------!
      END SUBROUTINE BEDLOAD_WILCOCK_CROWE
      !================================================================!
! ENDFC
!======================================================================!
!======================================================================!
!
      END INTERFACE
      END MODULE INTERFACE_SISYPHE_BEDLOAD
