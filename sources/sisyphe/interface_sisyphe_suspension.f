!                   *************************************
                    MODULE INTERFACE_SISYPHE_SUSPENSION !
!                   *************************************
!
!
!***********************************************************************
! SISYPHE   V6P2                                   18/06/2012
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTERFACE                           !
! *********************************** !
!**********************************************************************C
! SISYPHE RELEASE 5.9  25/06/2008                             F. HUVELIN
!**********************************************************************C
            ! ======================================= !
            !  INTERFACE FOR THE SISYPHE SUBROUTINE   !
            !        FOR THE SUSPENDED TRANSPORT      !
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

      !================================================================!
      SUBROUTINE SUSPENSION_BILAN
     &(MESH,CST,HN,ZFCL_S,MASKEL,IELMT,ITRA,LT,NIT,DT,CSF,
     & MASSOU,MASED0,MSK,ENTET,MASTEN,MASTOU,MASINI,T2,
     & T3,MASFIN,MASDEPT,MASDEP,AGGLOT,
     & VOLU2D,NUMLIQ,NFRLIQ,NPTFR,FLBORTRA)
      USE BIEF
      IMPLICIT NONE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CST,HN,VOLU2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZFCL_S,MASKEL,FLBORTRA
      INTEGER,          INTENT(IN)    :: IELMT,ITRA,LT,NIT
      INTEGER,          INTENT(IN)    :: NFRLIQ,NPTFR
      INTEGER,          INTENT(IN)    :: NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,CSF
      DOUBLE PRECISION, INTENT(IN)    :: MASSOU,MASED0,AGGLOT
      LOGICAL,          INTENT(IN)    :: MSK,ENTET
      DOUBLE PRECISION, INTENT(INOUT) :: MASTEN,MASTOU,MASINI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2,T3
      DOUBLE PRECISION, INTENT(INOUT) :: MASFIN,MASDEPT,MASDEP
      END SUBROUTINE SUSPENSION_BILAN
      !================================================================!
!                   ***************************
                    SUBROUTINE SUSPENSION_BILAN_COH
!                   ***************************
!
     &(MESH,CST,HN,MASKEL,IELMT,ITRA,LT,NIT,DT,
     &  XMVS,MS_VASE, NOMBLAY,NPOIN,
     &  MASSOU,MASED0,MSK,ENTET,MASTEN,MASTOU,MASINI,T1,T2,
     &  T3,MASFIN,MASDEPT,MASDEP,AGGLOT,
     &  VOLU2D,NUMLIQ,NFRLIQ,NPTFR,FLBORTRA,SEDCO)
      USE BIEF
      IMPLICIT NONE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HN,CST
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,FLBORTRA
      TYPE(BIEF_OBJ),   INTENT(IN)    :: VOLU2D
      INTEGER,          INTENT(IN)    :: IELMT,ITRA,LT,NIT,NFRLIQ,NPTFR
      INTEGER,          INTENT(IN)    :: NUMLIQ(NPTFR), NOMBLAY,NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DT,XMVS
      DOUBLE PRECISION, INTENT(IN)    :: MASSOU,MASED0,AGGLOT
      LOGICAL,          INTENT(IN)    :: MSK,ENTET,SEDCO
      DOUBLE PRECISION, INTENT(INOUT) :: MASTEN,MASTOU,MASINI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2,T3,T1
      DOUBLE PRECISION, INTENT(INOUT) :: MASFIN,MASDEPT,MASDEP
      DOUBLE PRECISION, INTENT(IN)    :: MS_VASE(NPOIN, NOMBLAY)
      END SUBROUTINE SUSPENSION_BILAN_COH

      !================================================================!
      SUBROUTINE SUSPENSION_COMPUTATION
      ! ********************************* !
     &(SLVTRA, HN,HN_TEL,UCONV, VCONV, MU,TOB,FDM, FD90, KSP,KSR,KS,
     & ELAY, AVA, AFBOR, BFBOR, LIMDIF, CLT, MASKEL, MASKTR,
     & MASKPT, IFAMAS, NPOIN, IELM, NPTFR, ITRA, LT, NIT, RESOL,
     & OPTBAN, KENT,KDDL,KDIR,KSORT,KLOG,KNEU,
     & OPTSUP, OPDTRA, DEBUG,CSF_SABLE,
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
      USE BIEF
      IMPLICIT NONE
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
      INTEGER,          INTENT(IN)    :: KDIR,OPTSUP,OPDTRA,SOLSYS
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
      TYPE (BIEF_OBJ),  INTENT(INOUT)  :: QSCLXS,QSCLYS
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(*)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_SABLE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_VASE(*)
      DOUBLE PRECISION, INTENT(INOUT)  :: MASFIN,MASDEPT,MASDEP
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
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_COMPUTATION
      !================================================================!

      !================================================================!
        SUBROUTINE SUSPENSION_CONV
     &(TOB,XMVE,KSR,NPOIN,ZREF,U2D,V2D,HN,
     & UCONV,VCONV,KARMAN,ZERO,XWC,ALPHA,RESOL,GLOSEG1,GLOSEG2,NSEG,
     & FLULIM,YAFLULIM,SOLSYS_SIS,SOLSYS,UCONV_TEL,VCONV_TEL)
      USE BIEF_DEF
      IMPLICIT NONE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: HN,U2D,V2D,ZREF,KSR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: UCONV,VCONV,ALPHA,FLULIM
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TOB,UCONV_TEL,VCONV_TEL
      INTEGER,          INTENT(IN)    :: NPOIN,RESOL,NSEG,SOLSYS
      INTEGER,          INTENT(IN)    :: GLOSEG1(NSEG),GLOSEG2(NSEG)
      INTEGER,          INTENT(INOUT) :: SOLSYS_SIS
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,XWC
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,XMVE
      LOGICAL, INTENT(INOUT)          :: YAFLULIM
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_CONV
      !================================================================!

      !================================================================!
      SUBROUTINE SUSPENSION_DISPERSION
      !----------------------------------------------------------------!
     & (TOB, XMVE,HN,  OPTDIF, NPOIN, XKX, XKY,
     &   T1, T2, T3, KX, KY, KZ, DISP,U2D,V2D,VISC_TEL,CODE)
      !----------------------------------------------------------------!
      USE BIEF
      IMPLICIT NONE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TOB, HN,VISC_TEL
      INTEGER,          INTENT(IN)    :: OPTDIF, NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XKX, XKY
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1, T2, T3
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: KX, KY, KZ, DISP
      TYPE (BIEF_OBJ),  INTENT(IN)    :: U2D,V2D
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_DISPERSION
      !================================================================!

      !==============================!
      SUBROUTINE SUSPENSION_DEPOT    !
      ! **************************** !
     &(TOB,HN, NPOIN, HMIN,XWC,VITCD,
     & ZERO,KARMAN,FDM,FD90,XMVE, T1,T2,ZREF,FLUDPT,DEBUG,SEDCO)
      USE BIEF
      IMPLICIT NONE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: HN, TOB
      INTEGER,          INTENT(IN)    :: NPOIN,DEBUG
      LOGICAL,          INTENT(IN)    :: SEDCO
      DOUBLE PRECISION, INTENT(IN)    :: HMIN
      DOUBLE PRECISION, INTENT(IN)    :: FDM,FD90,XWC
      DOUBLE PRECISION, INTENT(IN)    :: VITCD
      DOUBLE PRECISION, INTENT(IN)    :: ZERO, KARMAN,XMVE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1,T2
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUDPT
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_DEPOT
      !================================================================!

      !================================!
        SUBROUTINE SUSPENSION_EROSION  !
      ! ****************************** !

     &(TAUP,HN,FDM,FD90,AVA,NPOIN,CHARR,XMVE,XMVS,VCE,GRAV,XWC,
     & ZERO,ZREF,AC,FLUER,CSTAEQ,QSC,ICQ,U2D,V2D,CSRATIO,DEBUG)
!
      USE BIEF
      IMPLICIT NONE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TAUP,HN,U2D,V2D,CSRATIO
      INTEGER,          INTENT(IN)    :: NPOIN,DEBUG
      LOGICAL,          INTENT(IN)    :: CHARR
      DOUBLE PRECISION, INTENT(IN)    :: XMVE,XMVS,GRAV,VCE,ZERO
      DOUBLE PRECISION, INTENT(IN)    :: XWC,AVA(NPOIN),FDM,FD90
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      DOUBLE PRECISION, INTENT(INOUT) :: AC
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER,CSTAEQ
      TYPE(BIEF_OBJ),   INTENT(IN)    :: QSC
      INTEGER,          INTENT (IN)   :: ICQ
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_EROSION
      !================================================================!

      !================================!
      SUBROUTINE SUSPENSION_EROSION_COH
      ! ****************************** !
     &(TAUP,NPOIN,XMVS,PARTHENIADES,
     & FLUER, TOCE_VASE, NOMBLAY, DT, MS_VASE)
!!
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER,          INTENT(IN)     :: NOMBLAY
      INTEGER,          INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: XMVS
      DOUBLE PRECISION, INTENT(IN)     :: PARTHENIADES
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_VASE(NOMBLAY), DT
      TYPE (BIEF_OBJ),  INTENT(INOUT)  :: FLUER
      TYPE (BIEF_OBJ),  INTENT(IN)     :: TAUP
!
!
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_EROSION_COH
      !================================================================!

      !================================!
        SUBROUTINE SUSPENSION_FLUX_MIXTE
      ! ****************************** !
     &  (TAUP,FDM,NPOIN,
     &   CHARR,XMVE,XMVS,VCE,GRAV,XWC,
     &   ZERO,PARTHENIADES,FLUER_SABLE,FLUER_VASE,ZREF,
     &   AC,CSTAEQ,QSC,ICQ,DEBUG,AVAIL,NSICLA,ES,
     &   TOCE_VASE,TOCE_SABLE,
     &   NOMBLAY,DT,TOCE_MIXTE,MS_SABLE,MS_VASE)

      USE BIEF_DEF
      IMPLICIT NONE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TAUP
      INTEGER,          INTENT(IN)    :: NPOIN,DEBUG,NSICLA
      INTEGER,          INTENT(IN)    :: NOMBLAY
      LOGICAL,          INTENT(IN)    :: CHARR
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, VCE,GRAV
      DOUBLE PRECISION, INTENT(IN)    ::  XWC,FDM
      DOUBLE PRECISION, INTENT(IN)    :: ZERO, PARTHENIADES
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      DOUBLE PRECISION, INTENT(INOUT) :: AC,AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: CSTAEQ
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER_SABLE,FLUER_VASE
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) ::TOCE_MIXTE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)      :: DT
      TYPE(BIEF_OBJ),   INTENT(IN)       ::  QSC
      INTEGER,          INTENT (IN)      :: ICQ
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_VASE(NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_SABLE
      !----------------------------------------------------------------!
      END SUBROUTINE
      !================================================================!

      !================================================================!
      SUBROUTINE SUSPENSION_FREDSOE
      !----------------------------------------------------------------!
     &  (FDM, TAUP, NPOIN, GRAV, XMVE, XMVS, AC,  CSTAEQ)
      USE BIEF
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    ::  TAUP
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    ::  GRAV, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: FDM
      DOUBLE PRECISION, INTENT(IN)    :: AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) ::  CSTAEQ
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_FREDSOE
      !================================================================!


      !================================================================!
      SUBROUTINE SUSPENSION_LISTING
      !----------------------------------------------------------------!
     &  (MESH,CST,ZFCL_S,UCONV,VCONV,MASKEL,IELMT,DT,MSK,T1)
      !----------------------------------------------------------------!
      USE BIEF
      IMPLICIT NONE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CST, ZFCL_S
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UCONV, VCONV, MASKEL
      INTEGER,          INTENT(IN)    :: IELMT
      DOUBLE PRECISION, INTENT(IN)    :: DT
      LOGICAL,          INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_LISTING
      !================================================================!

        SUBROUTINE SUSPENSION_MAIN
      ! ************************** !
     &(SLVTRA,HN,HN_TEL,MU,TOB,FDM,FD90,KSP,KSR,KS,VOLU2D,V2DPAR,UNSV2D,
     & AFBOR,BFBOR,ZF,LICBOR,IFAMAS,MASKEL,MASKPT,U2D,V2D,NSICLA,NPOIN,
     & NPTFR,IELMT,OPTDIF,RESOL,LT,NIT,OPTBAN,OPTADV,OPDTRA,
     & KENT,KSORT,KLOG,KNEU,KDIR,KDDL,DEBUG,
     & DTS,CSF_SABLE,ZERO,GRAV,XKX,XKY,KARMAN,
     & XMVE,XMVS,VCE,HMIN,XWC,VITCD,PARTHENIADES,BILMA,MSK,
     & CHARR,IMP_INFLOW_C,MESH,ZF_S,CS,CST,CTILD,CBOR,DISP,
     & IT1,IT2,IT3,IT4,TB,T1,T2,T3,T4,T8,T9,T10,T11,T12,T14,
     & TE1,CLT,TE2,TE3,S,AM1_S,AM2_S,MBOR,ELAY,LIMDIF,
     & MASKTR, TETA_SUSP, AC, MASED0, MASINI, MASTEN,
     & MASTOU, ES,ES_SABLE, ES_VASE,AVAIL,  ENTETS, PASS, ZFCL_S,
     & HPROP, FLUDPT, FLUDP, FLUER, DISP_C, KX, KY,
     & KZ, UCONV, VCONV,QSXS, QSYS, QSCLXS, QSCLYS, QSCL_S,
     & QS_S,QS_C,CSTAEQ,CSRATIO,ICQ,MASTCP,MASFIN,MASDEPT,MASDEP,MASSOU,
     & CORR_CONV,ZREF,SEDCO,VISC_TEL,CODE,
     & DIFT,DM1,UCONV_TEL,VCONV_TEL,ZCONV,SOLSYS,FLBOR_TEL,FLBOR_SIS,
     & FLBORTRA,NUMLIQ,NFRLIQ,MIXTE,NOMBLAY,CONC,
     & TOCE_VASE,TOCE_SABLE,FLUER_VASE,TOCE_MIXTE,MS_SABLE,MS_VASE,
     & DIRFLU,MAXADV)

      USE BIEF
      IMPLICIT NONE
      TYPE (SLVCFG),    INTENT(INOUT) :: SLVTRA
      TYPE (BIEF_OBJ),  INTENT(IN)    :: HN,HN_TEL,MU,TOB
      TYPE (BIEF_OBJ),  INTENT(IN)    :: KSP,KSR,KS
      TYPE (BIEF_OBJ),  INTENT(IN)    :: VOLU2D,AFBOR,BFBOR,ZF
      TYPE (BIEF_OBJ),  INTENT(IN)    :: V2DPAR,UNSV2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: LICBOR, IFAMAS, MASKEL, MASKPT
      TYPE (BIEF_OBJ),  INTENT(IN)    :: U2D, V2D,DM1,ZCONV,FLBOR_TEL
      INTEGER,          INTENT(IN)    :: NSICLA, NPOIN, NPTFR, IELMT
      INTEGER,          INTENT(IN)    :: OPTDIF, RESOL,LT, NIT
      INTEGER,          INTENT(IN)    :: OPTBAN,OPTADV,OPDTRA,NFRLIQ
      INTEGER,          INTENT(IN)    :: KENT, KSORT, KLOG, KNEU
      INTEGER,          INTENT(IN)    :: KDIR,KDDL
      INTEGER,          INTENT(IN)    :: DEBUG,SOLSYS,NOMBLAY,MAXADV
      INTEGER,          INTENT(IN)    :: NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DTS,CSF_SABLE
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,GRAV
      DOUBLE PRECISION, INTENT(IN)    :: FDM(NSICLA),FD90(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: XKX,XKY,KARMAN,VCE
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, HMIN, XWC(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: VITCD
      DOUBLE PRECISION, INTENT(IN)    :: PARTHENIADES
      LOGICAL,          INTENT(IN)    :: BILMA, MSK, CHARR
      LOGICAL,          INTENT(IN)    :: IMP_INFLOW_C
      LOGICAL,          INTENT(IN)    :: SEDCO(NSICLA),MIXTE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZF_S,CS,CST,CTILD,CBOR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DISP,IT1,IT2,IT3,IT4,TB
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1,T2,T3,T4,T8
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T9,T10,T11,T12,T14,TE1,CLT
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: TE2,TE3,S,AM1_S,AM2_S,MBOR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ELAY, LIMDIF,FLBORTRA
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MASKTR
      DOUBLE PRECISION, INTENT(INOUT) :: TETA_SUSP, AC(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASED0(NSICLA), MASINI(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASTEN(NSICLA), MASTOU(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: TOCE_VASE(NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: TOCE_SABLE
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      LOGICAL,          INTENT(INOUT) :: ENTETS, PASS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZFCL_S,HPROP,ZREF
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUDPT,FLUDP,FLUER
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DISP_C,KX,KY,KZ,UCONV
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: VCONV,FLBOR_SIS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: QSXS,QSYS,QSCLXS,QSCLYS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: QSCL_S,QS_S,CSTAEQ,CSRATIO
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER_VASE,TOCE_MIXTE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MS_SABLE,MS_VASE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: QS_C,VISC_TEL
      TYPE (BIEF_OBJ),  INTENT(IN)    :: UCONV_TEL,VCONV_TEL
      DOUBLE PRECISION, INTENT(OUT)   :: MASTCP(NSICLA),MASFIN(NSICLA)
      DOUBLE PRECISION, INTENT(OUT)   :: MASDEPT(NSICLA),MASDEP(NSICLA)
      DOUBLE PRECISION, INTENT(OUT)   :: MASSOU
      INTEGER, INTENT(IN)             :: ICQ,DIRFLU
      LOGICAL, INTENT (IN)            :: CORR_CONV,DIFT
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
!
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_MAIN
      !================================================================!


      !================================================================!
        SUBROUTINE SUSPENSION_ROUSE
      !================================================================!

     & (USTAR,HN,NPOIN,KARMAN,ZERO,XWC,ZREF,T2)
      USE BIEF
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN) :: USTAR,HN,ZREF
      INTEGER,          INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(IN) :: KARMAN,XWC,ZERO
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_ROUSE
      !================================================================!

      !================================================================!
        SUBROUTINE SUSPENSION_BIJKER
      !================================================================!

     &  (TAUP, NPOIN, CHARR, QSC, ZREF, ZERO, CSTAEQ, XMVE)


      USE BIEF
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TAUP, QSC
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: CHARR
      DOUBLE PRECISION, INTENT(IN)    :: ZERO
      DOUBLE PRECISION, INTENT(IN)    :: XMVE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_BIJKER
      !================================================================!
      ! ************************** !
        SUBROUTINE SUSPENSION_EVOL
      ! ************************** !

     &(ZFCL_S,FLUDP,FLUER,DT, NPOIN,XMVS, QFLUX,MS_VASE,ES_VASE,
     & CONC,NOMBLAY)
!
      USE BIEF
      IMPLICIT NONE
      TYPE (BIEF_OBJ),  INTENT(INOUT)   :: ZFCL_S,FLUDP,FLUER,QFLUX
      DOUBLE PRECISION, INTENT(IN)      :: DT, XMVS
      INTEGER, INTENT(IN)               :: NPOIN,NOMBLAY
      DOUBLE PRECISION, INTENT(INOUT)   :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT)  :: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT)  :: ES_VASE(NPOIN,NOMBLAY)

      END SUBROUTINE SUSPENSION_EVOL
      ! ***************************** !
        SUBROUTINE SUSPENSION_VANRIJN ! (_IMP_)
      ! ***************************** !

     &  (FDM, TAUP, NPOIN, GRAV,
     &   XMVE, XMVS,VCE, ZERO, AC, CSTAEQ,ZREF)

      USE BIEF
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TAUP,ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    ::  GRAV,  XMVE, XMVS,VCE
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,AC,FDM
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ

      END SUBROUTINE SUSPENSION_VANRIJN

      ! ***************************** !
        SUBROUTINE SUSPENSION_SANDFLOW ! (_IMP_)
      ! ***************************** !
!
     &  (FDM, FD90,  NPOIN, GRAV,
     &   XMVE, XMVS, ZERO, CSTAEQ,HN,U2D,V2D,CSRATIO)
!
      USE BIEF
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HN,U2D,V2D,CSRATIO
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: ZERO, FDM, FD90
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ
!
      END SUBROUTINE SUSPENSION_SANDFLOW
!
!======================================================================!
!======================================================================!

!================================================================!
      SUBROUTINE SUSPENSION_MILES
!================================================================!
!
     & (HN,NPOIN,HMIN,FDM,FD90,XWC,T2)
      USE BIEF
      IMPLICIT NONE
      TYPE(BIEF_OBJ),   INTENT(IN) :: HN
      INTEGER,          INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(IN) :: FDM,FD90,XWC,HMIN
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2
      !----------------------------------------------------------------!
      END SUBROUTINE SUSPENSION_MILES
      !================================================================!

      END INTERFACE
      END MODULE INTERFACE_SISYPHE_SUSPENSION
!
!#######################################################################
!
