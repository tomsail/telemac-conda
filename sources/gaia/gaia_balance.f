!                   ***********************
                    SUBROUTINE GAIA_BALANCE
!                   ***********************
     &(ZF_TEL,TELNIT)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Compute mass balance and evolutions to display informations on
!! the evolutions of the data
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  ZF_TEL     BOTTOM ELEVATION OF THE CALLING TELEMAC
!>@param  [in]      TELNIT     NUMBER OF TELEMAC ITERATIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE INTERFACE_GAIA, EX_GAIA_BALANCE => GAIA_BALANCE
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_GAIA
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      USE INTERFACE_PARALLEL, ONLY: P_MIN,P_MAX,P_SUM
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: ZF_TEL
      INTEGER,           INTENT(IN)    :: TELNIT
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      INTEGER IMA, IMI, IMAX, IMIN
      DOUBLE PRECISION :: XMA
      DOUBLE PRECISION :: XMI
      DOUBLE PRECISION :: XMAX, XMIN
      DOUBLE PRECISION :: SUM_EVOL_MM
!
!-----------------------------------------------------------------------
!
!=======================================================================
!     COMPUTES THE EVOLUTIONS FOR THIS CYCLE OF TIMESTEP
!     AND UPDATES AFTER THIS COMPUTATION
!=======================================================================
!
!     COMPUTES  THE EVOLUTIONS FOR THIS TIMESTEP
!     EVOLUTIONS ARE MASS EVOLUTION
!     FIXME: E, EVOL_MB, EVOL_MS
!
      IF(CHARR) THEN
        CALL OS('X=Y     ',X=E,Y=EVOL_MB)
      ELSE
        CALL OS('X=0     ',X=E)
      ENDIF
!
      IF(SUSP)  CALL OS('X=X+Y   ',X=E,Y=EVOL_MS)
!
!     consolidation
      IF(BED_MODEL.EQ.2.OR.BED_MODEL.EQ.3) THEN
!     evolution of mass with consodilation is logicaly 0
!       (we check this in bed1_consolidation_layer.f)
      !>@todo can be removed if it works

        DO I=1,NPOIN
          IF(EVOL_MC%R(I).GT.MIN_SED_MASS_COMP)THEN
            WRITE(LU,*)'CONSOLIDATION MODEL SEEMS
     &                  TO BE NOT CONSERVATIVE'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
!       evolution of mass with consodilation is logicaly 0
!>@ to do can be removed if it works
        CALL OS('X=X+Y   ',X=E,Y=EVOL_MC)
      ENDIF
!
      IF(SLIDE.AND.ENTET) THEN
        SUM_EVOL_MM = 0.D0
        DO I=1,NPOIN
          SUM_EVOL_MM = SUM_EVOL_MM + EVOL_MM%R(I)*VOLU2D%R(I)
        ENDDO
        IF(NCSIZE.GT.1) THEN
          SUM_EVOL_MM = P_SUM(SUM_EVOL_MM)
        ENDIF
        WRITE(LU,111) SUM_EVOL_MM
111     FORMAT(5X,'MASS EVOLUTION DUE TO SLIDING =',G16.7,'(KG)')
        CALL OS('X=X+Y   ',X=E,Y=EVOL_MM)
      ENDIF
!
!     FIXME: THIS IS NOT USED
      CALL OS('X=X+Y   ', X=ESOMT, Y=E)
!
!     EVOLUTIONS IN METER FOR OUTPUTS
!
      CALL OS('X=Y-Z   ', X=T1, Y=ZF, Z=ZF_TEL)
      CALL OS('X=X+Y   ', X=CUMBE, Y=T1)
!
!=====================================================================
!      MASS BALANCE
!=====================================================================
!     COMPUTES THE COMPONENTS OF SAND TRANSPORT FOR THE MASS BALANCE,
!     GRAPHIC OUTPUTS AND VALIDATION STAGE
!
      IF(BILMA) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'BILAN_GAIA'
        CALL MASS_BALANCE(DT,NPTFR,ENTET,NSICLA,
     &                    NUMLIQ%I,NFRLIQ,FLBCLA,LT,TELNIT,NPOIN,
     &                    VOLU2D,CHARR,SUSP,EVCL_MB,EVCL_MS,
     &                    MASSTOT,MASS0TOT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_BILAN_GAIA'
      ENDIF
!
!     CONTROL SECTIONS
!
      IF(NCP.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'FLUSEC_GAIA'
        CALL FLUSEC_GAIA(U2D,V2D,HN,
     &                      QSXC,QSYC,CHARR,SUSP,
     &                      MESH%IKLE%I,
     &                      MESH%NELMAX,MESH%NELEM,
     &                      MESH%X%R,MESH%Y%R,
     &                      DT,NCP,CTRLSC,ENTET,AT0)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_FLUSEC_GAIA'
      ENDIF
!
!------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_GAIA_SUBITERATION_END
#endif
!
!=====================================================================
!     PRINTS OUT EXTREME VALUES
!=====================================================================
!
      IF(ENTET.AND.CHARR) THEN
        CALL OS('X=Y-Z   ', X=T1, Y=ZF, Z=ZF_TEL)
        WRITE(LU,*)
        CALL MAXI(XMAX,IMAX,T1%R,NPOIN)
        IF(NCSIZE.GT.1) THEN
          XMA=P_MAX(XMAX)
          IF(XMAX.EQ.XMA) THEN
            IMA=MESH%KNOLG%I(IMAX)
          ELSE
            IMA=0
          ENDIF
          IMA=P_MAX(IMA)
        ELSE
          IMA=IMAX
          XMA=XMAX
        ENDIF
        WRITE(LU,372) XMA,IMA
372     FORMAT(' MAXIMAL EVOLUTION (M)    : ',G16.7,' NODE  :',I6)
        CALL MINI(XMIN,IMIN,T1%R,NPOIN)
        IF(NCSIZE.GT.1) THEN
          XMI=P_MIN(XMIN)
          IF(XMIN.EQ.XMI) THEN
            IMI=MESH%KNOLG%I(IMIN)
          ELSE
            IMI=0
          ENDIF
          IMI=P_MAX(IMI)
        ELSE
          IMI=IMIN
          XMI=XMIN
        ENDIF
        WRITE(LU,374) XMI,IMI
374     FORMAT(' MINIMAL EVOLUTION (M)    : ',G16.7,' NODE  :',I6)
!
        IF(CONST_ALAYER) THEN
          IF(NSICLA.GT.1.AND.XMI.LT.-0.5D0*ELAY0) THEN
            WRITE(LU,886)
886         FORMAT(' EROSION GREATER THAN ONE LAYER THICKNESS !')
          ENDIF
          IF(NSICLA.GT.1.AND.XMA.GT.ELAY0) THEN
            WRITE(LU,888)
888         FORMAT(' DEPOSITION MORE THAN ONE LAYER THICKNESS !')
          ENDIF
        ENDIF
      ENDIF
      IF(ENTET) THEN
        CALL MAXI(XMAX,IMAX,CUMBE%R,NPOIN)
        IF(NCSIZE.GT.1) THEN
          XMA=P_MAX(XMAX)
          IF(XMAX.EQ.XMA) THEN
            IMA=MESH%KNOLG%I(IMAX)
          ELSE
            IMA=0
          ENDIF
          IMA=P_MAX(IMA)
        ELSE
          IMA=IMAX
          XMA=XMAX
        ENDIF
        WRITE(LU,882) XMA,IMA
882     FORMAT(' TOTAL MAXIMAL EVOLUTION  : ',G16.7,' NODE  :',I6)
        CALL MINI(XMIN,IMIN,CUMBE%R,NPOIN)
        IF(NCSIZE.GT.1) THEN
          XMI=P_MIN(XMIN)
          IF(XMIN.EQ.XMI) THEN
            IMI=MESH%KNOLG%I(IMIN)
          ELSE
            IMI=0
          ENDIF
          IMI=P_MAX(IMI)
        ELSE
          IMI=IMIN
          XMI=XMIN
        ENDIF
        WRITE(LU,884) XMI,IMI
884     FORMAT(' TOTAL MINIMAL EVOLUTION  : ',G16.7,' NODE  :',I6)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
