!                   ******************************
                    SUBROUTINE BEDLOAD_NERBED_VF_GAIA
!                   ******************************
!
     &(MESH,LIEBOR,KSORT,V2DPAR,QSX,QSY,NPOIN,NSEG,
     & NPTFR,DT,QS,T1,T2,T3,BREACH,NUBO,VNOIN,MASS_SAND)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief    NON ERODABLE METHOD FOR FINITE VOLUMES.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] BREACH    Indicator for non erodible bed (finite volumes schemes)
!>@param[in]     DT        Time step
!>@param[in]     KSORT     Convention for free output
!>@param[in,out] LIEBOR    Physical boundary conditions for bed evolution
!>@param[in,out] MESH      Mesh structure
!>@param[in]     NPOIN     Number of points
!>@param[in]     NPTFR     Number of boundary points
!>@param[in]     NSEG      Number of segments per control section
!>@param[in]     NUBO      Global number of edge extremities
!>@param[in,out] QS        Bedload transport rate
!>@param[in]     QSX       Solid discharge x
!>@param[in]     QSY       Solid discharge y
!>@param[in,out] T1 WORK   Bief_obj structure
!>@param[in,out] T2 WORK   Bief_obj structure
!>@param[in,out] T3 WORK   Bief_obj structure
!>@param[in]     V2DPAR    Integral of test functions, assembled in parallel
!>@param[in]     VNOIN     Outward unit normals
!>@param[in]     MASS_SAND Mass sand of the active layer
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_BEDLOAD_NERBED_VF => BEDLOAD_NERBED_VF_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
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
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I, K
      INTEGER          :: IEL, IEL1, IEL2, ISEGIN
      DOUBLE PRECISION :: QSP1, QSP2, QSPC
      DOUBLE PRECISION :: XN, YN, TEMP,PROD_SCAL
      DOUBLE PRECISION :: VNOIN1, VNOIN2, RNORM
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ! ****************** !
      ! I - INITIALISES    !
      ! ****************** !
!
      ! BREACH INDICATES IF NON ERODABLE BED WILL BE REACHED
      ! DURING TIME STEP FOR THIS POINT
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     GIVE T1 AND T2 THE SAME STRUCTURE AS QS
!
      CALL CPSTVC(QS,T1)
      CALL CPSTVC(QS,T2)
!
      DO IEL = 1, NPOIN
        BREACH%I(IEL) = 0
        T1%R(IEL)=0.D0
        T2%R(IEL)=0.D0
      ENDDO
!
      ! ************************************************* !
      ! II - DETERMINES THE OUTGOING FLUX FOR EACH CELL   ! (_IMP_)
      ! ************************************************* !
      ! THE PRINCIPLE IS THAT QS IS CALCULATED FOR EACH SEGMENT AS
      ! HALF THE SUM OF THE QS AT THE CENTERS OF THE ELEMENTS WHICH
      ! SEGMENT FORMS THE BOUNDARY. IT IS THEN PROJECTED ON THE NORMAL
      ! TO THE SEGMENT, MULTIPLIED BY THE LENGTH OF THE SEGMENT, AND
      ! THIS FLUX TERM IS ADDED (OR SUBTRACTED) TO THE TWO ELEMENTS.
!
      DO ISEGIN = 1, NSEG
!
        IEL1 = NUBO(1,ISEGIN)
        IEL2 = NUBO(2,ISEGIN)
!
        ! II.1 - SEGMENT LENGTH (RNORM)
        ! ----------------------------------
        VNOIN1 = VNOIN(1,ISEGIN)
        VNOIN2 = VNOIN(2,ISEGIN)
        RNORM  = VNOIN(3,ISEGIN)
        PROD_SCAL= (MESH%X%R(IEL2)-MESH%X%R(IEL1))*VNOIN1+
     &             (MESH%Y%R(IEL2)-MESH%Y%R(IEL1))*VNOIN2
        IF(PROD_SCAL.LT.0.D0)THEN
          IEL1 = NUBO(2,ISEGIN)
          IEL2 = NUBO(1,ISEGIN)
        ENDIF
!
        ! II.2 - PROJECTS QS FOR THE SEGMENT ONTO THE SEGMENT NORMAL
        ! ------------------------------------------------------------
        QSP1 = VNOIN1*QSX%R(IEL1) + VNOIN2*QSY%R(IEL1)
        QSP2 = VNOIN1*QSX%R(IEL2) + VNOIN2*QSY%R(IEL2)
        QSPC = (QSP1+QSP2)*0.5D0
!
        ! II.3 - QS SUCH AS THE OUTGOING FLUX IS MAXIMUM
        ! ----------------------------------------------
        T1%R(IEL1) = T1%R(IEL1) + RNORM*MAX(QSPC,QSP1,0.D0)
        T1%R(IEL2) = T1%R(IEL2) - RNORM*MIN(QSPC,QSP2,0.D0)
!
        IF(QSPC > 0.D0) THEN
          T2%R(IEL1) = T2%R(IEL1) + RNORM*QSP1
        ELSEIF(QSPC < 0.D0) THEN
          T2%R(IEL2) = T2%R(IEL2) - RNORM*QSP2
        ENDIF
!
      ENDDO
!
      ! ************************************** !
      ! III - LOOP ON THE BOUNDARY NODES       !
      ! ************************************** !
!
      DO K = 1, NPTFR
        IEL = MESH%NBOR%I(K)
!
        ! III.1 - FREE EVOLUTION: SEDIMENTS ARE FREE TO LEAVE
        ! ---------------------------------------------------------
        IF (LIEBOR%I(K) == KSORT) THEN
!
          ! XNEBOR (*+NPTFR) AND YNEBOR (*+NPTFR)
          ! CONTAIN THE VECTOR NORMAL TO A BOUNDARY NODE
          ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          XN   = MESH%XNEBOR%R(K+NPTFR)
          YN   = MESH%YNEBOR%R(K+NPTFR)
          TEMP = QSX%R(IEL)*XN + QSY%R(IEL)*YN
          IF (TEMP > 0.D0) THEN
            T1%R(IEL) = T1%R(IEL) + TEMP
            T2%R(IEL) = T2%R(IEL) + TEMP
          ENDIF
!
        ENDIF
!
        ! III.2 - FOR A SOLID BOUNDARY: NOTHING TO PROGRAM
        !         BECAUSE THE SEDIMENT FLUX IS ZERO HERE
        !         FOR IMPOSED EVOLUTION : SEE BEDLOAD_SOLVS_VF_GAIA.F
        ! --------------------------------------------------------
      ENDDO
!
      IF(NCSIZE > 1) THEN
        CALL PARCOM(T1, 2, MESH)
        CALL PARCOM(T2, 2, MESH)
      ENDIF
!
      ! ************************************************ !
      ! IV - COMPUTES THE MAXIMUM FLUX AUTHORISED PER CELL!
      ! ************************************************ !
!
      DO I = 1, NPOIN
!
        T3%R(I)=MASS_SAND(I)*V2DPAR(I)/DT
        IF (T3%R(I) < 0.D0) T3%R(I) = 0.D0
!
        ! IF THE OUTGOING FLUX IS TOO LARGE, QS IS CAPPED AT THE NODE
        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        IF(T1%R(I) > T3%R(I)) THEN
          BREACH%I(I) = 1
          IF(T2%R(I) > T3%R(I)) THEN
            QS%R(I) = QS%R(I)*T3%R(I)/T2%R(I)
          ENDIF
        ENDIF
!
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END

