!                 ******************************
                  SUBROUTINE BEDLOAD_DIFFIN_GAIA
!                 ******************************
!
     &(U2D, V2D, NBOR, XNEBOR, YNEBOR, MASKEL, NELBOR, NPTFR,
     & KENT, KSORT, KLOG, KDIR, KDDL, KNEU, MSK, CLT, LITBOR,
     & MASKTR, LIMTRA,IKLBOR,NELEB,NELEBX)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Initialises the boundary conditions.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     U2D    Mean flow velocity x-direction
!>@param[in]     V2D    Mean flow velocity y-direction
!>@param[in]     NBOR   Number of boudary points
!>@param[in]     XNEBOR X-coordinates of the boundary point
!>@param[in]     YNEBOR Y-coordinates of the boundary point
!>@param[in]     MASKEL Masking of elements
!>@param[in]     NELBOR Numbers of elements touching the border
!>@param[in]     NPTFR  Number of boudaries
!>@param[in]     KENT   Convention for liquid input with prescribed value
!>@param[in]     KSORT  Convention for free output
!>@param[in]     KLOG   Convention for solid boundary
!>@param[in]     KDIR   Convention for dirichlet point
!>@param[in]     KDDL   Convention for degree of freedom
!>@param[in]     KNEU   Convention for neumann condition
!>@param[in]     MSK    If yes, there is masked elements
!>@param[in,out] CLT    Type of boundary conditions for tracer
!>@param[in,out] LITBOR Type of boundary conditions for tracer (***)
!>@param[in,out] MASKTR Masking for tracers, per point
!>@param[in,out] LIMTRA Type of boundary condition for tracer
!>@param[in]     IKLBOR Connectivity of boundary elements.
!>@param[in]     NELEB  Number of boudary elements
!>@param[in]     NELEBX Maximum number of boudary elements
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_BEDLOAD_DIFFIN => BEDLOAD_DIFFIN_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: U2D,V2D,NBOR,XNEBOR,YNEBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,NELBOR
      INTEGER,        INTENT(IN)    :: NPTFR,KENT,KSORT,KLOG
      INTEGER,        INTENT(IN)    :: KDIR,KDDL,KNEU,NELEB,NELEBX
      INTEGER,        INTENT(IN)    :: IKLBOR(NELEBX,2)
      LOGICAL,        INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CLT
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LITBOR, MASKTR, LIMTRA
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: K,K1,K2,IELEB
      DOUBLE PRECISION   :: USCALN,C
      INTEGER, PARAMETER :: DIR = 1
      INTEGER, PARAMETER :: DDL = 2
      INTEGER, PARAMETER :: NEU = 3
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     ******************************************************
!     I - TYPES OF BOUNDARY CONDITIONS FOR THE TRACER
!         MAY BE MODIFIED DEPENDING ON THE SIGN OF U.N
!         FOR THE LIQUID BOUNDARIES (N : OUTGOING NORMAL)
!     ******************************************************
!
      DO K = 1, NPTFR
        CLT%I(K) = LITBOR%I(K)
!       I.1 - LIQUID BOUNDARIES
!       --------------------------------------
        IF (CLT%I(K) == KENT) THEN
          USCALN = U2D%R(NBOR%I(K))*XNEBOR%R(K)
     &           + V2D%R(NBOR%I(K))*YNEBOR%R(K)
!         OUTGOING VELOCITY, FREE TRACER
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF (USCALN >= 0.D0) CLT%I(K) = KSORT
        ELSEIF(CLT%I(K) == KSORT) THEN
          USCALN = U2D%R(NBOR%I(K))*XNEBOR%R(K)
     &           + V2D%R(NBOR%I(K))*YNEBOR%R(K)
!         ENTERING VELOCITY, FREE TRACER
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF (USCALN <= 0.D0) CLT%I(K) = KENT
        ENDIF
      ENDDO
!
!     ****************************************************************
!     II - MASKTR ARRAY DEFINED AS A FUNCTION OF CLT
!          EQUALS 1 FOR A SEGMENT OF NEUMANN TYPE, AND 0 OTHERWISE
!          A SEGMENT IS OF NEUMANN TYPE IF THE USER SPECIFIES AT LEAST
!          ONE OF ITS NODES AS NEUMANN.
!     ****************************************************************
!
      CALL OS('X=0     ', X=MASKTR)
!
      DO IELEB = 1 , NELEB
        K1=IKLBOR(IELEB,1)
        K2=IKLBOR(IELEB,2)
!       II.1 - NEUMANN TYPE SEGMENTS
!       -------------------------------
        IF(CLT%I(K1).EQ.KLOG.OR.CLT%I(K2).EQ.KLOG) THEN
          MASKTR%ADR(NEU)%P%R(IELEB) = 1.D0
!       II.2 - OUTGOING TYPE SEGMENTS
!       ------------------------------
        ELSEIF(CLT%I(K1).EQ.KENT.AND.CLT%I(K2).EQ.KSORT) THEN
          MASKTR%ADR(DDL)%P%R(IELEB) = 1.D0
        ELSEIF (CLT%I(K1).EQ.KSORT.OR.CLT%I(K2).EQ.KSORT) THEN
          MASKTR%ADR(DDL)%P%R(IELEB) = 1.D0
!       II.3 - OUTGOING TYPE SEGMENTS
!       ------------------------------
        ELSEIF(CLT%I(K1).EQ.KSORT.AND.CLT%I(K2).EQ.KENT) THEN
          MASKTR%ADR(DDL)%P%R(IELEB) = 1.D0
        ELSEIF(CLT%I(K1).EQ.KENT.OR.CLT%I(K2).EQ.KENT) THEN
          MASKTR%ADR(DIR)%P%R(IELEB) = 1.D0
        ELSE
          WRITE(LU,102)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!     ***********************
!     III - POTENTIAL MASKING
!     ***********************
!
      IF(MSK) THEN
        DO IELEB = 1 , NELEB
          C=MASKEL%R(NELBOR%I(IELEB))
          MASKTR%ADR(DIR)%P%R(IELEB) = MASKTR%ADR(DIR)%P%R(IELEB)*C
          MASKTR%ADR(DDL)%P%R(IELEB) = MASKTR%ADR(DDL)%P%R(IELEB)*C
          MASKTR%ADR(NEU)%P%R(IELEB) = MASKTR%ADR(NEU)%P%R(IELEB)*C
        ENDDO
      ENDIF
!
!     **************************************************************
!     IV - FROM PHYSICAL CONDITION TO TECHNICAL CONDITIONS
!     **************************************************************
!
      DO K = 1, NPTFR
!       IV.1 - 'INCOMING' BOUNDARY : IMPOSED TRACER
!       -----------------------------------------
        IF(CLT%I(K).EQ.KENT) THEN
          LIMTRA%I(K) = KDIR
        ELSEIF(CLT%I(K).EQ.KSORT) THEN
          LIMTRA%I(K) = KDDL
!       IV.2 - SOLID BOUNDARY : NEUMANN CONDITIONS
!       ------------------------------------
        ELSEIF(CLT%I(K).EQ.KLOG ) THEN
          LIMTRA%I(K) = KNEU
!       IV.3 - ERROR: UNKNOWN LITBOR VALUE
!       ----------------------------------------
        ELSE
          WRITE(LU,12) K, LITBOR%I(K)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!-------------------------------------------------------------------!
!-------------------------------------------------------------------!
102   FORMAT(' BEDLOAD_DIFFIN_GAIA: UNEXPECTED CASE')
12    FORMAT(' BEDLOAD_DIFFIN_GAIA: POINT ',1I8,' LITBOR= ',1I8,' ?')
!-------------------------------------------------------------------!
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
