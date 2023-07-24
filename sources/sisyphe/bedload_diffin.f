!                   *************************
                    SUBROUTINE BEDLOAD_DIFFIN
!                   *************************
!
     &(U, V, NBOR, XNEBOR, YNEBOR, MASKEL, NELBOR, NPTFR,
     & KENT, KSORT, KLOG, KDIR, KDDL, KNEU, MSK, CLT, LITBOR,
     & MASKTR, LIMTRA,IKLBOR,NELEB,NELEBX)
!
!***********************************************************************
! SISYPHE   V7P0                                   27/03/2014
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS.
!
!history  FRANCOIS MENARD (PLACEMENT @ LNHE)
!+        17/08/2004
!+        V6P0
!+
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
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/03/2014
!+        V7P0
!+  Adaptation to different numbering of boundary elements.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CLT            |<->| TYPE OF BOUNDARY CONDITIONS FOR TRACER (MODIFIED LITBOR)
!| IKLBOR         |-->| CONNECTIVITY OF BOUNDARY ELEMENTS.
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIMTRA         |<->| TYPE OF BOUNDARY CONDITION FOR TRACER
!| LITBOR         |<->| TYPE OF BOUNDARY CONDITIONS FOR TRACER (***)
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MASKTR         |<->| MASKING FOR TRACERS, PER POINT
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NBOR           |-->| NUMBER OF BOUDARY POINTS
!| NELBOR         |-->| NUMBERS OF ELEMENTS TOUCHING THE BORDER
!| NELEB          |-->| NUMBER OF BOUDARY ELEMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUDARY ELEMENTS
!| NPTFR          |-->| NUMBER OF BOUDARIES
!| U              |-->| FLOW VELOCITY IN THE X DIRECTION
!| V              |-->| FLOW VELOCITY IN THE Y DIRECTION
!| XNEBOR         |-->| X-COORDINATES OF THE BOUNDARY POINT
!| YNEBOR         |-->| Y-COORDINATES OF THE BOUNDARY POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_BEDLOAD_DIFFIN => BEDLOAD_DIFFIN
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: U,V,NBOR,XNEBOR,YNEBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,NELBOR
      INTEGER,        INTENT(IN)    :: NPTFR,KENT,KSORT,KLOG
      INTEGER,        INTENT(IN)    :: KDIR,KDDL,KNEU,NELEB,NELEBX
      INTEGER,        INTENT(IN)    :: IKLBOR(NELEBX,2)
      LOGICAL,        INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CLT
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LITBOR, MASKTR, LIMTRA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
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
      ! ****************************************************** !
      ! I - TYPES OF BOUNDARY CONDITIONS FOR THE TRACER        !
      !     MAY BE MODIFIED DEPENDING ON THE SIGN OF U.N       !
      !     FOR THE LIQUID BOUNDARIES (N : OUTGOING NORMAL)    !
      ! ****************************************************** !
!
      DO K = 1, NPTFR
        CLT%I(K) = LITBOR%I(K)
        ! I.1 - LIQUID BOUNDARIES
        ! --------------------------------------
        IF (CLT%I(K) == KENT) THEN
          USCALN = U%R(NBOR%I(K))*XNEBOR%R(K)
     &           + V%R(NBOR%I(K))*YNEBOR%R(K)
          ! OUTGOING VELOCITY, FREE TRACER
          ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF (USCALN >= 0.D0) CLT%I(K) = KSORT
        ELSEIF(CLT%I(K) == KSORT) THEN
          USCALN = U%R(NBOR%I(K))*XNEBOR%R(K)
     &           + V%R(NBOR%I(K))*YNEBOR%R(K)
          ! ENTERING VELOCITY, FREE TRACER
          ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF (USCALN <= 0.D0) CLT%I(K) = KENT
        ENDIF
      ENDDO
!
      ! **************************************************************** !
      ! II - MASKTR ARRAY DEFINED AS A FUNCTION OF CLT                   !
      !      EQUALS 1 FOR A SEGMENT OF NEUMANN TYPE, AND 0 OTHERWISE     !
      !      A SEGMENT IS OF NEUMANN TYPE IF THE USER SPECIFIES AT LEAST !
      !      ONE OF ITS NODES AS NEUMANN.                                !
      ! **************************************************************** !
!
      CALL OS('X=0     ', X=MASKTR)
!
      DO IELEB = 1 , NELEB
        K1=IKLBOR(IELEB,1)
        K2=IKLBOR(IELEB,2)
        ! II.1 - NEUMANN TYPE SEGMENTS
        ! -------------------------------
        IF(CLT%I(K1).EQ.KLOG.OR.CLT%I(K2).EQ.KLOG) THEN
          MASKTR%ADR(NEU)%P%R(IELEB) = 1.D0
        ! II.2 - OUTGOING TYPE SEGMENTS
        ! ------------------------------
        ELSEIF(CLT%I(K1).EQ.KENT.AND.CLT%I(K2).EQ.KSORT) THEN
          MASKTR%ADR(DDL)%P%R(IELEB) = 1.D0
        ELSEIF (CLT%I(K1).EQ.KSORT.OR.CLT%I(K2).EQ.KSORT) THEN
          MASKTR%ADR(DDL)%P%R(IELEB) = 1.D0
        ! II.3 - OUTGOING TYPE SEGMENTS
        ! ------------------------------
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
      ! *********************** !
      ! III - POTENTIAL MASKING !
      ! *********************** !
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
      ! ************************************************************** !
      ! IV - FROM PHYSICAL CONDITION TO TECHNICAL CONDITIONS           !
      ! ************************************************************** !
!
      DO K = 1, NPTFR
        ! IV.1 - 'INCOMING' BOUNDARY : IMPOSED TRACER
        ! -----------------------------------------
        IF(CLT%I(K).EQ.KENT) THEN
          LIMTRA%I(K) = KDIR
        ELSEIF(CLT%I(K).EQ.KSORT) THEN
          LIMTRA%I(K) = KDDL
        ! IV.2 - SOLID BOUNDARY : NEUMANN CONDITIONS
        ! ------------------------------------
        ELSEIF(CLT%I(K).EQ.KLOG ) THEN
          LIMTRA%I(K) = KNEU
        ! IV.3 - ERROR: UNKNOWN LITBOR VALUE
        ! ----------------------------------------
        ELSE
          WRITE(LU,12) K, LITBOR%I(K)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
      !----------------------------------------------------------------!
102   FORMAT(' BEDLOAD_DIFFIN: UNEXPECTED CASE')
12    FORMAT(' BEDLOAD_DIFFIN: POINT ',1I8,' LITBOR= ',1I8,' ?')
      !----------------------------------------------------------------!
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
