!                   *******************************
                    SUBROUTINE SUSPENSION_LISTING !
!                   *******************************
!
     &(MESH,CST,ZFCL_S,UCONV,VCONV,MASKEL,IELMT,DT,MSK,T1)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    WRITES OUT MIN/MAX VALUES.
!
!history  F. HUVELIN
!+        22/12/04
!+        V5P8
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
!+   Name of variables
!+
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CST            |<->| CONCENTRATION AT TIME T(N+1)
!| DT             |-->| TIME STEP IN SECONDS
!| IELMT          |-->| NUMBER OF ELEMENTS
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| UCONV          |<->| X-COMPONENT ADVECTION FIELD (TELEMAC)
!| VCONV          |<->| Y-COMPONENT ADVECTION FIELD
!| ZFCL_S         |<->| BED EVOLUTION PER CLASS, DUE TO SUSPENDED SEDIMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_SUSPENSION_LISTING => SUSPENSION_LISTING
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MIN,P_MAX
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CST,ZFCL_S
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UCONV,VCONV,MASKEL
      INTEGER,          INTENT(IN)    :: IELMT
      DOUBLE PRECISION, INTENT(IN)    :: DT
      LOGICAL,          INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: IMAX,IMA
      DOUBLE PRECISION :: XMAX,XMA
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      CALL MAXI(XMAX,IMAX,CST%R,MESH%NPOIN)
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
      WRITE(LU,510) XMA, IMA
      CALL MINI(XMAX, IMAX, CST%R, MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_MIN(XMAX)
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
      WRITE(LU,511) XMA, IMA
      CALL MAXI(XMAX, IMAX, ZFCL_S%R, MESH%NPOIN)
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
      WRITE(LU,512) XMA, IMA
      CALL MINI(XMAX, IMAX, ZFCL_S%R, MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_MIN(XMAX)
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
      WRITE(LU,513) XMA, IMA
!
      CALL CFLPSI(T1, UCONV, VCONV, DT, IELMT, MESH, MSK, MASKEL)
      CALL MAXI(XMAX, IMAX, T1%R, MESH%NPOIN)
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
      WRITE(LU,517) XMA,IMA
      !----------------------------------------------------------------!
510   FORMAT(' MAXIMAL CONCENTRATION    : ',G16.7,' %, NODE = ',1I8)
511   FORMAT(' MINIMAL CONCENTRATION    : ',G16.7,' %, NODE = ',1I8)
512   FORMAT(' MAXIMAL EVOLUTION        : ',G16.7,'  , NODE = ',1I8)
513   FORMAT(' MINIMAL EVOLUTION        : ',G16.7,'  , NODE = ',1I8)
517   FORMAT(' MAX. CFL FOR SUSPENSION  : ',G16.7,'  , NODE = ',1I8)
      !----------------------------------------------------------------!
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE SUSPENSION_LISTING
