!                   ******************************
                    SUBROUTINE BEDLOAD_NERBED_VF !
!                   ******************************
!
     &(MESH,LIEBOR,KSORT,ELAY,V2DPAR,QSX,QSY,AVA,NPOIN,NSEG,NPTFR,
     & DT,QS,T1,T2,T3,BREACH,CSF_SABLE,NUBO,VNOIN)
!
!***********************************************************************
! SISYPHE   V7P0                                   03/06/2014
!***********************************************************************
!
!brief    NON ERODABLE METHOD FOR FINITE VOLUMES.
!
!history  M. GONZALES DE LINARES
!+        07/05/2002
!+        V5P3
!+  First version.
!
!history  J-M HERVOUET (EDF,LNHE)
!+        31/01/2008
!+        V6P0
!+   CORRECTED INITIALISATION ERROR FOR T1 AND T2
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
!+  Names of variables .
!+
!history  R.ATA (EDF-LNHE)
!+        02/06/2014
!+        V7P0
!+  Corrections of normals and nubo tables
!+  after changes in FV data structure of Telemac-2D.
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AVA            |-->| PERCENT AVAILABLE
!| BREACH         |<->| INDICATOR FOR NON ERODIBLE BED (FINITE VOLUMES SCHEMES)
!| DT             |-->| TIME STEP
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIEBOR         |<->| PHYSICAL BOUNDARY CONDITIONS FOR BED EVOLUTION
!| MESH           |<->| MESH STRUCTURE
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS PER CONTROL SECTION
!| NUBO           |-->| GLOBAL NUMBER OF EDGE EXTREMITIES
!| QS             |<->| BEDLOAD TRANSPORT RATE
!| QSX            |-->| SOLID DISCHARGE X
!| QSY            |-->| SOLID DISCHARGE Y
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| VNOIN          |-->| OUTWARD UNIT NORMALS
!| CSF_SABLE      |-->| VOLUME CONCENTRATION OF SAND (1-POROSITY)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_BEDLOAD_NERBED_VF => BEDLOAD_NERBED_VF
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: LIEBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: QSX, QSY
      INTEGER,          INTENT(IN)    :: NPOIN, NSEG, NPTFR,KSORT
      DOUBLE PRECISION, INTENT(IN)    :: DT
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS, T1, T2, T3
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH
      DOUBLE PRECISION, INTENT(IN)    :: ELAY(NPOIN),V2DPAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AVA(NPOIN), CSF_SABLE
! RA
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: VNOIN(3,NSEG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
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
        !         FOR IMPOSED EVOLUTION : SEE BEDLOAD_SOLVS_VF.F
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
        T3%R(I)=ELAY(I)*V2DPAR(I)*AVA(I)* CSF_SABLE/DT
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

