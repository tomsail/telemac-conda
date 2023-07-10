!                   *****************
                    SUBROUTINE PREDES
!                   *****************
!
     &(LLT,AAT,YAGOUT,CODE)
!
!***********************************************************************
! SISYPHE   V6P2                                   01/07/2012
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+
!+
!
!history  JMH
!+        07/12/2009
!+        V6P0
!+   KS SET TO 0 IF LLT=0
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
!history  P. TASSI (EDF)
!+        01/07/2012
!+        V6P2
!+   Bug correction discharge along y
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AAT            |-->| CURRENT TIME (FOR BUILDING SOLUTIONS)
!| CODE           |-->| NAME OF CALLING PROGRAMME (TELEMAC2D OR 3D)
!| LLT            |-->| LOCAL LT (MAY BE LT-1+PERCOU)
!| YAGOUT         |-->| LOGICAL: IF YES GRAPHIC OUTPUT ANYWAY
!|                |   | (STEERED BY T2D)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: LLT
      DOUBLE PRECISION , INTENT(IN) :: AAT
      CHARACTER(LEN=24), INTENT(IN) :: CODE
      LOGICAL          , INTENT(IN) :: YAGOUT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LTT,I,J
      LOGICAL IMP,LEO
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN DESIMP (BIEF LIBRARY)
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LLT/LISPR)*LISPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIL) IMP=.TRUE.
      LTT=(LLT/LEOPR)*LEOPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIG) LEO=.TRUE.
!     IF CODE =TELEMAC2D OUTOUT IS MANAGED BY T2D
      IF(CODE(8:9).EQ.'2D')LEO=YAGOUT
!
!     NO PRINTOUTS REUIRED: LEAVING
      IF (.NOT.(LEO.OR.IMP)) GO TO 1000
!
!=======================================================================
!     COMPUTES SECONDARY VARIABLES
!=======================================================================
!
!     FREE SURFACE: H+ZF
!
      IF((LEO.AND.SORLEO(4)).OR.(IMP.AND.SORIMP(4))) THEN
        CALL OS('X=Y+Z   ',X=Z,Y=HN,Z=ZF)
      ENDIF
!
!     DISCHARGE
!
      IF((LEO.AND.SORLEO(6)).OR.(IMP.AND.SORIMP(6))) THEN
        DO I=1,NPOIN
          Q%R(I)=HN%R(I)*SQRT(U2D%R(I)**2+V2D%R(I)**2)
        ENDDO
      ENDIF
!
!     DISCHARGE ALONG X
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL OS('X=YZ    ',X=QU,Y=U2D,Z=HN)
      ENDIF
!
!     DISCHARGE ALONG Y
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        CALL OS('X=YZ    ',X=QV,Y=V2D,Z=HN)
      ENDIF
!
!=======================================================================
!
!     VARIABLES WHICH ARE NOT INITIALISED AT THE FIRST CALL OF PREDES
!
      IF(LLT.EQ.0) THEN
        IF((LEO.AND.SORLEO(19)).OR.(IMP.AND.SORIMP(19))) THEN
          CALL OS('X=0     ',X=KS)
        ENDIF
      ENDIF
!
!=======================================================================
! UPDATE THE POINTERS TO THE DIFFERENTIATED VARIABLES
!=======================================================================
!
!     TODO: TRY NOT USING THE HARDCODED NUMBER 28
!
      J = 28+MAX(4,NPRIV)+NSICLA*(NOMBLAY+4)+2*NOMBLAY+VARCL%N
      DO I = 1,NADVAR
        IF((LEO.AND.SORLEO(J)).OR.(IMP.AND.SORIMP(J))) THEN
          CALL AD_GET_SISYPHE(I,ADVAR%ADR(I)%P)
          J = J + 1
        ENDIF
      ENDDO
!
!=======================================================================
!
      ! USER FUNCTION
      CALL USER_PREDES(LLT,AAT,YAGOUT,CODE,LEO,IMP)
1000  CONTINUE
!
!=======================================================================
!
      RETURN
      END
