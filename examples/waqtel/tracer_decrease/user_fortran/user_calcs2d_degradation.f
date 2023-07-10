!                 ***********************************
                  SUBROUTINE USER_CALCS2D_DEGRADATION
!                 ***********************************
!
     &(NPOIN,TN,TEXP,TIMP,HPROP,NWAQ_DEGRA,RANK_DEGRA,LOITRAC,
     & COEF1TRAC)
!
!***********************************************************************
! WAQTEL   V8P2
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR DEGRADATION LAWS PROCESSES
!         IMPLEMENTATION DONE BY THE USER
!
!history  C.-T. PHAM
!+        26/07/2020
!+        V8P2
!+        Creation from SOURCE_WAQ
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COEF1TRAC      |-->| COEFFICIENT 1 FOR LAW OF TRACERS DEGRADATION
!| HPROP          |-->| PROPAGATION DEPTH
!| LOITRAC        |-->| LAW OF TRACERS DEGRADATION
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!| NWAQ_DEGRA     |-->| NUMBER OF TRACERS WITH A DEGRADATION LAW
!| RANK_DEGRA     |-->| GROUP TRACERS WITH A DEGRADATION LAW
!| TEXP           |-->| EXPLICIT SOURCE TERM
!| TIMP           |-->| IMPLICIT SOURCE TERM
!| TN             |-->| TRACERS AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN
      TYPE(BIEF_OBJ), INTENT(IN)      :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TEXP,TIMP
      TYPE(BIEF_OBJ), INTENT(IN)      :: HPROP
      INTEGER, INTENT(IN)             :: NWAQ_DEGRA
      INTEGER, INTENT(IN)             :: LOITRAC(*)
      INTEGER, INTENT(IN)             :: RANK_DEGRA(*)
      DOUBLE PRECISION, INTENT(IN)    :: COEF1TRAC(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER J,ITRAC
!
!-----------------------------------------------------------------------
!
      DO J = 1,NWAQ_DEGRA
        ITRAC = RANK_DEGRA(J)
        IF(LOITRAC(ITRAC).EQ.4) THEN
          CALL OS('X=X+CY  ',X=TIMP%ADR(ITRAC)%P,
     &            Y=HPROP,C=-COEF1TRAC(ITRAC))
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
