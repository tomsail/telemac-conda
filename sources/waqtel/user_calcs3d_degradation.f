!                 ***********************************
                  SUBROUTINE USER_CALCS3D_DEGRADATION
!                 ***********************************
!
     &(NPOIN3,NPOIN2,NPLAN,TN,TEXP,TIMP,Z,NWAQ_DEGRA,RANK_DEGRA,
     & LOITRAC,COEF1TRAC)
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
!| LOITRAC        |-->| LAW OF TRACERS DEGRADATION
!| NPLAN          |-->| NUMBER OF VERTICAL PLANES
!| NPOIN2         |-->| NUMBER OF NODES IN THE 2D MESH
!| NPOIN3         |-->| NUMBER OF NODES IN THE 3D MESH
!| NWAQ_DEGRA     |-->| NUMBER OF TRACERS WITH A DEGRADATION LAW
!| RANK_DEGRA     |-->| GROUP TRACERS WITH A DEGRADATION LAW
!| TEXP           |-->| EXPLICIT SOURCE TERM
!| TIMP           |-->| IMPLICIT SOURCE TERM
!| TN             |-->| TRACERS AT TIME N
!| Z              |-->| Z COORDINATES FOR NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN3,NPOIN2,NPLAN
      TYPE(BIEF_OBJ), INTENT(IN)      :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TEXP,TIMP
      TYPE(BIEF_OBJ), INTENT(IN)      :: Z
      INTEGER, INTENT(IN)             :: NWAQ_DEGRA
      INTEGER, INTENT(IN)             :: LOITRAC(*)
      INTEGER, INTENT(IN)             :: RANK_DEGRA(*)
      DOUBLE PRECISION, INTENT(IN)    :: COEF1TRAC(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
