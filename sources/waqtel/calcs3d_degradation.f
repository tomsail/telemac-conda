!                   ******************************
                    SUBROUTINE CALCS3D_DEGRADATION
!                   ******************************
!
     &(NPOIN3,NPOIN2,NPLAN,TN,TEXP,TIMP,Z,NWAQ_DEGRA,RANK_DEGRA,
     & LOITRAC,COEF1TRAC)
!
!***********************************************************************
! WAQTEL   V8P2
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR DEGRADATION LAWS PROCESSES
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
      INTEGER J,ITRAC
!
!-----------------------------------------------------------------------
!
      DO J = 1,NWAQ_DEGRA
        ITRAC = RANK_DEGRA(J)
        IF(LOITRAC(ITRAC).EQ.1) THEN
          CALL OS('X=X+C   ',X=TIMP%ADR(ITRAC)%P,
     &            C=2.3D0/COEF1TRAC(ITRAC)/3600.D0)
        ELSEIF(LOITRAC(ITRAC).EQ.2) THEN
          CALL OS('X=X+C   ',X=TIMP%ADR(ITRAC)%P,
     &            C=COEF1TRAC(ITRAC)/3600.D0)
        ELSEIF(LOITRAC(ITRAC).EQ.3) THEN
          CALL OS('X=X+C   ',X=TIMP%ADR(ITRAC)%P,
     &            C=COEF1TRAC(ITRAC)/86400.D0)
        ELSEIF(LOITRAC(ITRAC).EQ.4) THEN
          CALL USER_CALCS3D_DEGRADATION(NPOIN3,NPOIN2,NPLAN,TN,TEXP,
     &              TIMP,Z,NWAQ_DEGRA,RANK_DEGRA,LOITRAC,
     &              COEF1TRAC)
        ELSEIF(LOITRAC(ITRAC).GT.4) THEN
          WRITE(LU,21) LOITRAC(ITRAC),ITRAC
21    FORMAT(1X,'LOITRAC ',I4,' FOR TRACER ',I4,' NOT IMPLEMENTED YET')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
