!                   ***********************
                    SUBROUTINE TRANSF_ZCHAR
!                   ***********************
!
     &(TRANSF,ZCHAR,ZSTAR,TRANSF_PLANE,NPLAN)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DETERMINES THE TYPE OF TRANSFORMATION.
!+
!+            BUILDS THE REDUCED COORDINATES FOR
!+                THE METHOD OF CHARACTERISTICS.
!
!history  J-M HERVOUET (LNHE)
!+        11/03/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
!| TRANSF         |<->| TYPE OF MESH TRANSFORMATION
!| TRANSF_PLANE   |<->| TYPE OF MESH TRANSFORMATION FOR EACH PLANE
!| ZCHAR          |<->| COORDINATES IN THE TRANSFORMED MESH
!|                |   | FOR THE METHOD OF CHARACTERISTICS
!| ZSTAR          |-->| PERCENTUAL MESH PLANES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)           :: NPLAN
      INTEGER, INTENT(IN)           :: TRANSF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: ZCHAR,TRANSF_PLANE
      TYPE(BIEF_OBJ), INTENT(IN)    :: ZSTAR
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN,ITRA
!
!***********************************************************************
!
!     DETERMINES THE TYPE OF TRANSFORMATION
!
!     1: CLASSICAL SIGMA TRANSFORMATION
!     2: SIGMA TRANSFORMATION WITH GIVEN PROPORTIONS
!     3: GENERALISED SIGMA TRANSFORMATION
!
      IF(TRANSF.NE.0.AND.TRANSF.NE.5) THEN
        ITRA=1
        DO IPLAN=2,NPLAN
          IF(TRANSF_PLANE%I(IPLAN).EQ.2) THEN
            ITRA=2
          ENDIF
        ENDDO
!       IF ANY PLANE WITH GIVEN ELEVATION
!       GENERALISED SIGMA TRANSFORMATION
        DO IPLAN=2,NPLAN
          IF(TRANSF_PLANE%I(IPLAN).EQ.3) ITRA=3
        ENDDO
        IF(ITRA.NE.TRANSF) THEN
          WRITE(LU,*) 'TRANSF_ZCHAR:'
          WRITE(LU,*) 'THE KEYWORD MESH TRANSFORMATION'
          WRITE(LU,*) 'SHOULD BE EQUAL TO ',ITRA,TRANSF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     BUILDS ZCHAR (USED ONLY BY CHARACTERISTICS)
!     ZCHAR IS THE VERTICAL COORDINATE OF THE TRANSFORMED MESH
!
      IF(TRANSF.EQ.0.OR.TRANSF.EQ.1.OR.TRANSF.EQ.3.OR.TRANSF.EQ.5) THEN
        DO IPLAN = 1,NPLAN
          ZCHAR%R(IPLAN) = DBLE(IPLAN-1)/DBLE(NPLAN-1)
        ENDDO
      ELSEIF(TRANSF.EQ.2) THEN
        DO IPLAN = 1,NPLAN
          IF(TRANSF_PLANE%I(IPLAN).EQ.1) THEN
            ZCHAR%R(IPLAN) = DBLE(IPLAN-1)/DBLE(NPLAN-1)
          ELSE
            ZCHAR%R(IPLAN) = ZSTAR%R(IPLAN)
!           SECURITY
            IF(IPLAN.EQ.1)     ZCHAR%R(IPLAN)=0.D0
            IF(IPLAN.EQ.NPLAN) ZCHAR%R(IPLAN)=1.D0
          ENDIF
        ENDDO
      ELSE
        WRITE(LU,*) 'WRONG TRANSFORMATION IN TRANSF_ZCHAR'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
