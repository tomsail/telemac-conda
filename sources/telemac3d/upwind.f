!                   *****************
                    SUBROUTINE UPWIND
!                   *****************
!
     &(M,WCC,DELTA,MESH2D,MESH3D,NPLAN)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    UPWINDS THE ADVECTION TERM OF VERTICAL VELOCITY.
!code
!+      A DIFFUSION TERM WITH DIFFUSION COEFFICIENT ABS(WCC)*DZ/2
!+      IS ADDED TO THE MATRIX M. FORMULA IS OBTAINED BY SIMPLIFYING
!+      THE Z PART OF DIFFUSION MATRIX BUILT IN SUBROUTINE MT02PP
!+      DZ THEN VANISHES.
!+
!+      THIS IS USED IN DIFF3D FOR SEDIMENT SETTLING VELOCITY AND
!+      FOR VERTICAL UPWINDING IN SUPG METHOD.
!
!history  J.M. HERVOUET  (LNHE)
!+        12/12/05
!+        V5P6
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
!history  J.M. HERVOUET (EDF LAB, LNHE)
!+        15/05/2014
!+        V7P0
!+   Checking that we are duly in prisms.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DELTA          |-->| UPWIND COEFFICIENT (BETWEEN 0 AND 1)
!| M              |<->| MATRIX
!| MESH2D         |-->| 2D MESH
!| MESH3D         |-->| 3D MESH
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| WCC            |-->| VERTICAL VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_UPWIND => UPWIND
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: M
      TYPE(BIEF_OBJ), INTENT(IN)     :: WCC
      DOUBLE PRECISION, INTENT(IN)   :: DELTA
      TYPE(BIEF_MESH), INTENT(IN)    :: MESH3D,MESH2D
      INTEGER, INTENT(IN)            :: NPLAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      IF(M%STO.EQ.1.AND.WCC%ELM.EQ.41) THEN
        CALL UPWINDEBE(M%D%R,M%X%R,MESH3D%IKLE%I,
     &                 MESH3D%NELMAX,
     &                 MESH2D%NELEM,
     &                 MESH2D%SURFAC%R,NPLAN,WCC%R,M%TYPEXT,DELTA)
      ELSEIF(M%STO.EQ.3.AND.WCC%ELM.EQ.41) THEN
        CALL UPWINDSEG(M%D%R,M%X%R,
     &                 MESH3D%IKLE%I,MESH3D%NELMAX,
     &                 MESH2D%NELEM,MESH2D%SURFAC%R,
     &                 NPLAN,WCC%R,MESH2D%NSEG,MESH3D%NSEG,M%TYPEXT,
     &                 DELTA)
      ELSE
        WRITE(LU,*) 'UPWIND: UNEXPECTED STORAGE FOR MATRIX M:',M%STO
        WRITE(LU,*) '        ONLY 1 AND 3 ARE TREATED'
        WRITE(LU,*) 'OR THE MESH HAS ELEMENTS OF TYPE:',WCC%ELM
        WRITE(LU,*) 'IT SHOULD BE 41 (PRISMS)'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
