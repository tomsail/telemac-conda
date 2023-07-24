!                   *****************
                    SUBROUTINE EXTMSK
!                   *****************
!
     &(MASKBR,MASK,NETAGE,NELEB)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    EXTRUDES THE 2D MASK ON THE VERTICAL FOR LATERAL
!+                BOUNDARIES.
!
!history  J.M. HERVOUET  (LNH)
!+        23/12/2003
!+        V5P5
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Boundary segments have now their own numbering, independent of
!+   boundary points numbering.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MASK           |-->| 2D MASK
!| MASKBR         |<->| 3D MASK ON LATERAL BOUNDARIES
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!| NETAGE         |-->| NUMBER OF PLANES - 1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NETAGE,NELEB
      DOUBLE PRECISION, INTENT(IN)  :: MASK(*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKBR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IETAGE,IELEB
!
!=======================================================================
!
!=======================================================================
!
      IF(MASKBR%ELM.EQ.70) THEN
!
!       QUADRILATERAL ON THE LATERAL BOUNDARIES
!
        DO IELEB = 1,NELEB
          DO IETAGE = 1,NETAGE
            MASKBR%R((IETAGE-1)*NELEB+IELEB)=MASK(IELEB)
          ENDDO
        ENDDO
!
      ELSEIF(MASKBR%ELM.EQ.60) THEN
!
!       TRIANGLES ON THE LATERAL BOUNDARIES
!
        DO IELEB = 1,NELEB
          DO IETAGE = 1,NETAGE
            MASKBR%R((IETAGE-1)*2*NELEB+IELEB      )=MASK(IELEB)
            MASKBR%R((IETAGE-1)*2*NELEB+IELEB+NELEB)=MASK(IELEB)
          ENDDO
        ENDDO
!
      ELSE
!
        WRITE(LU,*) 'UNKNOWN ELEMENT FOR MASKBR IN EXTMSK'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
