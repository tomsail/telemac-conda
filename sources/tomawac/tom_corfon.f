!                   *********************
                    SUBROUTINE TOM_CORFON
!                   *********************
!
!
!***********************************************************************
! TOMAWAC   V6P1                                   14/06/2011
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!history  F. MARCOS
!+
!+
!+
!
!history  OPTIMER (    )
!+        12/01/2001
!+
!+   TOMAWAC/COWADIS MERGE
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_TOM_CORFON => TOM_CORFON
      IMPLICIT NONE
!
!
!
!-----------------------------------------------------------------------
!
!  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
!
      IF(LISFON.GT.0) THEN
!
        CALL FILTER(SZF,.TRUE.,ST1,ST2,AM1,'MATMAS          ',
     &          1.D0,ST1,ST1,ST1,ST1,ST1,ST1,MESH,.FALSE.,ST1,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(LISFON.EQ.0) THEN
        WRITE(LU,*)
        WRITE(LU,*) 'TOM_CORFON : NO MODIFICATION OF BOTTOM'
        WRITE(LU,*)
      ELSE
        WRITE(LU,*)
        WRITE(LU,*) 'TOM_CORFON : ',LISFON,' BOTTOM SMOOTHINGS'
        WRITE(LU,*)
      ENDIF
      ! USER FUNCTION
      CALL USER_TOM_CORFON
!
!-----------------------------------------------------------------------
!
      RETURN
      END
