!                       *********************
                        SUBROUTINE XY_TOMAWAC
!                       *********************
!
     &( SPEC )
!
!***********************************************************************
! ARTEMIS   V7P2                                     Feb 2017
!***********************************************************************
!
!brief    READS IN THE COORDINATES OF THE OUTER MODEL GRID NODES.
!
!history  N.DURAND (HRW)
!+        March 2001
!+        
!+   Original version (applied to COWADIS)
!
!history  N.DURAND (HRW)
!+        Feb 2017
!+        V7P2
!+   Revisited to use the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SPEC           |<->| SPECTRUM STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SPECTRUM) , INTENT(INOUT) :: SPEC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                      :: IERR, I
      INTEGER                      :: NFIC1, TYP1, NPOIN1
!
      DOUBLE PRECISION,ALLOCATABLE :: X1(:),Y1(:)
!
      CHARACTER(LEN=8)             :: FFORMAT1
!
!-----------------------------------------------------------------------
!
!     FOR THE OUTER TOMAWAC MODEL
      NFIC1 = ART_FILES(WACRES)%LU
      FFORMAT1 = ART_FILES(WACRES)%FMT
      TYP1 = TRIANGLE_ELT_TYPE
!
!-----------------------------------------------------------------------
!
!     NUMBER OF NODES IN THE TOMAWAC OUTER MESH
!
      CALL GET_MESH_NPOIN(FFORMAT1,NFIC1,TYP1,NPOIN1,IERR)
      CALL CHECK_CALL(IERR,'XY_TOMAWAC:GET_MESH_NPOIN')
!     IF(DEBUG.GT.0) WRITE(LU,*) 'XY_TOMAWAC:NPOIN_OUTER:',NPOIN1
!
!     COORDINATES
!
      ALLOCATE(X1(NPOIN1),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'XY_TOMAWAC:X1')
      ALLOCATE(Y1(NPOIN1),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'XY_TOMAWAC:Y1')
      CALL GET_MESH_COORD(FFORMAT1,NFIC1,1,2,NPOIN1,X1,IERR)
      CALL CHECK_CALL(IERR, 'XY_TOMAWAC:GET_MESH_COORD:X_OUTER')
      CALL GET_MESH_COORD(FFORMAT1,NFIC1,2,2,NPOIN1,Y1,IERR)
      CALL CHECK_CALL(IERR, 'XY_TOMAWAC:GET_MESH_COORD:Y_OUTER')
!
!     X AND Y FOR EACH SPECTRUM/POINT IN SPEC
!
      DO I=1,SPEC%N
        SPEC%XOUTER(I) = X1(SPEC%NOUTER(I))
        SPEC%YOUTER(I) = Y1(SPEC%NOUTER(I))
      ENDDO
!     IF(DEBUG.GT.0) WRITE(LU,*) 
!    &                'XY_TOMAWAC:SPEC%XOUTER:',SPEC%XOUTER
!     IF(DEBUG.GT.0) WRITE(LU,*) 
!    &                'XY_TOMAWAC:SPEC%YOUTER:',SPEC%YOUTER
!
      DEALLOCATE(X1,Y1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
