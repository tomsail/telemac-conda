!                   *******************
                    SUBROUTINE FINDKSCE
!                   *******************
!
     &(NPOIN2,NPLAN,Z,NSCE,ISCE,ZSCE,KSCE,INFO)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FINDS THE CLOSEST GRID POINTS AMONGST THE PLANES
!+                OF THE 3D GRID.
!
!history  C. GUILBAUD
!+        13/10/2000
!+
!+   ORIGINAL
!
!history  J-M HERVOOUET (LNHE)
!+        01/08/2006
!+        V5P7
!+   CORRECTED FOR PARALLELISM
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
!| INFO           |-->| IF YES, LISTING PRINTOUT
!| ISCE           |-->| NODE ADRESSES IN 2D MESH FOR SOURCES
!| KSCE           |<->| NUMBER OF PLANE FOR SOURCES
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| Z              |-->| Z COORDINATES
!| ZSCE           |-->| COORDINATES OF GIVEN POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPOIN2,NPLAN,NSCE
      INTEGER, INTENT(IN)    :: ISCE(NSCE)
      INTEGER, INTENT(INOUT) :: KSCE(NSCE)
!
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: ZSCE(NSCE)
!
      LOGICAL, INTENT(IN) :: INFO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,I
!
      DOUBLE PRECISION DISTANCE,TEMPO
!
      INTRINSIC ABS
!
!-----------------------------------------------------------------------
!
      DO I=1,NSCE
!
        DISTANCE=1.D10
        KSCE(I)=0
!
        IF(ISCE(I).GT.0) THEN
!
          DO K=1,NPLAN
            TEMPO=ABS(ZSCE(I)-Z(ISCE(I),K))
            IF(TEMPO.LT.DISTANCE) THEN
              DISTANCE=TEMPO
              KSCE(I)=K
            ENDIF
          ENDDO
!
!-----------------------------------------------------------------------
!
          IF(INFO) THEN
            WRITE(LU,*) 'SOURCE POINT ',I,' PUT ON PLANE ',KSCE(I)
            WRITE(LU,*) 'LOCATED AT ',DISTANCE,' METRES'
          ENDIF
!
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
