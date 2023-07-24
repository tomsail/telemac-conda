!                   **************************
                    SUBROUTINE MEAN_GRAIN_SIZE
!                   **************************
!
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    GEOMETRIC MEAN GRAIN SIZES OF ACTIVE-LAYER AND UNDER-LAYER.
!
!history  BUI MINH DUC
!+        **/11/2002
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I,J
!
!-----------------------------------------------------------------------
!
!     UNLADM IS NEEDED FOR HUNZIKER
!
      DO J=1,NPOIN
        ACLADM%R(J) = 0.D0
        UNLADM%R(J) = 0.D0
        IF(NSICLA.GT.1) THEN
          DO I=1,NSICLA
            IF(AVAIL(J,1,I).GT.0.D0) THEN
              ACLADM%R(J) = ACLADM%R(J) + FDM(I)*AVAIL(J,1,I)
              UNLADM%R(J) = UNLADM%R(J) + FDM(I)*AVAIL(J,2,I)
            ENDIF
          ENDDO
        ENDIF
        IF(ACLADM%R(J).LE.0.D0) ACLADM%R(J) = FDM(1)
        IF(UNLADM%R(J).LE.0.D0) UNLADM%R(J) = ACLADM%R(J)
      ENDDO

      CALL USER_MEAN_GRAIN_SIZE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
