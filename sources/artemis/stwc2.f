!                   ****************
                    SUBROUTINE STWC2
!                   ****************
!
     &(IMIN,IMAX,N,DIR2,SDIR)
!
!***********************************************************************
! ARTEMIS   V7P4                                     Nov 2017
!***********************************************************************
!
!brief    COMPUTES THE ENERGY DENSITY BASED ON A TOMAWAC SPECTRUM.
!
!history  N.DURAND (HRW)
!+        November 2017
!+        V7P4
!+   New. Wrapper around calls to STIRLING and LISSAGE, when linear
!+   interpolation (STWC1) is not sufficient
!
!history  N.DURAND (HRW)
!+        January 2019
!+        V8P0
!+   Added USE BIEF_DEF since TYPE SPECTRUM is now defined in BIEF_DEF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IMIN           |-->| DIRECTION INDEX FOR TETMIN
!| IMAX           |-->| DIRECTION INDEX FOR TETMAX
!| N              |-->| NUMBER OF FINER RESOLUTION POINTS
!| DIR2           |<--| ARTEMIS DIR COMPONENTS
!| SDIR           |<->| ARTEMIS SPECTRUM, INTEGRATED FOR EACH DIR COMPONENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_ARTEMIS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: IMIN,IMAX,N
      DOUBLE PRECISION, INTENT(INOUT) :: SDIR(N),DIR2(N)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                       :: IDD
      DOUBLE PRECISION,ALLOCATABLE  :: SDIR2(:)
      DOUBLE PRECISION              :: DTETA2
!
      INTEGER                       :: LISS
      INTEGER                       :: IERR
!
!-----------------------------------------------------------------------
!
      ALLOCATE(SDIR2(N),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'STWC2:SDIR2')
!
!=======================================================================
!     TOMAWAC SPECTRUM IS GIVEN AT DISCRETE FREQUENCIES AND DIRECTIONS
!     THAT ARE COARSER THAN REQUIRED TO GIVE A SMOOTH ESTIMATE OF ENERGY
!     IN SPECTRUM
!     => REQUIRES INTERPOLATION (WITHIN RANGE) WITH STIRLING,
!        FOLLOWED BY SMOOTHING WITH LISSAGE
!-----------------------------------------------------------------------
!
      DO IDD = 1,N
        DIR2(IDD)  = 0.D0
        SDIR2(IDD) = 0.D0
      ENDDO
!
      IF(IMAX.LT.IMIN) THEN
        DO IDD = IMIN,NDIR
          DIR2(IDD-IMIN+1)  = S_TOM%DIR(IDD)-360.0D0
          SDIR2(IDD-IMIN+1) = SDIR(IDD)
        ENDDO
        DO IDD = 1,IMAX
          DIR2(NDIR-IMIN+1+IDD)  = S_TOM%DIR(IDD)
          SDIR2(NDIR-IMIN+1+IDD) = SDIR(IDD)
        ENDDO
        CALL STIRLING( NDIR-IMIN+1+IMAX, DIR2, SDIR2,
     &                N, DTETA2, SDIR )
!
      ELSE
        DO IDD = IMIN,IMAX
          DIR2(IDD-IMIN+1)  = S_TOM%DIR(IDD)
          SDIR2(IDD-IMIN+1) = SDIR(IDD)
        ENDDO
        CALL STIRLING( IMAX-IMIN+1, DIR2, SDIR2,
     &                N, DTETA2, SDIR )
      ENDIF
!
      DO IDD = 1,N
        DIR2(IDD) = DIR2(1)+FLOAT(IDD-1)*DTETA2
      ENDDO
!
      DO LISS = 1,250
        CALL LISSAGE( N, SDIR, SDIR2)
        CALL OV('X=Y     ', SDIR, SDIR2, SDIR2, 0.D0, N)
      ENDDO
!     DO IDD = 1,N
!       WRITE(LU,*) DIR2(IDD),SDIR(IDD)
!     ENDDO
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(SDIR2)
!
!=======================================================================
!
      RETURN
      END
