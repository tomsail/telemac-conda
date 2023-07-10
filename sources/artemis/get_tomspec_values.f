!                   *****************************
                    SUBROUTINE GET_TOMSPEC_VALUES
!                   *****************************
!
     &(CHAINTWC,SPEC)
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    WRAPPER TO READ IN THE TOMAWAC ENERGY SPECTRA.
!
!history  N.DURAND (HRW)
!+        Feb 2017
!+        V7P2
!+   This is a wrapper for call to GET_TOMSPEC_VALUE1 or GET_TOMSPEC_VALUE2
!+   (eventually) depending on choice of nesting: CHAINTWC
!
!history  N.DURAND (HRW)
!+        January 2019
!+        V8P0
!+   Added USE BIEF_DEF since TYPE SPECTRUM is now defined in BIEF_DEF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHAINTWC       |-->| NESTING OPTION (1 OR 2 CURRENTLY ALLOWED)
!| SPEC           |<->| SPECTRUM STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY: SPECTRUM
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                                 :: CHAINTWC
      TYPE(SPECTRUM)   , INTENT(INOUT)        :: SPEC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
      SELECT CASE (CHAINTWC)
        CASE (1)
          CALL GET_TOMSPEC_VALUE1(SPEC)
        CASE (2)
          CALL GET_TOMSPEC_VALUE2(SPEC)
        CASE DEFAULT
          WRITE(LU,*) 'GET_TOMSPEC_VALUES: BAD OPTION: ',CHAINTWC
          CALL PLANTE(1)
          STOP
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
