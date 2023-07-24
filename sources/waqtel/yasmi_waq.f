!                       ***********************
                        SUBROUTINE YASMI_WAQ
!                       ***********************
     &  (YASMI)
!
!
!***********************************************************************
! TELEMAC2D   V7P0
!***********************************************************************
!
!brieF tells which tracers will have implicit source terms
!
!
!history  R.ATA
!+        12/02/2016
!+        V7P2
!+        CREATION
!
!         R. ATA
!+        07/07/2016
!+        V7P3
!+        ADAPTATION FOR THE NEW MANAGEMENT OF TRACERS
!
!history  S.E. BOURBAN (HRW)
!+        21/09/2017
!+        V7P3
!+        WAQPROCESS is now a prime number, so that multiple processes
!+        can be called by multiplication of the prime numbers.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| YASMI          |<--| LOGICS FOR IMPLICIT SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL
      USE INTERFACE_WAQTEL, EX_YASMI_WAQ => YASMI_WAQ
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      LOGICAL          , INTENT(INOUT)::  YASMI(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I
      LOGICAL :: FOUND
      FOUND = .FALSE.
!
!-----------------------------------------------------------------------
!
!     O2 MODULE
!
      IF( 2*INT(WAQPROCESS/2).EQ.WAQPROCESS ) THEN
!
!       DISSOLVED O2
        YASMI(IND_O2) = YASMI(IND_O2) .OR. .FALSE.
!       ORGANIC LOAD
        YASMI(IND_OL) = .TRUE.
!       NH4 LOAD
        YASMI(IND_NH4) = .TRUE.
!
        FOUND = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     BIOMASS MODULE
!
      IF( 3*INT(WAQPROCESS/3).EQ.WAQPROCESS ) THEN
!
!       PHYTO BIOMASS
        YASMI(IND_PHY) = YASMI(IND_PHY) .OR. .FALSE.
!       DISSOLVED PO4
        YASMI(IND_PO4) = YASMI(IND_PO4) .OR. .FALSE.
!       POR NON ASSIM
        YASMI(IND_POR) = YASMI(IND_POR) .OR. .FALSE.
!       DISSOLVED NO3
        YASMI(IND_NO3) = YASMI(IND_NO3) .OR. .FALSE.
!       NO3 NON ASSIM
        YASMI(IND_NOR) = YASMI(IND_NOR) .OR. .FALSE.
!
        FOUND = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EUTRO MODULE
!
      IF( 5*INT(WAQPROCESS/5).EQ.WAQPROCESS ) THEN
!
!       PHYTO BIOMASS
        YASMI(IND_PHY) = YASMI(IND_PHY) .OR. .FALSE.
!       DISSOLVED PO4
        YASMI(IND_PO4) = YASMI(IND_PO4) .OR. .FALSE.
!       POR NON ASSIM
        YASMI(IND_POR) = YASMI(IND_POR) .OR. .FALSE.
!       DISSOLVED NO3
        YASMI(IND_NO3) = YASMI(IND_NO3) .OR. .FALSE.
!       NOR NON ASSIM
        YASMI(IND_NOR) = YASMI(IND_NOR) .OR. .FALSE.
!       CHARGE NH4
        YASMI(IND_NH4) = .TRUE.
!       ORGANIC LOAD
        YASMI(IND_OL) = .TRUE.
!       DISSOLVED O2
        YASMI(IND_O2) = YASMI(IND_O2) .OR. .FALSE.
!
        FOUND = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     MICROPOL MODULE
!
      IF( 7*INT(WAQPROCESS/7).EQ.WAQPROCESS ) THEN
!
!       SUSPENDED LOAD
        YASMI(IND_SS) = YASMI(IND_SS) .OR. .FALSE.
!       BED SEDIMENTS
        YASMI(IND_SF) = YASMI(IND_SF) .OR. .FALSE.
!       MICRO POLLUTANT
        YASMI(IND_C) = .TRUE.
!       ADSORBED SUSPENDED LOAD
        YASMI(IND_CSS) = .TRUE.
!       ADSORBED BED SED
        YASMI(IND_CSF) = .TRUE.
        IF( KIN_MICROPOL.EQ.2 ) THEN
!         ADSORBED SUSPENDED LOAD 2
          YASMI(IND_CSS2) = .TRUE.
!         ADSORBED BED SED 2
          YASMI(IND_CSF2) = .TRUE.
        ENDIF
!
        FOUND = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     THERMIC MODULE
!
      IF( 11*INT(WAQPROCESS/11).EQ.WAQPROCESS ) THEN
!
!       CONSIDERED IMPLICIT BECAUSE ATABOS AND BTABOS ARE IMPLICIT
        YASMI(IND_T) = .TRUE.
!
        FOUND = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     AED2 COUPLING
!
      IF( 13*INT(WAQPROCESS/13).EQ.WAQPROCESS ) THEN
        FOUND = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     DEGRADATION LAW
!
      IF( 17*INT(WAQPROCESS/17).EQ.WAQPROCESS ) THEN
        FOUND = .TRUE.
!
        DO I = 1,NWAQ_DEGRA
          YASMI(RANK_DEGRA(I)) = .TRUE.
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     GHOST PROCESS IN WAITING FOR THE MERGE WITH ICE PROCESS
!
      IF( 19*INT(WAQPROCESS/19).EQ.WAQPROCESS ) THEN
        FOUND = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!      UNKNOWN PROCESS
!
      IF( .NOT.FOUND ) THEN
        WRITE(LU,20) WAQPROCESS
20    FORMAT(1X,'YASMI_WAQ: UNKNOWN WAQ MODULE: ',I4)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
