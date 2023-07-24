!                     **************************
                      SUBROUTINE NAMETRAC_WAQTEL
!                     **************************
!
     &  (NAMETRAC,NTRAC,MAXTRA,PROCESS)
!
!***********************************************************************
! WAQTEL      V8P4
!***********************************************************************
!
!brief    Gives names to tracers added by the water quality
!
!
!history  R.ATA
!+        12/09/2014
!+        V7P0
!+        CREATION
!
!history  M.JODEAU
!+        05/2016
!+        V7P3
!+        AED2 coupling
!
!history  S.E. BOURBAN (HRW)
!+        07/06/2017
!+        V7P3
!+        Re-working NAMETRAC to avoid conflicting naming convention
!+        between user defined tracers, water quality processes and
!+        ice processes.
!
!history  S.E. BOURBAN (HRW)
!+        21/09/2017
!+        V7P3
!+        WAQPROCESS is now a prime number, so that multiple processes
!+        can be called by multiplication of the prime numbers.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS, DIMENSION OF NAMETRAC
!| NAMETRAC       |<->| ARRAY OF NAMES OF TRACERS
!| NTRAC          |-->| MODIFYING NUMBER OF TRACER IF NECESARY
!| PROCESS        |-->| USED TO FILTER ACCESS TO WATER QUALITY PROCESSES
!|                |   | PROCESS CAN BE DIFFERENT FROM WAQPROCESS, AND
!|                |   | IF 1, THEN ONLY THE INITIAL SETTING IS DONE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL
      USE INTERFACE_WAQTEL, EX_NAMETRAC_WAQTEL => NAMETRAC_WAQTEL
#if defined HAVE_AED2
      USE T3D_AED2
#endif
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER          , INTENT(IN   )::  PROCESS,MAXTRA
      INTEGER          , INTENT(INOUT)::  NTRAC
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! AED2
#if defined HAVE_AED2
      CHARACTER(LEN=16),ALLOCATABLE :: NAMESAED2(:)
#endif
      INTEGER I
      LOGICAL :: FOUND
!
!-----------------------------------------------------------------------
!
      FOUND = .FALSE.
!
!-----------------------------------------------------------------------
!
!     INITIALISATION
!
      IF( PROCESS.EQ.1 ) THEN
        FOUND = .TRUE.

        WAQTR = 0
        RANKTR = 0
!
!     SORTING INDICES BY GROUPS
!
        RANK_OXYGN = 0 ! GROUP O2 TRACERS
        RANK_BIOMA = 0 ! GROUP BIOMASS TRACERS
        RANK_EUTRO = 0 ! GROUP EUTRO TRACERS
        RANK_MICRO = 0 ! GROUP MICROPOL TRACERS
        RANK_THERM = 0 ! GROUP THERMIC TRACER
!
        NWAQ_AED2 = 0
        RANK_AED2 = 0  ! GROUP AED2 TRACERS
!
        NWAQ_DEGRA = 0
        RANK_DEGRA = 0 ! GROUP TRACERS WITH A DEGRADATION LAW
!
!     INDICES OF UNIQUE TRACERS
!
        IND_T = 0     ! TEMPERATURE
        IND_S = 0     ! SALINITY
        IND_O2 = 0    ! DISSOLVED O2
        IND_OL = 0    ! ORGANIC LOAD
        IND_NH4 = 0   ! NH4 LOAD
        IND_PHY = 0   ! PHYTO BIOMASS
        IND_PO4 = 0   ! DISSOLVED PO4
        IND_POR = 0   ! POR NON ASSIM
        IND_NO3 = 0   ! DISSOLVED NO3
        IND_NOR = 0   ! NOR NON ASSIM
        IND_SS = 0    ! SUSPENDED LOAD
        IND_SF = 0    ! BED SEDIMENTS
        IND_C = 0     ! MICRO POLLUTANT
        IND_CSS = 0   ! ABS. SUSP. LOAD.
        IND_CSF = 0   ! ABSORB. BED SED.
        IND_CSS2 = 0  ! ABS. SUSP. LOAD.2
        IND_CSF2 = 0  ! ABSORB. BED SED.2

!
      ELSE IF( PROCESS*INT(WAQPROCESS/PROCESS).NE.WAQPROCESS ) THEN
        WRITE(LU,21) PROCESS,WAQPROCESS
21    FORMAT(1X,'NAMETRAC_WAQTEL: PROCESS CALLED ',I4,
     &       ' IS NOT A MULTIPLYING VALUE OF WAQ: ',I4)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     THERMIC MODULE
!
      IF( 11*INT(PROCESS/11).EQ.PROCESS ) THEN
        FOUND = .TRUE.
!
!       1. ~~> TEMPERATURE
        IF( IND_T.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_T,.TRUE.,
     &      'TEMPERATURE     ','TEMPERATURE     ','   oC           ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_T
        ENDIF
        RANK_THERM(1) = IND_T
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     AED2 COUPLING
!
      IF( 13*INT(PROCESS/13).EQ.PROCESS ) THEN
#if defined HAVE_AED2
        FOUND = .TRUE.
!
!       1. ~~> TEMPERATURE
        IF( IND_T.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_T,.TRUE.,
     &      'TEMPERATURE     ','TEMPERATURE     ','   oC           ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_T
        ENDIF
!
!       2. ~~> SALINITY
        IF( IND_S.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_S,.TRUE.,
     &      'SALINITE        ','SALINITY        ','   g/l          ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_S
        ENDIF
!
        CALL NAMES_VAR_AED2(NWQVARS,NAMESAED2)
!       I. ~~> AED2 VARIABLES
        NWAQ_AED2 = NWQVARS
        DO I = 1,NWQVARS
          IF( NAMESAED2(I) == 'CAR_pH          ' ) THEN
            CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &        RANK_AED2(I),.TRUE.,
     &        NAMESAED2(I),NAMESAED2(I),'   -/m3         ')
          ELSE
            CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &        RANK_AED2(I),.TRUE.,
     &        NAMESAED2(I),NAMESAED2(I),'   mmol/m3      ')
          ENDIF
          IF( RANK_AED2(I).EQ.NTRAC ) THEN
            WAQTR = WAQTR + 1
            RANKTR(WAQTR) = RANK_AED2(I)
          ENDIF
        ENDDO
!
#else
        WRITE(LU,*) 'ERROR: AED2 LIBRARY NOT COMPILED'
        CALL PLANTE(1)
        STOP
#endif
      ENDIF
!
!-----------------------------------------------------------------------
!
!     O2 MODULE
!
      IF( 2*INT(PROCESS/2).EQ.PROCESS ) THEN
        FOUND = .TRUE.
!
!       1. ~~> DISSOLVED O2
        IF( IND_O2.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_O2,.TRUE.,
     &      'O2 DISSOUS      ','DISSOLVED O2    ','   mgO2/l       ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_O2
        ENDIF
        RANK_OXYGN(1) = IND_O2
!
!       2. ~~> ORGANIC LOAD
        IF( IND_OL.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_OL,.TRUE.,
     &      'CHARGE ORGANIQUE','ORGANIC LOAD    ','   mgO2/l       ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_OL
        ENDIF
        RANK_OXYGN(2) = IND_OL
!
!       3. ~~> NH4 LOAD
        IF( IND_NH4.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_NH4,.TRUE.,
     &      'CHARGE NH4      ','NH4 LOAD        ','   mgNH4/l      ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_NH4
        ENDIF
        RANK_OXYGN(3) = IND_NH4
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     BIOMASS MODULE
!
      IF( 3*INT(PROCESS/3).EQ.PROCESS ) THEN
        FOUND = .TRUE.
!
!       1. ~~> PHYTO BIOMASS
        IF( IND_PHY.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_PHY,.TRUE.,
     &      'BIOMASSE PHYTO  ','PHYTO BIOMASS   ',' micro_g/l      ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_PHY
        ENDIF
        RANK_BIOMA(1) = IND_PHY
!
!       2. ~~> DISSOLVED PO4
        IF( IND_PO4.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_PO4,.TRUE.,
     &      'PO4 DISSOUS     ','DISSOLVED PO4   ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_PO4
        ENDIF
        RANK_BIOMA(2) = IND_PO4
!
!       3. ~~> POR NON ASSIM
        IF( IND_POR.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_POR,.TRUE.,
     &      'POR NON ASSIMILE','POR NON ASSIM   ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_POR
        ENDIF
        RANK_BIOMA(3) = IND_POR
!
!       4. ~~> DISSOLVED NO3
        IF( IND_NO3.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_NO3,.TRUE.,
     &      'NO3 DISSOUS     ','DISSOLVED NO3   ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_NO3
        ENDIF
        RANK_BIOMA(4) = IND_NO3
!
!       5. ~~> NOR NON ASSIM
        IF( IND_NOR.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_NOR,.TRUE.,
     &      'NOR NON ASSIMILE','NOR NON ASSIM   ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_NOR
        ENDIF
        RANK_BIOMA(5) = IND_NOR
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EUTRO MODULE
!
      IF( 5*INT(PROCESS/5).EQ.PROCESS ) THEN
        FOUND = .TRUE.
!
!       1. ~~> PHYTO BIOMASS
        IF( IND_PHY.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_PHY,.TRUE.,
     &      'BIOMASSE PHYTO  ','PHYTO BIOMASS   ',' micro_g/l      ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_PHY
        ENDIF
        RANK_EUTRO(1) = IND_PHY
!
!       2. ~~> DISSOLVED PO4
        IF( IND_PO4.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_PO4,.TRUE.,
     &      'PO4 DISSOUS     ','DISSOLVED PO4   ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_PO4
        ENDIF
        RANK_EUTRO(2) = IND_PO4
!
!       3. ~~> POR NON ASSIMIL
        IF( IND_POR.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_POR,.TRUE.,
     &      'POR NON ASSIMILE','POR NON ASSIMIL ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_POR
        ENDIF
        RANK_EUTRO(3) = IND_POR
!
!       4. ~~> DISSOLVED NO3
        IF( IND_NO3.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_NO3,.TRUE.,
     &      'NO3 DISSOUS     ','DISSOLVED NO3   ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_NO3
        ENDIF
        RANK_EUTRO(4) = IND_NO3
!
!       5. ~~> NOR NON ASSIM
        IF( IND_NOR.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_NOR,.TRUE.,
     &      'NOR NON ASSIMILE','NOR NON ASSIM   ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_NOR
        ENDIF
        RANK_EUTRO(5) = IND_NOR
!
!       6. ~~> NH4 LOAD
        IF( IND_NH4.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_NH4,.TRUE.,
     &      'CHARGE NH4      ','NH4 LOAD        ','   mgNH4/l      ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_NH4
        ENDIF
        RANK_EUTRO(6) = IND_NH4
!
!       7. ~~> ORGANIC LOAD
        IF( IND_OL.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_OL,.TRUE.,
     &      'CHARGE ORGANIQUE','ORGANIC LOAD    ','   mgO2/l       ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_OL
        ENDIF
        RANK_EUTRO(7) = IND_OL
!
!       8. ~~> DISSOLVED O2
        IF( IND_O2.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_O2,.TRUE.,
     &      'O2 DISSOUS      ','DISSOLVED O2    ','   mgO2/l       ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_O2
        ENDIF
        RANK_EUTRO(8) = IND_O2
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     MICROPOL MODULE
!
      IF( 7*INT(PROCESS/7).EQ.PROCESS ) THEN
        FOUND = .TRUE.
!
!       1. ~~> SUSPENDED LOAD
        IF( IND_SS.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_SS,.TRUE.,
     &      'MAT. EN SUSP.   ','SUSPENDED LOAD  ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_SS
        ENDIF
        RANK_MICRO(1) = IND_SS
!
!       2. ~~> BED SEDIMENTS
        IF( IND_SF.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_SF,.TRUE.,
     &      'SEDIMENT DU FOND','BED SEDIMENTS   ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_SF
        ENDIF
        RANK_MICRO(2) = IND_SF
!
!       3. ~~> MICRO POLLUTANT
        IF( IND_C.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_C,.TRUE.,
     &      'MICRO POLLUANT  ','MICRO POLLUTANT ','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_C
        ENDIF
        RANK_MICRO(3) = IND_C
!
!       4. ~~> ABS. SUSP. LOAD.
        IF( IND_CSS.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_CSS,.TRUE.,
     &      'MES ABSORBEE    ','ABS. SUSP. LOAD.','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_CSS
        ENDIF
        RANK_MICRO(4) = IND_CSS
!
!       5. ~~> ABSORB. BED SED.
        IF( IND_CSF.EQ.0 ) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_CSF,.TRUE.,
     &      'SED. FOND. ABS. ','ABSORB. BED SED.','   mg/l         ')
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = IND_CSF
        ENDIF
        RANK_MICRO(5) = IND_CSF
!
        IF( KIN_MICROPOL.EQ.2 ) THEN
!
!         6. ~~> ABS. SUSP. LOAD.2
          IF( IND_CSS2.EQ.0 ) THEN
            CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &        IND_CSS2,.TRUE.,
     &        'MES ABSORBEE2    ','ABS. SUSP. LOAD2','   mg/l         ')
            WAQTR = WAQTR + 1
            RANKTR(WAQTR) = IND_CSS2
          ENDIF
          RANK_MICRO(6) = IND_CSS2
!
!         7. ~~> ABSORB. BED SED.2
          IF( IND_CSF2.EQ.0 ) THEN
            CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &        IND_CSF2,.TRUE.,
     &        'SED. FOND. ABS.2 ','ABSORB. BED SED2','   mg/l         ')
            WAQTR = WAQTR + 1
            RANKTR(WAQTR) = IND_CSF2
          ENDIF
          RANK_MICRO(7) = IND_CSF2
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     DEGRADATION LAW
!
      IF( 17*INT(PROCESS/17).EQ.PROCESS ) THEN
        FOUND = .TRUE.
!
        DO I = 1,NTRAC
          WAQTR = WAQTR + 1
          RANKTR(WAQTR) = I
          NWAQ_DEGRA = NWAQ_DEGRA + 1
          RANK_DEGRA(NWAQ_DEGRA) = I
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     GHOST PROCESS IN WAITING FOR THE MERGE WITH ICE PROCESS
!
      IF( 19*INT(PROCESS/19).EQ.PROCESS ) THEN
        FOUND = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     UNKNOWN PROCESS
!
      IF( .NOT.FOUND ) THEN
        WRITE(LU,20) PROCESS
20    FORMAT(1X,'NAMETRAC_WAQTEL: UNKNOWN WAQ MODULE: ',I4)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE NAMETRAC_WAQTEL
