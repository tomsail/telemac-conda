!                     **************************
                      SUBROUTINE NAMETRAC_KHIONE
!                     **************************
!
     &  (NAMETRAC,NTRAC,MAXTRA)
!
!***********************************************************************
! KHIONE      V8P3
!***********************************************************************
!
!brief    Add tracers needed for the modelling of frazil ice
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MAXTRA   |-->| MAXIMUM NUMBER OF TRACERS, DIMENSION OF NAMETRAC
!| NAMETRAC |<->| ARRAY OF NAMES OF TRACERS
!| NTRAC    |<->| MODIFYING NUMBER OF TRACER IF NECESARY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_KHIONE, EX_NAMETRAC_KHIONE => NAMETRAC_KHIONE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(INOUT)::  NTRAC
      INTEGER,           INTENT(IN)   ::  MAXTRA
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: IFRA,I,K
      CHARACTER(LEN=2)   :: CHAR2
      LOGICAL :: FOUND
!
!-----------------------------------------------------------------------
!
      FOUND = .FALSE.
!
!-----------------------------------------------------------------------
!
!     ADDING TRACERS WHEN RELEVANT
!
!     THERMAL BUDGET
      IF( (THERMAL_BUDGET) .OR. (CLOGGING) ) THEN
        FOUND = .TRUE.
!
!   ~~> TEMPERATURE
        CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &    IND_T,.TRUE.,
     &    'TEMPERATURE     ',
     &    'TEMPERATURE     ',
     &    '   oC           ')
!
!   ~~> SALINITY
        IF(SALINITY) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_S,.TRUE.,
     &      'SALINITE        ',
     &      'SALINITY        ',
     &      '   ppt          ' )
        ENDIF
!
!   ~~> FRAZIL MULTI-CLASS
        IFRA = 1
        IF(NC_FRA.GT.1) THEN
          IND_FRA = NTRAC+1
          DO I=1,NC_FRA
            WRITE(CHAR2,'(I2)') IFRA
            IFRA = IFRA + 1
            CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,K,.TRUE.,
     &        'FRASIL ' //ADJUSTL(CHAR2)//'       ',
     &        'FRAZIL ' //ADJUSTL(CHAR2)//'       ',
     &        'VOLUME FRACTION ')
          ENDDO

!   ~~> FRAZIL MONO-CLASS
        ELSE
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_FRA,.TRUE.,
     &      'FRASIL          ',
     &      'FRAZIL          ',
     &      'VOLUME FRACTION ')
        ENDIF
!
!   ~~> DYNAMIC ICE COVER
        IF(DYN_ICOVER) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_DCI,.TRUE.,
     &      'FRACTION COUV.  ',
     &      'ICE COVER FRAC. ',
     &      'SURFAC FRACTION ')
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_DTI,.TRUE.,
     &      'EPAISSEUR COUV. ',
     &      'ICE COVER THICK.',
     &      '   M            ')
        ENDIF
!
!     STATIC ICE COVER
      ELSEIF( (ICOVER_IMPACT) .OR. (BD_ICE) ) THEN
!
        FOUND = .TRUE.
!
        NC_FRA = 0
!
!   ~~> TEMPERATURE
        CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &    IND_T,.TRUE.,
     &    'TEMPERATURE     ',
     &    'TEMPERATURE     ',
     &    '   oC           ')
!
!   ~~> SALINITY
        IF(SALINITY) THEN
          CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,
     &      IND_S,.TRUE.,
     &      'SALINITE        ',
     &      'SALINITY        ',
     &      '   ppt          ' )
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     UNKNOWN PROCESS
!
      IF(.NOT.FOUND ) THEN
        WRITE(LU,*) 'NAMETRAC_KHIONE: NO ACTIVE PROCESSES'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
