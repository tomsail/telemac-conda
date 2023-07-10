!                   *****************************
                    SUBROUTINE ART_READ_BIN_FRLIQ
!                   *****************************
!
     &(NOM)
!
!***********************************************************************
! TELEMAC2D   V8P0                                          Dec 2018
!***********************************************************************
!
!brief    READS A BINARY LIQUID BOUNDARY FILE AND EXTRACTS HM0 VALUES
!+        FOR THE REQUIRED TIME
!+        ASSUMES THAT IPOBO (BND) HAS THE GLOBAL NODE NUMBER WHETHER
!+        SIMULATION IS IN SERIAL OR PARALLEL MODE
!
!history  N. DURAND (HRW)
!+        14/09/2017
!+        V7P3
!+   First version
!
!history  N. DURAND (HRW)
!+        Dec 2018
!+        V8P0
!+   WACLQD replaces ARTBI1
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16),INTENT(IN)            :: NOM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                      :: IERR, I
      INTEGER                      :: NFIC, TYP
      INTEGER                      :: WACNVAR,WACNPOIN
      INTEGER                      :: RECORD
!
      CHARACTER(LEN=8)             :: FFORMAT
      CHARACTER(LEN=16),ALLOCATABLE :: VARNAME(:),VARUNIT(:)
!
      LOGICAL                      :: DEJAHS1
!
!-----------------------------------------------------------------------
!
      NFIC = ART_FILES(WACLQD)%LU
      FFORMAT = ART_FILES(WACLQD)%FMT
!     THE SUPPORT MESH IS (BOUNDARY) EDGES
      TYP = EDGE_BND_ELT_TYPE
      DEJAHS1 = .FALSE.
!
!-----------------------------------------------------------------------
!
!     PARAMETERS OUTPUT TO THE TOMAWAC RESULT FILE
!     AND PRESENT IN THE BINARY BOUNDARY FILE
!
      CALL GET_DATA_NVAR(FFORMAT,NFIC,WACNVAR,IERR)
      CALL CHECK_CALL(IERR,'READ_BIN_FRLIQ:GET_DATA_NVAR')
      ALLOCATE(VARNAME(WACNVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'READ_BIN_FRLIQ:VARNAME')
      ALLOCATE(VARUNIT(WACNVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'READ_BIN_FRLIQ:VARUNIT')
      CALL GET_DATA_VAR_LIST(FFORMAT,NFIC,WACNVAR,VARNAME,VARUNIT,IERR)
      CALL CHECK_CALL(IERR,'READ_BIN_FRLIQ:GET_DATA_VAR_LIST')
!
!-----------------------------------------------------------------------
!
!     NUMBER OF POINTS IN THE BINARY BOUNDARY FILE
!
      CALL GET_MESH_NPOIN(FFORMAT,NFIC,TYP,WACNPOIN,IERR)
      CALL CHECK_CALL(IERR,'READ_BIN_FRLIQ:GET_MESH_NPOIN')
!
!-----------------------------------------------------------------------
!
!       HM0 AT RECORD TPSTWC
!
      WRITE(LU,201) TPSTWC
      CALL GET_DATA_TIMESTEP(FFORMAT,NFIC,RECORD,TPSTWC,IERR)
      CALL CHECK_CALL(IERR,'READ_BIN_FRLIQ:GET_DATA_TIMESTEP')
!
      DO I=1,WACNVAR

        IF(VARNAME(I).EQ.NOM) THEN
          IF(DEBUG.GT.0) WRITE(LU,301) VARNAME(I)
          CALL GET_DATA_VALUE(FFORMAT,NFIC,RECORD,VARNAME(I),HBS%R,
     &                      WACNPOIN,IERR)
          CALL CHECK_CALL(IERR,'READ_BIN_FRLIQ:GET_DATA_VALUE')
          DEJAHS1 = .TRUE.

        ENDIF

      ENDDO
!
      IF( .NOT.DEJAHS1 ) THEN
        WRITE(LU,401)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(VARNAME,VARUNIT)
!
!-----------------------------------------------------------------------
!
!     PRINTOUT FORMATS:
!
201   FORMAT(/,1X,'READ_BIN_FRLIQ : READING TIME STEP ',1F9.2,'S')

301   FORMAT(  1X,'               :         PARAMETER ',1A18)
401   FORMAT(/,1X,'READ_BIN_FRLIQ : MISSING HM0 ',
     &            'IN THE TOMAWAC RESULT FILE')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
