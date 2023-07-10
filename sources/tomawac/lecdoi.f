!                   *****************
                    SUBROUTINE LECDOI
!                   *****************
!
     &(F1,NAME1,MODE1, F2,NAME2,MODE2,F3,NAME3,MODE3,
     & NPOIN2,NDON,FFORMAT,AT,TV1,TV2,
     & F11,F12,F21,F22,F31,F32,INDIC,CHDON,NVAR,TEXTE,
     & TROUVE,UNITIME,PHASTIME)
!
!***********************************************************************
! TOMAWAC   V6P3                                   20/06/2011
!***********************************************************************
!
!brief    THIS SUBROUTINE PROJECTS THE CURRENTS / WINDS ON THE
!+                COMPUTATION MESH AND INTERPOLATES TO FIRST TIME STEP.
!+
!+           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D AMONGST OTHERS)
!
!history
!+
!+        V5P0
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
!history  G.MATTAROLO (EDF)
!+        05/2011
!+        V6P1
!+   Bug correction in the reading of the TELEMAC format file
!
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        16/11/2012
!+        V6P3
!+   Only SELAFIN format with same mesh kept. A number of arguments
!+   removed
!
!history  J-M HERVOUET (EDF - LNHE)
!+        24/01/2013
!+        V6P3
!+   Generalised for reading 3 variables with given names.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| FONFMT         |-->| DATA FILE FORMAT
!| CHDON          |-->| NAME OF THE VARIABLE READ FROM THE DATA FILE
!| F1             |<--| FIRST VARIABLE TO READ
!| F2             |<--| SECOND VARIABLE TO READ
!| F3             |<--| THIRD VARIABLE TO READ
!| F11            |<->| DATA VALUES AT TIME TV1 IN THE DATA FILE FOR F1
!| F12            |<->| DATA VALUES AT TIME TV2 IN THE DATA FILE FOR F1
!| F21            |<->| DATA VALUES AT TIME TV1 IN THE DATA FILE FOR F2
!| F22            |<->| DATA VALUES AT TIME TV2 IN THE DATA FILE FOR F2
!| F31            |<->| DATA VALUES AT TIME TV1 IN THE DATA FILE FOR F3
!| F32            |<->| DATA VALUES AT TIME TV2 IN THE DATA FILE FOR F3
!| INDIC          |-->| FILE FORMAT
!| MODE1          |-->| MODE: 0= DO NOT READ
!|                |   |       1= READ IF PRESENT
!|                |   |       2= READ AND STOP IF NOT PRESENT
!| MODE2          |-->| LIKE MODE1 FOR SECOND VARIABLE
!| MODE3          |-->| LIKE MODE1 FOR THIRD VARIABLE
!| NAME1          |-->| NAME OF FIRST VARIABLE
!| NAME2          |-->| NAME OF SECOND VARIABLE
!| NAME3          |-->| NAME OF THIRD VARIABLE
!| NDON           |-->| LOGICAL UNIT NUMBER OF THA DATA FILE
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NVAR           |<--| NUMBER OF VARIABLES READ
!| PHASTIME       |-->| TIME SHIFT IN FILE
!| TEXTE          |<->| NAMES OF VARIABLES IN SERAFIN FILE
!| TROUVE         |<->| 3 LOGICAL, WILL SAY IF VARIABLES HAVE BEEN FOUND
!| TV1            |<->| DATA TIME T1
!| TV2            |<->| DATA TIME T2
!| UNITIME        |-->| UNIT OF TIME IN FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_LECDOI => LECDOI
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NDON,NPOIN2,INDIC
      INTEGER, INTENT(IN)             :: MODE1,MODE2,MODE3
      INTEGER, INTENT(INOUT)          :: NVAR
      DOUBLE PRECISION, INTENT(INOUT) :: F1(NPOIN2),F2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F3(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F11(NPOIN2),F12(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F21(NPOIN2),F22(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F31(NPOIN2),F32(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: AT,UNITIME,PHASTIME
      DOUBLE PRECISION, INTENT(INOUT) :: TV1,TV2
      CHARACTER(LEN=8), INTENT(IN)    :: FFORMAT
      CHARACTER(LEN=7), INTENT(IN)    :: CHDON
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1,NAME2,NAME3
      CHARACTER(LEN=32),INTENT(INOUT) :: TEXTE(30)
      LOGICAL, INTENT(INOUT)          :: TROUVE(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NP,I,J,MODE(3)
      DOUBLE PRECISION COEF
      CHARACTER(LEN=80) TITCAS
      CHARACTER(LEN=32) NAME(3),FULL_NAME(3)
      CHARACTER(LEN=16), ALLOCATABLE :: VAR_NAME(:), VAR_UNIT(:)
      DOUBLE PRECISION :: TIME1,TIME2
      INTEGER :: RECORD1,RECORD2
      INTEGER :: IERR
!
!-----------------------------------------------------------------------
!
      MODE(1)=MODE1
      MODE(2)=MODE2
      MODE(3)=MODE3
      NAME(1)=NAME1
      NAME(2)=NAME2
      NAME(3)=NAME3
!
!-----------------------------------------------------------------------
!     READS THE POINTS FROM LOGICAL UNIT NDON
!-----------------------------------------------------------------------
!
      IF(INDIC.EQ.3) THEN
!
!       -----------------------------------------------------------------
!       TELEMAC FORMAT,
!       VARIABLES 1 AND 2 ARE THE X AND Y COMPONENTS OF THE WIND
!       -----------------------------------------------------------------
!
        ! Getting title
        CALL GET_MESH_TITLE(FFORMAT,NDON,TITCAS,IERR)
        CALL CHECK_CALL(IERR,'LECDOI:GET_MESH_TITLE')
        !
        CALL GET_DATA_NVAR(FFORMAT,NDON,NVAR,IERR)
        CALL CHECK_CALL(IERR,'LECDOI:GET_DATA_NVAR')
        !
        ALLOCATE(VAR_NAME(NVAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'LECDOI:VAR_NAME')
        ALLOCATE(VAR_UNIT(NVAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'LECDOI:VAR_UNIT')
        CALL GET_DATA_VAR_LIST(FFORMAT,NDON,NVAR,VAR_NAME,VAR_UNIT,IERR)
        CALL CHECK_CALL(IERR,'LECDOI:GET_DATA_VAR_LIST')
        DO I=1,NVAR
          TEXTE(I)(1:16) = VAR_NAME(I)
          TEXTE(I)(17:32) = VAR_UNIT(I)
          ! CHECK IF THE VARIABLE ARE IN THE FILE
          DO J=1,3
            IF((TEXTE(I).EQ.NAME(J)).AND.
     &        MODE(J).GT.0) THEN
              TROUVE(J) = .TRUE.
              FULL_NAME(J) = NAME(J)
            ENDIF
          ENDDO
        ENDDO
        DEALLOCATE(VAR_NAME)
        DEALLOCATE(VAR_UNIT)
        ! get the number of point
        CALL GET_MESH_NPOIN(FFORMAT,NDON,POINT_BND_ELT_TYPE,NP,IERR)
        CALL CHECK_CALL(IERR,'LECDOI:GET_MESH_NPOIN')
        WRITE(LU,*) '--------------------------------------------'
        WRITE(LU,*) 'LECDOI : READING OF TELEMAC DATA FILE '
        WRITE(LU,*) '         FILE TITLE : ',TITCAS
        WRITE(LU,*) '         NUMBER OF POINTS   : ',NP
        WRITE(LU,*) '--------------------------------------------'
        IF(NP.NE.NPOIN2) THEN
          WRITE(LU,*) ' '
          WRITE(LU,*) 'THE MESH OF THE CURRENTS FILE'
          WRITE(LU,*) 'IS DIFFERENT FROM THE GEOMETRY FILE'
          WRITE(LU,*) ' '
          CALL PLANTE(1)
          STOP
        ENDIF
        ! Timesteps
        RECORD1 = 0
        RECORD2 = 1
        CALL GET_DATA_TIME(FFORMAT,NDON,RECORD1,TIME1,IERR)
        CALL CHECK_CALL(IERR,'LECDOI:GET_DATA_TIME')
        TV1=(TIME1-PHASTIME)*UNITIME
        !
        IF(TV1.GT.AT) THEN
          WRITE(LU,*) '************************************************'
          WRITE(LU,*) 'THE FIRST RECORDING OF THE ',CHDON,' FILE '
          WRITE(LU,*) '  ',TV1,' IS OLDER THAN THE BEGINNING '
          WRITE(LU,*) '  OF THE COMPUTATION',AT
          WRITE(LU,*) '************************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
        !
        ! Get the two records before and after at for the interpolation
        DO
          CALL GET_DATA_TIME(FFORMAT,NDON,RECORD2,TIME2,IERR)
          CALL CHECK_CALL(IERR,'LECDOI:GET_DATA_TIME')
          TV2=(TIME2-PHASTIME)*UNITIME
          IF(TV2.LT.AT) THEN
            RECORD1 = RECORD2
            RECORD2 = RECORD2 + 1
            TV1 = TV2
          ELSE
            EXIT
          ENDIF
        ENDDO
        ! Check if all the variables are found for record1
        DO J=1,3
          IF(MODE(J).EQ.2.AND..NOT.TROUVE(J)) THEN
            WRITE(LU,*) 'LECDOI: VARIABLE ',NAME1,' NOT FOUND'
            WRITE(LU,*) TRIM(NAME(J)(1:16))
            CALL PLANTE(1)
            STOP
          ELSEIF(MODE(J).GT.0.AND.TROUVE(J)) THEN
            WRITE(LU,*) 'VARIABLE ',J,' READ (',
     &      TRIM(NAME(J)(1:16)),') AT TIME ',TV1
            ! Read the data for varialbe j on record1
            IF(J.EQ.1) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD1,
     &                            FULL_NAME(J),F11,NP,IERR)
            ELSEIF(J.EQ.2) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD1,
     &                            FULL_NAME(J),F21,NP,IERR)
            ELSEIF(J.EQ.3) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD1,
     &                            FULL_NAME(J),F31,NP,IERR)
            ENDIF
          ENDIF
        ENDDO
        ! Read the variables
        !
        ! Check if all the variables are found for record2
        DO J=1,3
          IF(MODE(J).EQ.2.AND..NOT.TROUVE(J)) THEN
            WRITE(LU,*) 'LECDOI: VARIABLE ',NAME1,' NOT FOUND'
            WRITE(LU,*) NAME(J)
            CALL PLANTE(1)
            STOP
          ELSEIF(MODE(J).GT.0.AND.TROUVE(J)) THEN
            WRITE(LU,*) 'VARIABLE ',J,' READ (',
     &      TRIM(NAME(J)(1:16)),') AT TIME ',TV2
            ! Read the data for varialbe j on record1
            IF(J.EQ.1) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD2,
     &                            FULL_NAME(J),F12,NP,IERR)
            ELSEIF(J.EQ.2) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD2,
     &                            FULL_NAME(J),F22,NP,IERR)
            ELSEIF(J.EQ.3) THEN
              CALL GET_DATA_VALUE(FFORMAT,NDON,RECORD2,
     &                            FULL_NAME(J),F32,NP,IERR)
            ENDIF
          ENDIF
        ENDDO
!
      ELSEIF (INDIC.EQ.4) THEN
!
!       READS A USER-DEFINED FORMAT
!
        IF(CHDON(1:1).EQ.'C') THEN
!         READS A CURRENT FIELD
          TROUVE(1)=.TRUE.
          TROUVE(2)=.TRUE.
          NVAR=2
          CALL COUUTI(NDON,FFORMAT)
        ELSEIF(CHDON(1:1).EQ.'V') THEN
!         READS A WIND FIELD
          TROUVE(1)=.TRUE.
          TROUVE(2)=.TRUE.
          NVAR=2
          CALL VENUTI(NDON,FFORMAT)
        ELSEIF(CHDON(1:1).EQ.'H') THEN
!         READS A DEPTH FIELD
          TROUVE(3)=.TRUE.
          NVAR=1
          CALL MARUTI(NDON,FFORMAT)
        ELSE
          WRITE(LU,*) 'UNKNOWN DATA TYPE'
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
        WRITE(LU,*) '************************************************'
        WRITE(LU,*)'LECDOI : UNKNOWN INDICATOR OF FORMAT : ',INDIC
        WRITE(LU,*)'         FOR THE ',CHDON,' DATA FILE '
        WRITE(LU,*) '************************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!   INTERPOLATES IN TIME
!-----------------------------------------------------------------------
!
      COEF=(AT-TV1)/(TV2-TV1)
!
      IF(TROUVE(1)) THEN
        DO I=1,NPOIN2
          F1(I)=(F12(I)-F11(I))*COEF+F11(I)
        ENDDO
      ENDIF
      IF(TROUVE(2)) THEN
        DO I=1,NPOIN2
          F2(I)=(F22(I)-F21(I))*COEF+F21(I)
        ENDDO
      ENDIF
      IF(TROUVE(3)) THEN
        DO I=1,NPOIN2
          F3(I)=(F32(I)-F31(I))*COEF+F31(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

