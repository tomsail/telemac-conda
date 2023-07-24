!                   *****************
                    SUBROUTINE LECSUI
!                   *****************
!
     &(F,NDIRE,NF,NPOIN2,VENT, COURAN,NPRE,FFORMAT,MAREE,TRA01)
!
!***********************************************************************
! TOMAWAC   V7P1
!***********************************************************************
!
!brief    READS THE DATA FOR A CONTINUATION OF COMPUTATION.
!
!warning  Parameters MAREE, COURAN or VENT must not be changed between
!+        2 runs. This would require an extra implementation consisting
!+        of checking the names of variables.
!
!history  F MARCOS (LNH)
!+        01/02/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        26/11/2012
!+        V6P3
!+   Correction of bugs and double precision. Changing file format
!+   for domain decomposition.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| F              |<--| DIRECTIONAL SPECTRUM
!| MAREE          |-->| LOGICAL INDICATING CONSIDERATION OF TIDE
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPRE           |-->| LOIGCAL UNIT NUMBER OF PREVIOUS COMPUTATION FILE
!| TRA01          |<--| DOUBLE PRECISION WORK TABLE OF SIZE NPOIN2*NDIRE
!| VENT           |-->| LOGICAL INDICATING IF THERE IS A WIND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TOMAWAC, ONLY : NAMVEB, NAMVEF,
     & AT, TV1, TV2, TC1, TC2, TM1, TM2, ZM1 ,ZM2, DEPTH, DZHDT,
     & UC, VC, UV, VV,  UV1, VV1, UV2, VV2, UC1, VC1, UC2, VC2
      USE INTERFACE_TOMAWAC, EX_LECSUI => LECSUI
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPRE,NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN2*NDIRE)
      LOGICAL, INTENT(IN)             :: COURAN,VENT,MAREE
      CHARACTER(LEN=8), INTENT(IN)    :: FFORMAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISTAT,NPOIN,NVAR,NPL
      INTEGER NELEM, NPTFR, NPTIR2, NDP, TYP_ELEM
      INTEGER NTIMESTEP, X_ORIG, Y_ORIG
      CHARACTER(LEN=80) CAR
!
      INTEGER NVA3
      CHARACTER(LEN=16),ALLOCATABLE :: VAR_NAME(:),VAR_UNIT(:)
!
!***********************************************************************
!
      CALL READ_MESH_INFO(FFORMAT,NPRE,CAR,NVAR,NPOIN,TYP_ELEM,
     &                    NELEM,NPTFR,NPTIR2,NDP,NPL,X_ORIG,Y_ORIG)
!
      IF(NPL.NE.NDIRE) THEN
        WRITE(LU,*) 'LECSUI: BAD NUMBER OF PLANES IN THE PREVIOUS'
        WRITE(LU,*) '        COMPUTATION FILE : ',NPL,' FOUND'
        WRITE(LU,*) '                           ',NDIRE,' EXPECTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(NPOIN.NE.NDIRE*NPOIN2) THEN
        WRITE(LU,*)'LECSUI: BAD NUMBER OF POINTS IN THE PREVIOUS'
        WRITE(LU,*)'        COMPUTATION FILE : ',NPOIN,' FOUND'
        WRITE(LU,*)'                           ',NPOIN2,' EXPECTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(COURAN.OR.VENT) THEN
        I=NVAR-2
      ELSE
        I=NVAR-1
      ENDIF
!
      IF(I.NE.NF) THEN
        WRITE(LU,*) 'LECSUI : BAD NUMBER OF FREQUENCIES'
        WRITE(LU,*) '         IN THE PREVIOUS'
        WRITE(LU,*) '         COMPUTATION FILE : ',I,' FOUND'
        WRITE(LU,*) '                            ',NF,' EXPECTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     PRINTS TITLE
!
      WRITE(LU,*) ' '
      WRITE(LU,*) '**** FOLLOWING COMPUTATION ****'
      WRITE(LU,*) ' '
      WRITE(LU,*) 'TITLE OF THE PREVIOUS COMPUTATION :',CAR
!
!     READS TIME
!
      CALL GET_DATA_NTIMESTEP(FFORMAT,NPRE,NTIMESTEP,ISTAT)
      CALL CHECK_CALL(ISTAT,'LECSUI:GET_DATA_NTIMESTEP')
      CALL GET_DATA_TIME(FFORMAT,NPRE,NTIMESTEP-1,AT,ISTAT)
      CALL CHECK_CALL(ISTAT,'LECSUI:GET_DATA_TIME')
!
      WRITE(LU,*) '- COMPUTATIONAL RESUMPTION AT TIME ',AT
!
!
! Get the number of variables
      CALL GET_DATA_NVAR(FFORMAT,NPRE,NVA3,ISTAT)
      CALL CHECK_CALL(ISTAT,'LECSUI:GET_DATA_NVAR')

      ALLOCATE(VAR_NAME(NVA3),STAT=ISTAT)
      CALL CHECK_ALLOCATE(ISTAT,'LECSUI:VAR_NAME')
      ALLOCATE(VAR_UNIT(NVA3),STAT=ISTAT)
      CALL CHECK_ALLOCATE(ISTAT,'LECSUI:VAR_UNIT')
!
      CALL GET_DATA_VAR_LIST(FFORMAT,NPRE,NVA3,VAR_NAME,VAR_UNIT,ISTAT)
      CALL CHECK_CALL(ISTAT,'LECSUI:GET_DATA_VAR_LIST')
!READS F
!

      DO I=1,NF
        CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(I),F(1,1,I),NPOIN,
     &                 ISTAT,RECORD=-1)
        CALL CHECK_CALL(ISTAT,'LECSUI:FIND_VARIABLE')
      ENDDO
!
!     READS DEPTH (ALWAYS WRITTEN, EVEN IF NOT RELEVANT)
!
      IF(MAREE) THEN
!        CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(I),F(1,1,I),NPOIN2,
        CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(NF+1),TRA01,NPOIN,
     &                 ISTAT,RECORD=-1)
        CALL OV('X=Y     ', X=DEPTH, Y=TRA01(1:NPOIN2), DIM1=NPOIN2)

!       SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
        TM1=AT
        TM2=AT
        CALL OV('X=Y     ', X=ZM1, Y=DEPTH, DIM1=NPOIN2)
        CALL OV('X=Y     ', X=ZM2, Y=DEPTH, DIM1=NPOIN2)
        CALL OV('X=0     ', X=DZHDT, DIM1=NPOIN2)
      ELSE
        CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(NF+1),TRA01,NPOIN,
     &                 ISTAT,RECORD=-1)
        CALL CHECK_CALL(ISTAT,'LECSUI:FIND_VARIABLE')
      ENDIF
!
!     READS UC,VC,UV,VV IF HAS TO
!
      IF(COURAN.OR.VENT) THEN
        IF(VENT) THEN
          CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(NF+2),TRA01,4*NPOIN2,
     &                   ISTAT,RECORD=-1)
          CALL CHECK_CALL(ISTAT,'LECSUI:FIND_VARIABLE')
        ELSE
          CALL FIND_VARIABLE(FFORMAT,NPRE,VAR_NAME(NF+2),TRA01,2*NPOIN2,
     &                   ISTAT,RECORD=-1)
          CALL CHECK_CALL(ISTAT,'LECSUI:FIND_VARIABLE')
        ENDIF
      ENDIF
!
      IF(COURAN) THEN
        CALL OV('X=Y     ', X=UC, Y=TRA01(       1:  NPOIN2),
     &          DIM1=NPOIN2)
        CALL OV('X=Y     ', X=VC, Y=TRA01(NPOIN2+1:2*NPOIN2),
     &          DIM1=NPOIN2)
!       SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
        TC1=AT
        TC2=AT
        CALL OV('X=Y     ', X=UC1, Y=UC, DIM1=NPOIN2)
        CALL OV('X=Y     ', X=UC2, Y=UC, DIM1=NPOIN2)
        CALL OV('X=Y     ', X=VC1, Y=VC, DIM1=NPOIN2)
        CALL OV('X=Y     ', X=VC2, Y=VC, DIM1=NPOIN2)
      ENDIF
!
      IF(VENT) THEN
        CALL OV('X=Y     ', X=UV, Y=TRA01(2*NPOIN2+1:3*NPOIN2),
     &          DIM1=NPOIN2)
        CALL OV('X=Y     ', X=VV, Y=TRA01(3*NPOIN2+1:4*NPOIN2),
     &          DIM1=NPOIN2)
!       SETS TRIPLETS U,V,TV1 AND 2 TO UV,VV,AT
        TV1=AT
        TV2=AT
        IF (NAMVEB.NE.' '.OR.NAMVEF.NE.' ') THEN
          CALL OV('X=Y     ', X=UV1, Y=UV, DIM1=NPOIN2)
          CALL OV('X=Y     ', X=UV2, Y=UV, DIM1=NPOIN2)
          CALL OV('X=Y     ', X=VV1, Y=VV, DIM1=NPOIN2)
          CALL OV('X=Y     ', X=VV2, Y=VV, DIM1=NPOIN2)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
