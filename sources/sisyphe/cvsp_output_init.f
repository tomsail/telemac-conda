!                     ***************************
                      SUBROUTINE CVSP_OUTPUT_INIT
!                     ***************************
!
!
!***********************************************************************
! SISYPHE   V7P2                                   05/12/2017
!***********************************************************************
!
!brief    CVSP_OUTPUT_INIT
!
!history  U.MERKEL
!+        30/07/2012
!+        V6P2
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic

!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history  R.KOPMANN (BAW)
!+        05/12/2017
!+        V7P2
!+        File opening via cas-file for VSPRES, no VSPHYD by default
!+        anymore, but could be reactiveted
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |<->|
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE CVSP_OUTPUTFILES
      USE INTERFACE_HERMES

      IMPLICIT NONE

      CHARACTER(LEN=32) VLABEL
!     CHARACTER(LEN=11) :: EXTENS
      INTEGER I, K
!     INTEGER IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER CFG(2)            ! FOR 3D MESH
      CFG(1) = OPTASS
      CFG(2) = PRODUC           ! PRODUC=1 HARD IN LECDON
!


!-----------------------------------------------------------------------
! CHECKING DIMENSIONS
!-----------------------------------------------------------------------
      IF(NUMVARUR3D2RES.LT.NSICLA+3) THEN
        WRITE(LU,*)'NUMVARUR3D2RES IS TOO SMALL'
        WRITE(LU,*)'PLEASE CHANGE NUMVARUR3D2RES TO ',NSICLA+3
      ENDIF
!-----------------------------------------------------------------------
! CHECKPOINTFILE
!-----------------------------------------------------------------------
      USRMSH_NPLAN = PRO_MAX_MAX
!      USRMSH_2DHYD_NPLAN = 2
!
!
!-----------------------------------------------------------------------
! ALLOCATES A 3D MESH FOR USEROUTPU: HERE VSPRES
!-----------------------------------------------------------------------
!
      CALL ALMESH(USRMSH,'USRMSH',41,SPHERI,CFG,
     &     SIS_FILES(SISGEO)%FMT, SIS_FILES(SISGEO)%LU,
     &     EQUA,0,NPLAN=USRMSH_NPLAN)
!
      DO I =1,USRMSH%NPTFR
        USRMSH%NBOR%I(I) = I
      END DO
!
!-----------------------------------------------------------------------
! ALLOCATES A 3D MESH FOR USEROUTPU: HERE VSPHYD
!-----------------------------------------------------------------------
!
!      CALL ALMESH(USRMSH_2DHYD,'USRHYD',41,SPHERI,CFG,
!     &     SIS_FILES(SISGEO)%FMT, SIS_FILES(SISGEO)%LU,
!     &     EQUA,0,NPLAN=USRMSH_2DHYD_NPLAN)
!
!      DO I =1,USRMSH_2DHYD%NPTFR
!        USRMSH_2DHYD%NBOR%I(I) = I
!      END DO
!
!-----------------------------------------------------------------------
! SET HERE THE NAMES OF THE PRINTOUT VARIABLES FOR THE USER FILE VSPRES
!-----------------------------------------------------------------------
!
      DO I = 1,NUMVARUR3D2RES
        IF (I.LE.3+NSICLA) THEN
          UR3D_FILES_OUTVAR(I) = .TRUE.
        ELSE
          UR3D_FILES_OUTVAR(I) = .FALSE.
        ENDIF
        WRITE(UNIT=VLABEL, FMT='(A5,I2,A25)') 'FRACTION_CLASS_',I-3
        UR3D_FILES_LABELS(I) = VLABEL
      ENDDO
! EXAMPLE FOR MANUALLY OVERWRITTING LEN=32
      UR3D_FILES_LABELS(1)= 'Z [M] ' !'PROFILE_ELEVATION               '
      UR3D_FILES_LABELS(2)= 'PROFILE_D50 [M]                 '
      UR3D_FILES_LABELS(3)= 'PROFILE_ERROR [-]               '
!
!-----------------------------------------------------------------------
! SET HERE THE NAMES OF THE PRINTOUT VARIABLES FOR THE USER FILE VSPHYD
!-----------------------------------------------------------------------
!
!      DO I = 1,NUMVAR2DHYD
!        UR2DHYD_FILES_OUTVAR(I) = .TRUE.
!      ENDDO
!      !'Z [M] AND ZF [M]                '
!      UR2DHYD_FILES_LABELS(1)= 'Z [M]                           '
!      UR2DHYD_FILES_LABELS(2)= 'U [M/S]                         '
!      UR2DHYD_FILES_LABELS(3)= 'V [M/S]                         '
!      UR2DHYD_FILES_LABELS(4)= 'W [M/S]                         '
!      UR2DHYD_FILES_LABELS(5)= 'SCALAR VELOCITY [M/S]           '
!      UR2DHYD_FILES_LABELS(6)= 'TAU [N/M**2]                    '
!
!-----------------------------------------------------------------------
! ALLOCATES THE BLOCK CONNECTING A VARIABLE NAME TO ITS ARRAY
!-----------------------------------------------------------------------
!
      CALL ALLBLO(URBLOC3D, 'URBL3D')
!      CALL ALLBLO(URBLOC2DHYD, 'URB2DH')
!
!-----------------------------------------------------------------------
! OPENS THE FILES FOR WRITING
!-----------------------------------------------------------------------
!
!      I=3
!        IF(NCSIZE.LE.1) THEN
! SCALAR
!          CALL OPEN_MESH(CP_FILES(I)%FMT,CP_FILES(I)%TELNAME,
!     &                   CP_FILES(I)%LU,CP_FILES(I)%ACTION,IERR)
!        ELSE
! PARALLEL, FILE TYPE: SCAL
!          IF(CP_FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
!            CALL OPEN_MESH(CP_FILES(I)%FMT,CP_FILES(I)%TELNAME,
!     &                     CP_FILES(I)%LU,CP_FILES(I)%ACTION,IERR)
! PARALLEL, OTHER FILE TYPE
!       ELSE
!          CALL OPEN_MESH(CP_FILES(I)%FMT,TRIM(CP_FILES(I)%TELNAME)
!     &                   //EXTENS(NCSIZE-1,IPID),
!     &                   CP_FILES(I)%LU,CP_FILES(I)%ACTION,IERR)
!          ENDIF
!        ENDIF
!        CALL CHECK_CALL(IERR,'CVSP_OUTPU_INIT:OPEN_MESH')

!        IF(IERR.NE.0) THEN
!           WRITE(LU,*) 'ERROR WHILE OPENING: ', CP_FILES(I)%TELNAME
!           WRITE(LU,*) 'IN ',CP_FILES(I)%FMT,' FORMAT '
!           WRITE(LU,*) 'IN ',CP_FILES(I)%ACTION,' MODE'
!           WRITE(LU,*) 'ERROR ',IERR
!           CALL PLANTE(1)
!           STOP
!        ENDIF
!
!
!-----------------------------------------------------------------------
! WRITES THE HEADER OF THE RESFILES
!-----------------------------------------------------------------------
!
      CALL WRITE_HEADER(SIS_FILES(VSPRES)%FMT,
     &     SIS_FILES(VSPRES)%LU,
     &     'USEROUTPUT3D                   '//
     &     '                                         ',
     &     NUMVARUR3D2RES,
     &     UR3D_FILES_LABELS,
     &     UR3D_FILES_OUTVAR)
!      CALL WRITE_HEADER(CP_FILES(3)%FMT,
!     &     CP_FILES(3)%LU,
!     &     '2D HYDRAULIC PARAMETERS IN 3D  '//
!     &     '                                         ',
!     &     NUMVAR2DHYD,
!     &     UR2DHYD_FILES_LABELS,
!     &     UR2DHYD_FILES_OUTVAR)
!
!-----------------------------------------------------------------------
! WRITES THE MESH INFORMATION IN THE OUTPUT FILE :
!-----------------------------------------------------------------------
!
      CALL WRITE_MESH(SIS_FILES(VSPRES)%FMT, ! RESULTS FILE FORMAT
     &     SIS_FILES(VSPRES)%LU,   ! LU FOR RESULTS FILE
     &     USRMSH,
     &     USRMSH_NPLAN,     ! NUMBER OF PLANES /NA/
     &     MARDAT,           ! START DATE
     &     MARTIM,T1,T2,           ! START TIME
     &     NCSIZE.GT.1, NPTIR)

!      CALL WRITE_MESH(CP_FILES(3)%FMT, ! RESULTS FILE FORMAT
!     &     CP_FILES(3)%LU,   ! LU FOR RESULTS FILE
!     &     USRMSH_2DHYD,
!     &     USRMSH_2DHYD_NPLAN, ! NUMBER OF PLANES /NA/
!     &     MARDAT,           ! START DATE
!     &     MARTIM,T1,T2,           ! START TIME
!     &     NCSIZE.GT.1, NPTIR)
!
!-----------------------------------------------------------------------
! INITS OUTPUT VECTORS
!-----------------------------------------------------------------------
!
! VSPRES
      DO K = 1, NSICLA
        CALL BIEF_ALLVEC(1,VSP_FRA(K),'VSPFRA',41,1,1,USRMSH)
      ENDDO

      CALL BIEF_ALLVEC(1,VSP_D,     'VSP__D',41,1,1,USRMSH)
      CALL BIEF_ALLVEC(1,VSP_D50,   'VSPD50',41,1,1,USRMSH)
      CALL BIEF_ALLVEC(1,VSP_ERROR, 'VSP_ER',41,1,1,USRMSH)

! VSPHYD
!      DO K = 1, NUMVAR2DHYD
!        CALL BIEF_ALLVEC(1,UR2DHYD(K),'VSPHYD',41,1,1,USRMSH_2DHYD)
!      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
