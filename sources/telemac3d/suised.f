!                   *****************
                    SUBROUTINE SUISED
!                   *****************
!
     &(S3D_EPAI,S3D_HDEP,ZR,ZF,T2,NPOIN2,S3D_NCOUCH,S3D_ITASS,
     & NSUIS,FFORMAT)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    READS SEDIMENTOLOGICAL DATA
!+                FOR A RESUMING COMPUTATION.
!
!warning  IMPLEMENTATION WITH SELAFIN ONLY WORKS WITH S3D_GIBSON
!
!history  C LE NORMANT (LNH)
!+        05/05/93
!+
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  S.E.BOURBAN AND N.DURAND (NRC-CHC)
!+        27/03/06
!+        V5P7
!+   SELAFIN IMPLEMENTATION
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history T. BENSON (HR-WALLINGFORD)
!+        15/08/2014
!+        V7P0
!+        Major rewrite to use 2D result file (simplified)
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| BINARY OF PREVIOUS SEDIMENT COMPUTATION FILE
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NSUIS          |-->| LOGICAL UNIT NUMBER OF THE PREVIOUS SEDIMENT
!|                |   | COMPUTATION FILE
!| S3D_EPAI       |<->| THICKNESS OF SOLID BED LAYER
!| S3D_HDEP       |<->| TOTAL THICKNESS OF MUD DEPOSIT
!| S3D_NCOUCH     |-->| NUMBER OF LAYERS WITHIN THE BED
!| ZF             |-->| ELEVATION OF BED
!|                |   | (FROM GEOMETRY OR 3D PREVIOUS COMPUTATION FILE)
!| ZR             |<--| ELEVATION OF RIGID BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!----------------------------------------------------------------------
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T2
      INTEGER, INTENT(IN)             :: NPOIN2,S3D_NCOUCH
      INTEGER, INTENT(IN)             :: S3D_ITASS, NSUIS
!
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_EPAI(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(OUT)   :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_HDEP(NPOIN2)
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
!

!
!----------------------------------------------------------------------
!
      INTEGER IPOIN
!
      INTEGER           :: IERR
      CHARACTER(LEN=80) :: TITLE
      INTEGER           :: RECORD
      CHARACTER(LEN=16) :: VARNAME
      INTEGER :: NPOIN_SUIS,TYP_ELEM,NELEM,NPTFR,NDP_SUIS,NPLAN
      DOUBLE PRECISION :: TIME
!
      INTEGER           :: NVAR,I,X_ORIG,Y_ORIG
!
!
!    NOTE: S3D_GIBSONMODEL NOT YET CODED
      IF (S3D_ITASS.EQ.2) THEN
        WRITE(LU,*)
     &    'SUISED: S3D_GIBSONMODEL NOT YET IMPLEMENTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   READ THE HEADER AND CHECK QUANTITIES / OPTIONS
      WRITE(LU,*) ' '
      WRITE(LU,*) 'READING BED SEDIMENT FROM PREVIOUS COMPUTATION FILE'
      WRITE(LU,*) ' '
!
      CALL READ_MESH_INFO(FFORMAT,NSUIS,TITLE,NVAR,NPOIN_SUIS,TYP_ELEM,
     &                    NELEM,NPTFR,NPTIR,NDP_SUIS,NPLAN,
     &                    X_ORIG,Y_ORIG)
!
      IF( (NPOIN_SUIS.NE.NPOIN2) .OR. (NDP_SUIS.NE.3) ) THEN
        WRITE(LU,*) 'SUISED: NUMBER OF NODES NOT COMPATIBLE'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(S3D_ITASS.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!     LOOK FOR 1. BED THICKNESS PER LAYER
!     IN THE RESULT FILE:
!     VARIABLE NAMES ARE BED DZ1, BED DZ2, etc....FOR EACH LAYER
!     UP TO S3D_NCOUCH(N.B. S3D_NCOUCHIS THE LOWEST LAYER)
!
!     ZERO THE TOTAL THICKNESS ARRAY
      DO IPOIN = 1,NPOIN2
          S3D_HDEP(IPOIN) = 0.D0
      ENDDO
!
!     FIND LAYER THICKNESS VARIABLES IN THE 2D RESULT FILE
      DO I=1,S3D_NCOUCH
!
!       MAKE THE NUMBERED NAME STRING
        IF(I.LT.10) THEN
          WRITE(VARNAME,'(A5,I1,A10)')  'LAYER',I,'  THICKNES'
        ELSEIF(I.LT.100) THEN
          WRITE(VARNAME,'(A5,I2,A9)')  'LAYER',I,' THICKNES'
        ELSE
          WRITE (LU,*) 'SUISED: NOT IMPLEMENTED FOR ',S3D_NCOUCH,
     &                 ' LAYERS'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        WRITE(LU,*)  'CHECKING PREVIOUS SED FILE FOR VARIABLE'

!       PUT RESULTS INTO T2 BIEF OBJECT
        RECORD = -1 ! To get the last time step
        CALL FIND_VARIABLE(FFORMAT,NSUIS,VARNAME,T2%R,NPOIN_SUIS,IERR,
     &                 RECORD=RECORD,TIME_RECORD=TIME)
!
        IF (IERR.EQ.0)  THEN
          WRITE(LU,*)
     &     'BED LAYER THICKNESS (LAYER',I,') FOUND IN GEOMETRY FILE'
!
!         TRANSFER THE DATA FROM BIEF OBJECT TO DOUBLE ARRAY
          DO IPOIN = 1,NPOIN2
            S3D_EPAI(IPOIN,I) = T2%R(IPOIN)
          ENDDO
        ELSE
          WRITE(LU,*)
     &     'BED LAYER THICKNESS (LAYER',I,') NOT FOUND IN GEOMETRY FILE'
        ENDIF
!
!       SUM THE TOTAL THICKNESS OF DEPOSIT (S3D_HDEP)
        DO IPOIN = 1,NPOIN2
          S3D_HDEP(IPOIN) = S3D_HDEP(IPOIN)+S3D_EPAI(IPOIN,I)
        ENDDO
!
      ENDDO
!
!     ELEVATION OF RIGID BED
      DO IPOIN = 1,NPOIN2
        ZR(IPOIN) = ZF(IPOIN)-S3D_HDEP(IPOIN)
      ENDDO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! TODO: STILL NEED TO CODE CONCENTRATION AND TIME OF CONSOLIDATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ELSEIF (S3D_ITASS.EQ.2) THEN
        ENDIF
      RETURN
      END
!
