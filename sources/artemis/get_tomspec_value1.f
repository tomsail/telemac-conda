!                   *****************************
                    SUBROUTINE GET_TOMSPEC_VALUE1
!                   *****************************
!
     &(SPEC)
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    READS IN THE TOMAWAC ENERGY SPECTRA.
!
!history  N.DURAND (HRW)
!+        09/03/2001
!+
!+   Original version
!
!history  T.ELLAM (HRW)
!+        13/03/2002
!+
!+   Modified to read in serafin format.spe
!
!history  N.DURAND (HRW)
!+        June 2014
!+
!+   Streamlined for V7P0
!
!history  N.DURAND (HRW)
!+        Feb 2017
!+        V7P2
!+   Revisited to use the hermes module
!+   Covers nesting option 1 only (original reading routines superseded)
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   DEGRAD now defined in DECLARATIONS_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SPEC           |<->| SPECTRUM STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SPECTRUM)   , INTENT(INOUT)        :: SPEC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                      :: IERR, I, IFF, IDD
      INTEGER                      :: NFIC
      INTEGER                      :: IDEC
      INTEGER                      :: RECORD
!
      DOUBLE PRECISION             :: EPS
      DOUBLE PRECISION,ALLOCATABLE :: X1(:),Y1(:),STOCK(:)
!
      CHARACTER(LEN=8)             :: FFORMAT
      CHARACTER(LEN=16),ALLOCATABLE :: VARNAME(:),VARUNIT(:)
!
      INTRINSIC DSQRT, DATAN2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      ALLOCATE(VARNAME(SPEC%N),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_TOMSPEC_VALUE1:VARNAME')
      ALLOCATE(VARUNIT(SPEC%N),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_TOMSPEC_VALUE1:VARUNIT')
!
      ALLOCATE(X1(NF*NDIR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_TOMSPEC_VALUE1:X1')
      ALLOCATE(Y1(NF*NDIR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_TOMSPEC_VALUE1:Y1')
      ALLOCATE(STOCK(NDIR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_TOMSPEC_VALUE1:STOCK')
!
!=======================================================================
!
      NFIC = ART_FILES(WACSPE)%LU
      FFORMAT = ART_FILES(WACSPE)%FMT
!
!-----------------------------------------------------------------------
!
!     ONLY ONE TOMAWAC SPECTRUM IS ALLOWED WHEN NESTING OPTION 1
!
!     IF MORE THAN ONE FOUND IN SPECTRUM FILE, WARNS THE USER THAT
!     THE FIRST IN FILE IS CONSIDERED
!
      IF(SPEC%N.GT.1) THEN
        WRITE(LU,*) 'WARNING: TOO MANY VARIABLES IN ',
     &         ART_FILES(WACSPE)%NAME,
     &         'ONLY THE FIRST VARIABLE IN THE FILE WILL BE READ'
      ENDIF
!
!-----------------------------------------------------------------------
!
!     NODE NUMBER FOR EACH SPECTRUM/POINT IN SPEC
!
      CALL GET_DATA_VAR_LIST(FFORMAT,NFIC,SPEC%N,VARNAME,VARUNIT,IERR)
      CALL CHECK_CALL(IERR,'GET_TOMSPEC_VALUE1:GET_DATA_VAR_LIST')
!     THE 1ST SPECTRUM/POINT IN SPEC EVEN IF THERE ARE MORE THAN 1
      READ (VARNAME(1)(9:16),'(I8)') SPEC%NOUTER(1)
!
!-----------------------------------------------------------------------
!
!     "X AND Y COORDINATES" OF THE SPECTRA IN SPEC
!
      CALL GET_MESH_COORD(FFORMAT,NFIC,1,2,NF*NDIR,X1,IERR)
      CALL CHECK_CALL(IERR,'GET_TOMSPEC_VALUE1:GET_MESH_COORD:X')
      CALL GET_MESH_COORD(FFORMAT,NFIC,2,2,NF*NDIR,Y1,IERR)
      CALL CHECK_CALL(IERR,'GET_TOMSPEC_VALUE1:GET_MESH_COORD:Y')
!
!     COMPUTES THE DISCRETE FREQUENCIES FROM "X AND Y"
!
      DO I = 1,NF
        SPEC%FRE(I) = DSQRT(X1((I-1)*NDIR+1)**2+Y1((I-1)*NDIR+1)**2)
!       IF(DEBUG.GT.0) WRITE(*,*) SPEC%FRE(I)
      ENDDO
!
!     COMPUTES THE DISCRETE DIRECTIONS FROM "X AND Y"
!     CONVENTION: FROM X, COUNTER-CLOCKWISE, TOWARDS
!     THE CALCULATION MEANS THAT DIRECTIONS ARE NO LONGER IN TOMAWAC CONVENTION
!     BUT RATHER DIRECTLY IN ARTEMIS CONVENTION
!
      DO I = 1,NDIR
        SPEC%DIR(I) = DATAN2(Y1(I),X1(I))
        SPEC%DIR(I) = SPEC%DIR(I)/DEGRAD
!       IN RANGE [0;360]
        IF (SPEC%DIR(I).LT.0D0) THEN
          SPEC%DIR(I) = 360.D0 + SPEC%DIR(I)
        ENDIF
!       IF(DEBUG.GT.0) WRITE(*,*) SPEC%DIR(I)
      ENDDO
!
!     RE-ORDERS DIRECTIONS FROM 0 to 360 WHEN NESTING OPTION 1
!     STARTS BY FINDING THE SMALLEST DIRECTION IN [0;360] => IDEC
      IDEC=0
      EPS=360.D0
      DO I = 1,NDIR
!       STOCKS THE DIRECTION
        STOCK(I)=SPEC%DIR(I)
        IF (SPEC%DIR(I).LT.EPS) THEN
          IDEC=I
          EPS=SPEC%DIR(I)
        ENDIF
      ENDDO
!
!     ORDERS SPEC%DIR FROM 0 to 360
      DO I = 1,NDIR
        IF (I.GT.IDEC) THEN
          IDD=NDIR+IDEC-I+1
        ELSE
          IDD=IDEC-I+1
        ENDIF
        SPEC%DIR(IDD)=STOCK(I)
      ENDDO
      SPEC%DIR(NDIR+1)=SPEC%DIR(1)+360.D0
!
!-----------------------------------------------------------------------
!
!     ENERGY DENSITIES FOR 1ST SPECTRUM/POINT IN SPEC ( HENCE ADR(1) ),
!     AND FOR RECORD TPSTWC
!
      CALL GET_DATA_TIMESTEP(FFORMAT,NFIC,RECORD,TPSTWC,IERR)
      CALL CHECK_CALL(IERR,'GET_TOMSPEC_VALUE1:GET_DATA_TIMESTEP')
!
      CALL GET_DATA_VALUE(FFORMAT,NFIC,RECORD,VARNAME(1),X1,
     &                    NF*NDIR,IERR)
      CALL CHECK_CALL(IERR,'GET_TOMSPEC_VALUE1:GET_DATA_VALUE')
!
      DO IFF=1,NF
        DO IDD=1,NDIR
          SPEC%ADR(1)%SOUTER(IFF,IDD) = X1((IFF-1)*NDIR+IDD)
        ENDDO
!       CLOSING THE LOOP
        SPEC%ADR(1)%SOUTER(IFF,NDIR+1) = SPEC%ADR(1)%SOUTER(IFF,1)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     RE-ORDERS ENERGY DENSITIES WHEN NESTING OPTION 1
!     IN LINE WITH RE_ORDERING OF DIRECTIONS ABOVE
!
      DO IFF = 1,NF
        DO I = 1,NDIR
          STOCK(I)=SPEC%ADR(1)%SOUTER(IFF,I)
        ENDDO
        DO I = 1,NDIR
          IF (I.GT.IDEC) THEN
            IDD=NDIR+IDEC-I+1
          ELSE
            IDD=IDEC-I+1
          ENDIF
          SPEC%ADR(1)%SOUTER(IFF,IDD)=STOCK(I)
        ENDDO
        SPEC%ADR(1)%SOUTER(IFF,NDIR+1)=SPEC%ADR(1)%SOUTER(IFF,1)
      ENDDO
!
!=======================================================================
!
      DEALLOCATE(X1,Y1,STOCK)
      DEALLOCATE(VARNAME,VARUNIT)
!
!-----------------------------------------------------------------------
!
!     PRINTOUT FORMATS:
!
      WRITE(LU,301) TPSTWC
!
301   FORMAT(/,1X,'GET_TOMSPEC_VALUE1 : READING TIME STEP ',1F9.2,'S')
!
!-----------------------------------------------------------------------
!
!     USER DEFINED FUNCTION TOTNRJ FOR CHECKS ONLY
!
!     IF(DEBUG.GT.0) CALL TOTNRJ(SPEC,1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
