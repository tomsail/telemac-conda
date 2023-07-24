!                   *****************************
                    SUBROUTINE GET_TOMSPEC_VALUE2
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
!+   Covers nesting option 2 only
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
      INTEGER                      :: IERR, I, IFF, IDD, ISPEC
      INTEGER                      :: NFIC, TYP
      INTEGER                      :: IDEC
      INTEGER                      :: RECORD
!
      DOUBLE PRECISION             :: EPS, DIST
      DOUBLE PRECISION,ALLOCATABLE :: X1(:),Y1(:),STOCK(:)
!
      CHARACTER(LEN=8)             :: FFORMAT
      CHARACTER(LEN=16),ALLOCATABLE :: VARNAME(:),VARUNIT(:)
!
      INTRINSIC DSQRT, DATAN, DATAN2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      ALLOCATE(VARNAME(SPEC%N),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_TOMSPEC_VALUE2:VARNAME')
      ALLOCATE(VARUNIT(SPEC%N),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_TOMSPEC_VALUE2:VARUNIT')
!
      ALLOCATE(X1(NF*NDIR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_TOMSPEC_VALUE2:X1')
      ALLOCATE(Y1(NF*NDIR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_TOMSPEC_VALUE2:Y1')
      ALLOCATE(STOCK(NDIR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GET_TOMSPEC_VALUE2:STOCK')
!
!=======================================================================
!
      NFIC = ART_FILES(WACSPE)%LU
      FFORMAT = ART_FILES(WACSPE)%FMT
!     THE SUPPORT MESH FOR TOMAWAC SPECTRA IS MADE OF QUADRANGLES
      TYP = QUADRANGLE_ELT_TYPE
!
!-----------------------------------------------------------------------
!
      IF(SPEC%N.GE.1) THEN
!
!       NODE NUMBER FOR EACH SPECTRUM/POINT IN SPEC
!
        CALL GET_DATA_VAR_LIST(FFORMAT,NFIC,SPEC%N,VARNAME,VARUNIT,IERR)
        CALL CHECK_CALL(IERR,'GET_TOMSPEC_VALUE2:GET_DATA_VAR_LIST')
        DO ISPEC=1,SPEC%N
          READ (VARNAME(ISPEC)(11:16),'(I8)') SPEC%NOUTER(ISPEC)
        ENDDO
!
!       WITH NESTING OPTION 2, GEOGRAPHICAL COORDINATES OF THE SPECTRAL POINTS
!       IN THE TOMAWAC OUTER MESH ARE REQUIRED FOR INTERPOLATION PURPOSES
!       AND TO IDENTIFY NODE CLOSEST TO REQUESTED REFERENCE POINT
!
        CALL XY_TOMAWAC(SPEC)
!
!       IDENTIFIES NODE CLOSEST TO REQUESTED REFERENCE POINT
!
        EPS = 3000.D0
        N_SFREF = 0
        DO ISPEC = 1,SPEC%N
          DIST = DSQRT((SPEC%XOUTER(ISPEC)-X_SFREF)**2 +
     &                 (SPEC%YOUTER(ISPEC)-Y_SFREF)**2 )
          IF(DIST.LT.EPS) THEN
            EPS = DIST
            N_SFREF = ISPEC
          ENDIF
        ENDDO
!
        IF(N_SFREF.EQ.0) THEN
          WRITE(LU,201) X_SFREF, Y_SFREF
          DEALLOCATE(X1,Y1,STOCK)
          DEALLOCATE(VARNAME,VARUNIT)
          CALL PLANTE(1)
          STOP
        ELSEIF(DEBUG.GT.0) THEN
          WRITE(LU,*) 'GET_TOMSPEC_VALUE2: N_SFREF:',N_SFREF
          WRITE(LU,*) '                  : NOUTER:',SPEC%NOUTER(N_SFREF)
        ENDIF
!
!-----------------------------------------------------------------------
!
!       "X AND Y COORDINATES" OF THE SPECTRA IN SPEC
!
        CALL GET_MESH_COORD(FFORMAT,NFIC,1,2,NF*NDIR,X1,IERR)
        CALL CHECK_CALL(IERR,'GET_TOMSPEC_VALUE2:GET_MESH_COORD:X')
        CALL GET_MESH_COORD(FFORMAT,NFIC,2,2,NF*NDIR,Y1,IERR)
        CALL CHECK_CALL(IERR,'GET_TOMSPEC_VALUE2:GET_MESH_COORD:Y')
!
!       COMPUTES THE DISCRETE FREQUENCIES FROM "X AND Y"
!
        DO I = 1,NF
          SPEC%FRE(I) = DSQRT(X1((I-1)*NDIR+1)**2+Y1((I-1)*NDIR+1)**2)
!         IF(DEBUG.GT.0) WRITE(*,*) I,SPEC%FRE(I)
        ENDDO
!
!       COMPUTES THE DISCRETE DIRECTIONS FROM "X AND Y"
!       CONVENTION: FROM X, COUNTER-CLOCKWISE, TOWARDS
!       THE CALCULATION MEANS THAT DIRECTIONS ARE NO LONGER IN TOMAWAC CONVENTION
!       BUT RATHER DIRECTLY IN ARTEMIS CONVENTION
!
        DO I = 1,NDIR
          SPEC%DIR(I) = DATAN2(Y1(I),X1(I))
          SPEC%DIR(I) = SPEC%DIR(I)/DEGRAD
!         IN RANGE [0;360]
          IF (SPEC%DIR(I).LT.0D0) THEN
            SPEC%DIR(I) = 360.D0 + SPEC%DIR(I)
          ENDIF
!         IF(DEBUG.GT.0) WRITE(*,*) I,SPEC%DIR(I)
        ENDDO
!
!       RE-ORDERS DIRECTIONS FROM 0 to 360
!       STARTS BY FINDING THE SMALLEST DIRECTION IN [0;360] => IDEC
        IDEC=0
        EPS=360.D0
        DO I = 1,NDIR
!         STOCKS THE DIRECTION
          STOCK(I)=SPEC%DIR(I)
          IF (SPEC%DIR(I).LT.EPS) THEN
            IDEC=I
            EPS=SPEC%DIR(I)
          ENDIF
        ENDDO
!
!       ORDERS SPEC%DIR FROM 0 to 360
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
!       ENERGY DENSITIES FOR EACH SPECTRUM/POINT IN SPEC, AND FOR RECORD TPSTWC
!
        CALL GET_DATA_TIMESTEP(FFORMAT,NFIC,RECORD,TPSTWC,IERR)
        CALL CHECK_CALL(IERR,'GET_TOMSPEC_VALUE2:GET_DATA_TIMESTEP')
!
        DO ISPEC=1,NSPEC
!         IF(DEBUG.GT.0) WRITE(LU,*) VARNAME(ISPEC)
          CALL GET_DATA_VALUE(FFORMAT,NFIC,RECORD,VARNAME(ISPEC),X1,
     &                        NF*NDIR,IERR)
          CALL CHECK_CALL(IERR,'GET_TOMSPEC_VALUE2:GET_DATA_VALUE')
!
          DO IFF=1,NF
            DO IDD=1,NDIR
              SPEC%ADR(ISPEC)%SOUTER(IFF,IDD) = X1((IFF-1)*NDIR+IDD)
            ENDDO
!           CLOSING THE LOOP
            SPEC%ADR(ISPEC)%SOUTER(IFF,NDIR+1) =
     &                    SPEC%ADR(ISPEC)%SOUTER(IFF,1)
          ENDDO
!
!         RE-ORDERS ENERGY DENSITIES
!         IN LINE WITH RE_ORDERING OF DIRECTIONS ABOVE
!
          DO IFF = 1,NF
            DO I = 1,NDIR
              STOCK(I)=SPEC%ADR(ISPEC)%SOUTER(IFF,I)
            ENDDO
            DO I = 1,NDIR
              IF (I.GT.IDEC) THEN
                IDD=NDIR+IDEC-I+1
              ELSE
                IDD=IDEC-I+1
              ENDIF
              SPEC%ADR(ISPEC)%SOUTER(IFF,IDD)=STOCK(I)
            ENDDO
            SPEC%ADR(ISPEC)%SOUTER(IFF,NDIR+1) =
     &                    SPEC%ADR(ISPEC)%SOUTER(IFF,1)
          ENDDO
!
        ENDDO  ! ISPEC=1,NSPEC
!
!=======================================================================
!
        DEALLOCATE(X1,Y1,STOCK)
        DEALLOCATE(VARNAME,VARUNIT)
!
      ELSE
        WRITE(LU,101) ART_FILES(WACSPE)%NAME
        DEALLOCATE(VARNAME,VARUNIT)
        CALL PLANTE(1)
        STOP
!
      ENDIF  ! (SPEC%N.GE.1)
!
!-----------------------------------------------------------------------
!
!     PRINTOUT FORMATS:
!
      WRITE(LU,301) TPSTWC
!
301   FORMAT(/,1X,'GET_TOMSPEC_VALUE2 : READING TIME STEP ',1F9.2,'S')
!
201   FORMAT(/,1X,'GET_TOMSPEC_VALUE2 : NO SPECTRAL POINT FOUND LESS',
     &       ' THAN 3KM FROM REFERENCE POINT (',1F9.2,';',1F9.2,')',/,
     &       'PLEASE REVIEW')
!
101   FORMAT(/,1X,'GET_TOMSPEC_VALUE2 : NO SPECTRAL POINT IN THE',
     &       ' TOMAWAC FILE ',1A30,';',/,
     &       'PLEASE REVIEW')
!
!-----------------------------------------------------------------------
!
!     USER DEFINED FUNCTION TOTNRJ FOR CHECKS ONLY
!
!     IF(DEBUG.GT.0) THEN
!       DO I=1,NSPEC
!         CALL TOTNRJ(SPEC,I)
!       ENDDO
!     ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
