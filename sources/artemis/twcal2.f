!                   *****************
                    SUBROUTINE TWCAL2
!                   *****************
!
!***********************************************************************
! ARTEMIS   V7P4                                     Nov 2017
!***********************************************************************
!
!brief    DISCRETISES AN ENERGY SPECTRUM IN NDALE x NPALE CELLS
!+                OF EQUAL ENERGY. THE RESULT IS A LIST OF
!+                DIRECTIONS CORRESPONDING TO EACH BAND AND
!+                FOR EACH LIQUID BOUNDARY NODE.
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   New. Computes BDALEs: spatially varying directional components
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BDALE          |<--| ISO ENERGY DIRECTIONS
!|                |   |     FOR EACH LIQUID BOUNDARY NODE
!| PMIN           |-->| MINIMUM FREQUENCY FOR SPECTRUM
!| PMAX           |-->| MAXIMUM FREQUENCY FOR SPECTRUM
!| TETMIN         |-->| MAXIMUM VALUE FOR THE PROPAGATION ANGLE
!| TETMAX         |-->| MAXIMUM VALUE FOR THE PROPAGATION ANGLE
!| NPALE          |-->| NUMBER OF DISCRETISATION BAND FOR PERIODS
!| NDALE          |-->| NUMBER OF DISCRETISATION BAND FOR DIRECTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_ARTEMIS
      USE DECLARATIONS_TELEMAC, ONLY : KINC,KSORT
      USE INTERFACE_ARTEMIS, ONLY : STWC1
      USE INTERFACE_PARALLEL, ONLY : P_MAX
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER                       :: IERR
      INTEGER                       :: IPTFR
      INTEGER                       :: JTMP,IMIN,IMAX
      INTEGER                       :: NPASF,NPASD
      INTEGER                       :: IDALE,IDD,IFF,I
      INTEGER                       :: ISPEC
!
      DOUBLE PRECISION              :: FMIN,FMAX
      DOUBLE PRECISION              :: DF,DTETA2
      DOUBLE PRECISION              :: SUMB,POIDS,SUMD,VAR,SUMICI
!
      INTEGER,ALLOCATABLE           :: IDTWC(:)
!
      DOUBLE PRECISION,ALLOCATABLE  :: SDIRTWC(:,:)
      DOUBLE PRECISION,ALLOCATABLE  :: SDIR(:)
!
      INTEGER                       :: IDDMINART
      DOUBLE PRECISION,ALLOCATABLE  :: DIR_ART(:)
      DOUBLE PRECISION,ALLOCATABLE  :: SDIR_ART(:)
!
      INTEGER                       :: PRINTMSG
!
!=======================================================================
!
!     MIN/MAX FREQUENCY
      FMIN = 1.D0 / PMAX
      FMAX = 1.D0 / PMIN
!
!     NUMBER OF INTEGRATION INTERVALS FOR THE TRAPEZOIDS METHOD : DIRECTIONS
      NPASD = 500*NDALE
!
!     NUMBER OF INTEGRATION INTERVALS FOR THE TRAPEZOIDS METHOD : FREQUENCIES
      NPASF = 500*NPALE
!
!     WIDTH OF AN INTEGRATION INTERVAL : DIRECTIONS
!     DTETA = (TETMAX-TETMIN)/FLOAT(NPASD)
!
!     WIDTH OF AN INTEGRATION INTERVAL : FREQUENCIES
      DF = (FMAX-FMIN)/FLOAT(NPASF)
!
!=======================================================================
!
      ALLOCATE(IDTWC(2*NDALE+2),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TWCAL2:IDTWC')
      ALLOCATE(SDIRTWC(NSPEC,NDIR+1),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TWCAL2:SDIRTWC')
      ALLOCATE(SDIR(NPASD+1),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TWCAL2:SDIR')
      ALLOCATE(DIR_ART(NPASD+1),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TWCAL2:DIR_ART')
      ALLOCATE(SDIR_ART(NPASD+1),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TWCAL2:SDIR_ART')
!
      PRINTMSG = 0
!
!-----------------------------------------------------------------------
!
!     INTEGRAL OF THE TOMAWAC SPECTRUM (TRAPEZOIDS METHOD)
!     FOR EACH DIRECTION COMPONENT
!
      DO ISPEC = 1,NSPEC
!
        DO IDD = 1,NDIR+1
          SUMD=0.D0
          DO IFF = 1,NPASF+1
!           IF FREQ AND/OR DIR ON THE BOUNDARY OF THE INTEGRATION DOMAIN :  CONTRIBUTION/2
            POIDS=1.D0
            IF(IFF.EQ.1 .OR. IFF.EQ.NPASF+1) POIDS=0.5D0*POIDS
            IF(IDD.EQ.1 .OR. IDD.EQ.NDIR+1)  POIDS=0.5D0*POIDS
            VAR=STWC1(FMIN+FLOAT(IFF-1)*DF,S_TOM%DIR(IDD),S_TOM,ISPEC)
            SUMD = SUMD + POIDS*VAR*DF
          ENDDO
          SDIRTWC(ISPEC,IDD) = SUMD
!         IF(DEBUG.GT.0) WRITE(LU,*) ISPEC,S_TOM%DIR(IDD),
!    &                               SDIRTWC(ISPEC,IDD)
        ENDDO
!
      ENDDO ! ISPEC = 1,NSPEC
      IF(DEBUG.GT.0)
     &    WRITE(LU,*) '< TWCAL2: SDIRTWC COMPUTED FOR 1 TO ',NSPEC
!
!-----------------------------------------------------------------------
!
!     FOR THE DIRECTION RANGE SPECIFIED IN ARTEMIS
!
!     INDICES COVERING THE RANGE
      IF(TETMAX-TETMIN.EQ.360.D0) THEN
        IMIN = 1
      ELSEIF(TETMIN.LE.0) THEN
        DO JTMP = 1,NDIR+1
          IF(S_TOM%DIR(JTMP).LE.TETMIN+360.D0) IMIN = JTMP
        ENDDO
      ELSE
        DO JTMP = 1,NDIR+1
          IF(S_TOM%DIR(JTMP).LE.TETMIN) IMIN = JTMP
        ENDDO
      ENDIF
!
      IF(TETMAX-TETMIN.EQ.360.D0) THEN
        IMAX = NDIR+1
      ELSEIF(TETMAX.LT.0) THEN
        DO JTMP = NDIR+1,1,-1
          IF(S_TOM%DIR(JTMP).GE.TETMAX+360.D0) IMAX = JTMP
        ENDDO
      ELSE
        DO JTMP = NDIR+1,1,-1
          IF(S_TOM%DIR(JTMP).GE.TETMAX) IMAX = JTMP
        ENDDO
      ENDIF
!
      IF(DEBUG.GT.0) WRITE(LU,*)'< TWCAL2: TETMIN,IMIN,S_TOM%DIR(IMIN)'
      IF(DEBUG.GT.0) WRITE(LU,*) TETMIN,IMIN,S_TOM%DIR(IMIN)
      IF(DEBUG.GT.0) WRITE(LU,*)'< TWCAL2: TETMAX,IMAX,S_TOM%DIR(IMAX)'
      IF(DEBUG.GT.0) WRITE(LU,*) TETMAX,IMAX,S_TOM%DIR(IMAX)
!
!=======================================================================
!
!     FOR EACH ARTEMIS BOUNDARY NODE
!
      DO IPTFR = 1 , NPTFR
!
        IF (LIHBOR%I(IPTFR).EQ.KINC .OR. LIHBOR%I(IPTFR).EQ.KSORT) THEN
!
          PRINTMSG = PRINTMSG + 1
!
!-----------------------------------------------------------------------
!
          IF(DEBUG.GT.0 .AND. PRINTMSG.EQ.1) THEN
            WRITE(LU,*)
            WRITE(LU,*) '> TWCAL2: ',
     &      'STARTS INTERPOLATION OF DIR. DISTRIBUTION TO ARTEMIS NODES'
          ENDIF
!
!     INTERPOLATES THE SPECTRUM FOR INCIDENT BOUNDARY POINTS, ONLY
!
          DO IDD = 1,NPASD+1
            SDIR(IDD) = 0.D0
          ENDDO
!
          CALL FASP_SP( S_TOM%XOUTER,S_TOM%YOUTER,SDIRTWC,NSPEC,
     &         X(MESH%NBOR%I(IPTFR)),Y(MESH%NBOR%I(IPTFR)),SDIR,IPTFR )
!
          CALL STWC2( IMIN,IMAX,NPASD+1,DIR_ART,SDIR )
          DTETA2 = DIR_ART(2)-DIR_ART(1)
!
          IF(DEBUG.GT.0 .AND. PRINTMSG.EQ.1)
     &     WRITE(LU,*) '< TWCAL2: INTERPOLATION TO ARTEMIS NODES ENDED'
!
!-----------------------------------------------------------------------
!
!     REORGANISE THE ARTEMIS SPECTRUM PRIOR TO SLICING
!
          IDDMINART = 1
          DO IDD = 2,NPASD
            IF(SDIR(IDD).LT.SDIR(IDDMINART)) IDDMINART = IDD
          ENDDO
!
!         REORGANISED SPECTRUM IN SDIR_ART(NPASD+1)
          DO IDD = 1,NPASD
            I=IDD-IDDMINART+1
            IF (IDD.LT.IDDMINART) I=I+NPASD
            SDIR_ART(I)=SDIR(IDD)
          ENDDO
          SDIR_ART(NPASD+1)=SDIR_ART(1)
!
!     INTEGRAL OF THE SPECTRUM (TRAPEZOIDS METHOD)
!
          SUMB = 0.D0
          DO IDD = 1,NPASD+1
            POIDS=1.D0
            IF(IDD.EQ.1 .OR. IDD.EQ.NPASD+1)  POIDS=0.5D0*POIDS
            SUMB = SUMB + POIDS*SDIR_ART(IDD)*DTETA2*DEGRAD
          ENDDO
!
!     SIGNIFICANT WAVE HEIGHT CORRESPONDING TO TOTAL ENERGY STORAGE
!
          HSCAL=4.D0*SQRT(SUMB)
!
          IF(DEBUG.GT.0 .AND. PRINTMSG.EQ.1) THEN
            WRITE(LU,*) '> TWCAL2: STARTS SPECTRUM DISCRETISATION'
            WRITE(LU,*) '           DIRECTIONS'
          ENDIF
!
!         =======================================================
!                              DIRECTIONS
!         =======================================================
!         DIVIDES THE SPECTRUM INTO 2*NDALE BANDS OF EQUAL ENERGY
          SUMB = SUMB/FLOAT(2*NDALE)
!
!         FIRST TERM OF DIRECTION DISCRETIZATION
          I=1
          IDTWC(I) = 1
!
!         IDENTIFIES THE ANGLES EVERY (I)*SUMB (I=1,NDALE)
          SUMICI = 0.D0
!
          DO IDD = 1,NPASD+1
            SUMICI = SUMICI + SDIR_ART(IDD)*DTETA2*DEGRAD
!
!           CHECKS IF SUMB = OVERALL SUMB/(2 NDALE) IS REACHED AND SAVES TETA
!
            IF (SUMICI.GE.SUMB*FLOAT(I) .OR. IDD.EQ.NPASD+1) THEN
              I=I+1
              IDTWC(I) = IDD
            ENDIF
          ENDDO
!
!         IDTWC HOLDS INDICES FOR THE DIRECTIONS TO BE CONSIDERED IN ARTEMIS
!
!         THESE ARE BASED ON CONTINUOUS SPECTRUM AND MAY NEED ADJUSTING,
!         IF PRINTED OUT FOR CHECKS, AS DONE FOR BDALE BELOW
!
          DO I=1,2*NDALE-1,2
            IDALE = (I+1)/2
            BDALE%ADR(IDALE)%P%R(IPTFR)=
     &               FLOAT(IDTWC(I+1)-1)*DTETA2+DIR_ART(IDDMINART)
            IF(BDALE%ADR(IDALE)%P%R(IPTFR).GT.DIR_ART(NPASD+1))
     &        BDALE%ADR(IDALE)%P%R(IPTFR)=
     &        BDALE%ADR(IDALE)%P%R(IPTFR)-DIR_ART(NPASD+1)+DIR_ART(1)
!
            IF(BDALE%ADR(IDALE)%P%R(IPTFR).GT.180D0)
     &        BDALE%ADR(IDALE)%P%R(IPTFR)=
     &        BDALE%ADR(IDALE)%P%R(IPTFR)-360.D0
          ENDDO
!
        ENDIF ! LIHBOR%I(IPTFR).EQ.KINC
!
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0 .AND. PRINTMSG.EQ.1)
     &  WRITE(LU,*) '< TWCAL2: SPECTRUM DISCRETISATION ENDED'
!
      ENDDO ! IPTFR = 1 , NPTFR
!
      DEALLOCATE(IDTWC,SDIRTWC,SDIR)
      DEALLOCATE(DIR_ART,SDIR_ART)
!
!-----------------------------------------------------------------------
!
!     FINDS BOUNDARY NODE CLOSEST TO REFERENCE POINT
!    (USED FOR PRINTOUTS ONLY IN ARTEMIS.F)
!
      IPTFR_REF = 0
      IF (NPTFR.GT.0) CALL TWCCLOSEST
      IF (NCSIZE.GT.1) IPTFR_REF = P_MAX(IPTFR_REF)
      IF(DEBUG.GT.0)
     &      WRITE(LU,*) 'CLOSEST: REFERENCE BOUNDARY POINT:',IPTFR_REF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
