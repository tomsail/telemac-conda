!                   *****************
                    SUBROUTINE TWCALE
!                   *****************
!
     &(ISPEC)
!
!***********************************************************************
! ARTEMIS   V7P4                                     Nov 2017
!***********************************************************************
!
!brief    DISCRETISES AN ENERGY SPECTRUM IN NDALE x NPALE CELLS
!+                OF EQUAL ENERGY. THE RESULT IS A LIST OF
!+                DIRECTIONS CORRESPONDING TO EACH BAND AND
!+                A MATRIX GIVING PERIODS FOR EACH CELL.
!
!history  C.PEYRARD
!+        07/2014
!+        V7P0
!+  creation
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+  1. tidying up
!+  2. DEGRAD now defined in DECLARATIONS_ARTEMIS
!
!history  N.DURAND (HRW)
!+        November 2017
!+        V7P4
!+  Added IPSEC as argument to TWCALE to extend use to CHAINTWC=2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DALE           |<--| ISO ENERGY DIRECTIONS
!| PDALE          |<--| MATRIX : GIVES ISO-ENERGY FREQUENCIES
!|                |   |          FOR EACH ISO-ENERGY DIRECTION
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
      USE INTERFACE_ARTEMIS, ONLY : STWC1
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: ISPEC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER                       :: NPASF,NPASD
      INTEGER                       :: IDALE,JPALE,IDD,IFF,I,J
!
      DOUBLE PRECISION              :: FMIN,FMAX
      DOUBLE PRECISION              :: DTETA,DF
      DOUBLE PRECISION              :: SUMB,POIDS,SUMD,VAR,SUMICI
!
      INTEGER                       :: IERR
      INTEGER,ALLOCATABLE           :: IDTWC(:)
      DOUBLE PRECISION,ALLOCATABLE  :: FTWC(:)
!
!-----------------------------------------------------------------------
!
      ALLOCATE(IDTWC(2*NDALE+2),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TWCALE:IDTWC')
      ALLOCATE(FTWC(2*NPALE+2),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TWCALE:FTWC')
!
!-----------------------------------------------------------------------
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
      DTETA = (TETMAX-TETMIN)/FLOAT(NPASD)
!
!     WIDTH OF AN INTEGRATION INTERVAL : FREQUENCIES
      DF = (FMAX-FMIN)/FLOAT(NPASF)
!
!     SIGNIFICANT WAVE HEIGHT INITIALISATION
      HSCAL = 0.D0
!
!-----------------------------------------------------------------------
!
!     INTEGRAL OF THE SPECTRUM (TRAPEZOIDS METHOD)
!
      SUMB = 0.D0
      DO IDD = 1,NPASD+1
        SUMD=0.D0
        DO IFF = 1,NPASF+1
!         IF FREQ AND/OR DIR ON THE BOUNDARY OF THE INTEGRATION DOMAIN :  CONTRIBUTION/2
          POIDS=1.D0
          IF(IFF.EQ.1 .OR. IFF.EQ.NPASF+1) POIDS=0.5D0*POIDS
          IF(IDD.EQ.1 .OR. IDD.EQ.NPASD+1) POIDS=0.5D0*POIDS
          VAR=STWC1(FMIN+FLOAT(IFF-1)*DF,TETMIN+FLOAT(IDD-1)*DTETA,
     &              S_TOM,ISPEC)
          SUMD = SUMD + POIDS*VAR*DF
        ENDDO
        SUMB = SUMB + SUMD*DTETA*DEGRAD
      ENDDO
!
!     SIGNIFICANT WAVE HEIGHT CORRESPONDING TO TOTAL ENERGY STORAGE
      HSCAL=4.D0*SQRT(SUMB)
!
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0) THEN
        WRITE(LU,*) '> TWCALE: STARTS SPECTRUM DISCRETISATION'
        WRITE(LU,*) '          DIRECTIONS'
      ENDIF
!
!     =======================================================
!                          DIRECTIONS
!     =======================================================
!     DIVIDES THE SPECTRUM INTO 2*NDALE BANDS OF EQUAL ENERGY
      SUMB = SUMB/FLOAT(2*NDALE)
!
!     FIRST TERM OF DIRECTION DISCRETIZATION
      I=1
      IDTWC(I) = 1
!
!     IDENTIFIES THE ANGLES EVERY (I)*SUMB (I=1,NDALE)
      SUMICI = 0.D0
!
      DO IDD = 1,NPASD+1
        SUMD=0.D0
        DO IFF = 1,NPASF+1
!         IF FREQ AND/OR DIR ON THE BOUNDARY OF THE INTEGRATION DOMAIN :  CONTRIBUTION/2
          POIDS=1.D0
          IF(IFF.EQ.1 .OR. IFF.EQ.NPASF+1) POIDS=0.5D0*POIDS
          IF(IDD.EQ.1 .OR. IDD.EQ.NPASD+1) POIDS=0.5D0*POIDS
          VAR=STWC1(FMIN+FLOAT(IFF-1)*DF,TETMIN+FLOAT(IDD-1)*DTETA,
     &              S_TOM,ISPEC)
          SUMD = SUMD + POIDS*VAR*DF
        ENDDO
        SUMICI = SUMICI + SUMD*DTETA*DEGRAD
!
!       CHECKS IF SUMB = OVERALL SUMB/(2 NDALE) IS REACHED AND SAVES TETA
!
        IF (SUMICI.GE.SUMB*FLOAT(I) .OR. IDD.EQ.NPASD+1) THEN
          I=I+1
          IDTWC(I) = IDD
        ENDIF
      ENDDO
!
!     IDTWC HOLDS INDICES FOR THE DIRECTIONS TO BE CONSIDERED IN ARTEMIS:
!     TETMIN   T1   Ts1   T2   Ts2     ......... Tn Tmax
!     TETMIN -> Ts1,Ts2... : SUMB/NDALE limit
!        &      T1,T2...   : mean direction to be computed by ARTEMIS
!
      DO I=1,2*NDALE-1,2
        IDALE = (I+1)/2
        DALE%R(IDALE)=TETMIN+FLOAT(IDTWC(I+1)-1)*DTETA
      ENDDO
!
!----------------------------------------------------------------------
!
      IF(DEBUG.GT.0) WRITE(LU,*) '          FREQUENCIES'
!
!     =======================================================
!                          FREQUENCIES
!     =======================================================
!     DIVIDES THE SPECTRUM/NDALE (EACH DIRECTION SLICE) INTO 2*NPALE BANDS OF EQUAL ENERGY
!     REMEMBERING THAT SUMB = SPECTRUM/(2 NDALE)
      SUMB = 2.D0*SUMB/FLOAT(2*NPALE)
!
!     IDENTIFIES THE FREQUENCIES EVERY (I)*SUMB (I=1,NPALE)
      I=1
      IDALE=1
!
!     FOR EACH DIRECTION DOMAIN
!
98    CONTINUE
      SUMICI = 0.D0
!
      J=1
      FTWC(J) = FMIN
      DO IFF = 1,NPASF+1
        SUMD=0.D0
        DO IDD = IDTWC(I),IDTWC(I+2)
!         IF FREQ AND/OR DIR ON THE BOUNDARY OF THE INTEGRATION DOMAIN :  CONTRIBUTION/2
          POIDS=1.D0
          IF (IFF.EQ.1 .OR. IFF.EQ.NPASF+1) POIDS=0.5D0*POIDS
          IF (IDD.EQ.1 .OR. IDD.EQ.NPASD+1) POIDS=0.5D0*POIDS
          VAR=STWC1(FMIN+FLOAT(IFF-1)*DF,TETMIN+FLOAT(IDD-1)*DTETA,
     &              S_TOM,ISPEC)
          SUMD = SUMD + POIDS*VAR*DTETA*DEGRAD
        ENDDO
        SUMICI = SUMICI + SUMD*DF
!
!       CHECKS IF SUMB = OVERALL SUMB/NDALE /(2 NPALE) IS REACHED AND SAVES TETA
!
        IF (SUMICI.GE.SUMB*FLOAT(J) .OR. IFF.EQ.NPASF+1) THEN
          J=J+1
          FTWC(J) = FMIN+FLOAT(IFF-1)*DF
        ENDIF
      ENDDO
!
!     STOCKS PERIODS IN A LINE OF PDALE
!
      DO JPALE=1,NPALE
        PDALE%R((IDALE-1)*NPALE+(NPALE-JPALE+1)) = 1.D0/FTWC(2*JPALE)
      ENDDO
!
      IF(I.LT.(2*NDALE-1)) THEN
!     GO TO NEXT DIRECTION
        I    = I    +2
        IDALE= IDALE+1
        GOTO 98
      ENDIF
!
!     PDALE HOLDS :
!     DIRECTION1 :  T11   T12   T13  .....
!     DIRECTION2 :  T21   T22   T23  .....
!        .
!        .
!        .
!     (DIRECTION i is given in DALE)
!
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0)
     &  WRITE(LU,*) '< TWCALE: SPECTRUM DISCRETISATION ENDED'
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(IDTWC,FTWC)
!
      RETURN
      END
