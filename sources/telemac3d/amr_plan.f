!                         *******************
                          SUBROUTINE AMR_PLAN
!                         *******************
!
     &(ZVALS,OBJSOL,REFTYPE,NPOIN2,NPLAN,NSEG2,GLOSEG,DIMGLO,
     & Z0,NEWZ,ZEXT,INTSOL,MONITOR,SSMONITOR,SMONITOR,NEXTR,SNNEIGH,
     & NNEIGH,MCOEFF,MESH2D,MESH3D)
!
!***********************************************************************
! TELEMAC3D   V8P1
!***********************************************************************
!
!brief    This subroutine, called from CALCOT, is designed to adaptively
!+        determine vertical layer positions based on the gradient of the
!+        variable passed as OBJSOL. Layer positions are determined
!+        in each column of nodes individually, with some horizontal
!+        smoothing applied to maintain mesh quality.
!+
!+        The algorithm used is similar to the 'variable diffusion'
!+        approach of Winslow (1969), with some refinements loosely
!+        based on Tang & Tang (2003).
!
!history  Chris Cawthorn (HR-Wallingford) c.cawthorn@hrwallingford.co.uk
!+        28/03/2011
!+        V6P1
!+        Original version
!+
!
!history  J-M Hervouet (LNHE)
!+        25/05/2011
!+        V6P1
!+        Parallelism
!+
!
!history  J-M Hervouet (LNHE)
!+        12/06/2012
!+        V6P1
!+        MCOEFF now double precision
!+
!
!history  S.E. Bourban (HRW)
!+        28/07/2012
!+        V6P2
!+        Further development of the AMR method so it works with
!+        wetting and drying (bypassing method based on HMIN)
!+        Note: suppressed by JMH (HMIN not used and missing in call)
!+
!
!history  C-T Pham (LNHE)
!+        24/10/2017
!+        V7P3
!+        One argument has changed since V7P1 in MULT_INTERFACE_SEG,
!+        the penultimate one (before NSEG)
!+
!
!history  J-M Hervouet (jubilado)
!+        26/10/2017
!+        V7P3
!+        A number of nested loops inverted to minimise strides (not a
!+        digit changed in results).
!
!history  C-T Pham (LNHE)
!+        24/09/2019
!+        V8P1
!+        Change for the loop through segments in 2D mesh to find
!+        neighbours
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| GLOSEG         |-->| EDGE ADJACENCY ARRAY
!| INTSOL         |<->| OBJECTIVE VARIABLE INTERPOLATED ON NEW LAYERS
!|                |   | (WORK ARRAY)
!| MCOEFF         |<->| LOCAL MONITOR FUNCTION COEFFICIENT (WORK ARRAY)
!| MESH2D         |<->| MESH STRUCTURE OF 2D MESH
!| MESH3D         |<->| MESH STRUCTURE OF 3D MESH
!| MONITOR        |<->| MONITOR FUNCTION (WORK ARRAY)
!| NEWZ           |<->| REVISED VERTICAL LAYER POSITIONS
!| NEXTR          |<->| NUMBER OF EXTREMA IN WATER COLUMN (WORK ARRAY)
!| NNEIGH         |<->| NUMBER OF NEIGHBOURS OF POINT IN 2D (WORK ARRAY)
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN2         |-->| NUMBER OF NODES IN 2D
!| NSEG2          |-->| NUMBER OF EDGES IN 2D
!| OBJSOL         |-->| OBJECTIVE VARIABLE TO BE USED FOR MESH REFINEMENT
!| REFTYPE        |-->| TYPE OF MONITOR FUNCTION
!|                |   | (A=ARCLENGTH, C=CURVATURE)
!| SMONITOR       |<->| SMOOTHED MONITOR FUNCTION (WORK ARRAY)
!| SNNEIGH        |<->| STRUCTURE OF NNEIGH
!| SSMONITOR      |<->| STRUCTURE OF SMONITOR
!| Z0             |<->| ORIGINAL LAYER POSITIONS (WORK ARRAY)
!| ZEXT           |<->| POSITITIONS OF EXTREMA (WORK ARRAY)
!| ZVALS          |<->| VERTICAL LAYER POSITIONS
!|                |   | ON OUTPUT, ADAPTED LAYER POSITIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPLAN,NPOIN2,NSEG2,DIMGLO
      DOUBLE PRECISION, INTENT(IN)    :: OBJSOL(NPOIN2,NPLAN)
      CHARACTER,        INTENT(IN)    :: REFTYPE
      INTEGER,          INTENT(IN)    :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZVALS(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: Z0(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: NEWZ(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: ZEXT(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: INTSOL(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: MONITOR(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: SMONITOR(NPOIN2,NPLAN)
      INTEGER,          INTENT(INOUT) :: NEXTR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: NNEIGH(NPOIN2),MCOEFF(NPOIN2)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: SNNEIGH,SSMONITOR
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH2D,MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: IPOIN,IPLAN,JPLAN,P1,P2,ISEG,IEXTR,ITERS,LASTJ
      INTEGER NELEM,K,IELEM,ERR
      DOUBLE PRECISION :: DSDZ,MINDIST,ZL,ZR,SL,SR
!      DOUBLE PRECISION, POINTER :: COEF(:)
      LOGICAL, ALLOCATABLE :: YESNO(:)
!
! -----------------------------
!     HARD-CODED PARAMETERS
! -----------------------------
!
      DOUBLE PRECISION :: RR = 0.5D0 ! RELAXATION PARAMETER
      DOUBLE PRECISION :: S1,S2,S3   ! TEMPORARY VARIABLES
      INTEGER          :: MAX_ITERATIONS = 10
!
!-----------------------------------------------------------------------
!
!     GIVING A CORRECT STRUCTURE TO WORK ARRAYS
!
      CALL CPSTVC(MESH2D%X,SNNEIGH)
      CALL CPSTVC(MESH3D%X,SSMONITOR)
!
!     PREPARING AN EDGE-BASED COEFFICIENT = 0.5 ON INTERFACE SEGMENTS
!                                     AND = 1.  ELSEWHERE
!
      NELEM = MESH2D%NELEM
      ALLOCATE(YESNO(NSEG2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'YESNO')
!     INITIALIZATION OF YESNO
      DO K=1,NSEG2
        YESNO(K)=.FALSE.
      ENDDO
!
!      IF(NCSIZE.GT.1) THEN
!!       MEMORY SPACE TAKEN IN MESH2D EDGE-BASED WORK MATRIX
!        COEF=>MESH2D%MSEG%X%R(1:NSEG2)
!        DO ISEG=1,NSEG2
!          COEF(ISEG)=1.D0
!        ENDDO
!        CALL MULT_INTERFACE_SEG(COEF,MESH2D%NH_COM_SEG%I,
!     &                          MESH2D%NH_COM_SEG%DIM1,
!     &                          MESH2D%NB_NEIGHB_SEG,
!     &                          MESH2D%NB_NEIGHB_PT_SEG%I,
!     &                          MESH2D%LIST_SEND_SEG%I,NSEG2)
!      ENDIF
!
! --------------------------------------------------------------------
! COPY ZVALS TO Z0, OBJSOL TO INTSOL, AND ZVALS TO NEWZ
! --------------------------------------------------------------------
!
      DO IPLAN = 1,NPLAN
        DO IPOIN = 1,NPOIN2
          Z0(IPOIN,IPLAN)     = ZVALS(IPOIN,IPLAN)
          NEWZ(IPOIN,IPLAN)   = ZVALS(IPOIN,IPLAN)
          INTSOL(IPOIN,IPLAN) = OBJSOL(IPOIN,IPLAN)
        ENDDO                  ! IPOIN = 1,NPOIN2
      ENDDO                    ! IPLAN = 1,NPLAN
!
! --------------------------------------------------------------------
! FIND ANY LOCAL EXTREMA, AND RECORD THEIR LOCATIONS
! --------------------------------------------------------------------
!
      DO IPOIN = 1,NPOIN2
        NEXTR(IPOIN) = 0
        DO IPLAN = 2,NPLAN-1
          S1 = OBJSOL(IPOIN,IPLAN-1)
          S2 = OBJSOL(IPOIN,IPLAN)
          S3 = OBJSOL(IPOIN,IPLAN+1)
          IF((S1.LT.S2.AND.S3.LT.S2) .OR.
     &       (S1.GT.S2.AND.S3.GT.S2) ) THEN
            NEXTR(IPOIN) = NEXTR(IPOIN) + 1
            ZEXT(IPOIN,NEXTR(IPOIN)) = Z0(IPOIN,IPLAN)
          ENDIF                ! LOCAL EXTREMUM FOUND
        ENDDO                  ! IPLAN = 2,NPLAN-1
      ENDDO                    ! IPOIN = 1,NPOIN2
!
! --------------------------------------------------------------------
! FIND MONITOR COEFFICIENT (DEPENDS ON TYPE OF REFINEMENT)
! ARC LENGTH: MCOEFF = 100/MAX|DSDZ^2|
! CURVATURE : MCOEFF =  10/MAX|CURVATURE^2|
!
! NOW COMPUTES LOCAL COEFFCIENT BASED ON LOCAL WATER COLUMN
! --------------------------------------------------------------------
!
      IF(REFTYPE .EQ. 'A') THEN    ! REFINE BASED ON ARC LENGTH
        DO IPOIN = 1,NPOIN2
          MCOEFF(IPOIN) = 0.D0
          DO IPLAN = 1,NPLAN-1
            DSDZ = (OBJSOL(IPOIN,IPLAN+1)-OBJSOL(IPOIN,IPLAN)) /
     &             (ZVALS(IPOIN,IPLAN+1)-ZVALS(IPOIN,IPLAN))
            IF(ABS(DSDZ).GT.MCOEFF(IPOIN)) THEN
              MCOEFF(IPOIN) = ABS(DSDZ)
            ENDIF
          ENDDO               ! IPLAN = 1,NPLAN-1
          IF( MCOEFF(IPOIN).GT.0.01D0 ) THEN
            MCOEFF(IPOIN) = 100.D0/(MCOEFF(IPOIN)**2)
          ENDIF
        ENDDO                 ! IPOIN = 1,NPOIN2
      ELSEIF (REFTYPE .EQ. 'C') THEN
        WRITE(LU,*) 'AMR_PLAN: CURVATURE-BASED REFINEMENT NOT
     &               YET IMPLEMENTED'
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'AMR_PLAN: UNKNOWN REFINEMENT TYPE: ',REFTYPE
        CALL PLANTE(1)
        STOP
      ENDIF                    ! CASE SPLITTING ON REFINEMENT TYPE
!
! --------------------------------------------------------------------
! BEGIN ITERATIVE LOOP TO SOLVE (WZ')' = 0
! WITH MONITOR FUNCTION W = SQRT(1+MCOEFF*DSDX^2)         (ARC LENGTH)
!                   OR  W = SQRT(1+MCOEFF*CURVATURE^2)    (CURVATURE)
! --------------------------------------------------------------------
!
      DO ITERS=1,MAX_ITERATIONS
!
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       1. FIND MONITOR FUNCTION W(I,J) = W(Z(I,J+1/2))
!          BASED ON INTERPOLATED SOLUTION
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IF(REFTYPE.EQ.'A') THEN ! ARC LENGTH
          DO IPLAN = 1,NPLAN-1
            DO IPOIN = 1,NPOIN2
              DSDZ = (INTSOL(IPOIN,IPLAN+1)-INTSOL(IPOIN,IPLAN)) /
     &               (ZVALS(IPOIN,IPLAN+1)-ZVALS(IPOIN,IPLAN))
              MONITOR(IPOIN,IPLAN)=SQRT(1.D0+MCOEFF(IPOIN)*DSDZ**2)
            ENDDO
          ENDDO
        ELSEIF (REFTYPE.EQ.'C') THEN
          WRITE(LU,*) 'AMR_PLAN: CURVATURE-BASED REFINEMENT NOT
     &                         YET IMPLEMENTED'
          CALL PLANTE(1)
          STOP
        ELSE
          WRITE(LU,*) 'AMR_PLAN: UNKNOWN REFINEMENT TYPE: ',
     &                         REFTYPE
          CALL PLANTE(1)
          STOP
        ENDIF
!
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      2. SMOOTH MONITOR FUNCTION USING A LOW-PASS FILTER
!         FIRST IN VERTICAL, THEN IN HORIZONTAL.
!         (OR JUST BY LOOPING THROUGH EDGES?)
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!        < VERTICAL SMOOTHING >
!

        DO IPLAN = 2,NPLAN-2
          DO IPOIN = 1,NPOIN2
            SMONITOR(IPOIN,IPLAN) = 0.25D0*(MONITOR(IPOIN,IPLAN-1) +
     &                                    2*MONITOR(IPOIN,IPLAN  ) +
     &                                      MONITOR(IPOIN,IPLAN+1))
          ENDDO
        ENDDO
!
        DO IPLAN = 2,NPLAN-2
          DO IPOIN = 1,NPOIN2
            MONITOR(IPOIN,IPLAN) = SMONITOR(IPOIN,IPLAN)
          ENDDO
        ENDDO
!
!       < HORIZONTAL SMOOTHING >
!
        DO JPLAN = 1,20  ! 20 ITERATIONS
!
        DO IPOIN = 1,NPOIN2
          NNEIGH(IPOIN) = 0.D0
        ENDDO
!
        DO IPLAN = 1,NPLAN
          DO IPOIN = 1,NPOIN2
            SMONITOR(IPOIN,IPLAN) = 0.D0
          ENDDO
        ENDDO
!
!       LOOP THROUGH SEGMENTS IN 2D MESH TO FIND NEIGHBOURS
!       AND COMPUTE A WEIGHTED MEAN OF THE MONITOR FUNCTION ON EACH NODE
!
        IF(NCSIZE.GT.1) THEN
!          DO ISEG = 1,NSEG2
!            P1 = GLOSEG(ISEG,1)
!            P2 = GLOSEG(ISEG,2)
!!           A SEGMENT MAY APPEAR TWICE IN PARALLEL, HENCE COEF
!            NNEIGH(P1) = NNEIGH(P1)+COEF(ISEG)
!            NNEIGH(P2) = NNEIGH(P2)+COEF(ISEG)
!            DO IPLAN = 1,NPLAN
!              SMONITOR(P1,IPLAN) = SMONITOR(P1,IPLAN)
!     &                           + COEF(ISEG)*MONITOR(P2,IPLAN)
!              SMONITOR(P2,IPLAN) = SMONITOR(P2,IPLAN)
!     &                           + COEF(ISEG)*MONITOR(P1,IPLAN)
!            ENDDO
!          ENDDO
          DO IELEM=1,NELEM
            DO K=1,3
              ISEG = MESH2D%ELTSEG%I(IELEM+(K-1)*NELEM)
              IF(.NOT.YESNO(ISEG)) THEN
                P1 = GLOSEG(ISEG,1)
                P2 = GLOSEG(ISEG,2)
!               A SEGMENT MAY APPEAR TWICE IN PARALLEL
                IF(MESH2D%IFABOR%I(IELEM+(K-1)*NELEM).NE.-2) THEN
                  NNEIGH(P1) = NNEIGH(P1)+1.D0
                  NNEIGH(P2) = NNEIGH(P2)+1.D0
                  DO IPLAN = 1,NPLAN-1
                    SMONITOR(P1,IPLAN) = SMONITOR(P1,IPLAN)
     &                                 + MONITOR(P2,IPLAN)
                    SMONITOR(P2,IPLAN) = SMONITOR(P2,IPLAN)
     &                                 + MONITOR(P1,IPLAN)
                  ENDDO
                ELSE
                  NNEIGH(P1) = NNEIGH(P1)+0.5D0
                  NNEIGH(P2) = NNEIGH(P2)+0.5D0
                  DO IPLAN = 1,NPLAN-1
                    SMONITOR(P1,IPLAN) = SMONITOR(P1,IPLAN)
     &                                 + 0.5D0*MONITOR(P2,IPLAN)
                    SMONITOR(P2,IPLAN) = SMONITOR(P2,IPLAN)
     &                                 + 0.5D0*MONITOR(P1,IPLAN)
                  ENDDO
                ENDIF
                YESNO(ISEG)=.TRUE.
              ENDIF
            ENDDO
          ENDDO
!         THE POINT ITSELF (MAY APPEAR SEVERAL TIMES IN PARALLEL, HENCE IFAC)
          DO IPOIN = 1,NPOIN2
            NNEIGH(IPOIN)=NNEIGH(IPOIN)+MESH2D%IFAC%I(IPOIN)
            DO IPLAN = 1,NPLAN
              SMONITOR(IPOIN,IPLAN)=SMONITOR(IPOIN,IPLAN)
     &        + MONITOR(IPOIN,IPLAN)*MESH2D%IFAC%I(IPOIN)
            ENDDO
          ENDDO
!         PARALLEL GATHERING AT INTERFACE NODES
          CALL PARCOM(SNNEIGH  ,2,MESH2D)
          CALL PARCOM(SSMONITOR,2,MESH3D)
        ELSE
!          DO ISEG = 1,NSEG2
!            P1 = GLOSEG(ISEG,1)
!            P2 = GLOSEG(ISEG,2)
!            NNEIGH(P1) = NNEIGH(P1)+1.D0
!            NNEIGH(P2) = NNEIGH(P2)+1.D0
!            DO IPLAN = 1,NPLAN-1
!              SMONITOR(P1,IPLAN) = SMONITOR(P1,IPLAN)+MONITOR(P2,IPLAN)
!              SMONITOR(P2,IPLAN) = SMONITOR(P2,IPLAN)+MONITOR(P1,IPLAN)
!            ENDDO              ! IPLAN = 1,NPLAN-1
!          ENDDO                ! ISEG = 1,NSEG2
!
          DO IELEM=1,NELEM
            DO K=1,3
              ISEG = MESH2D%ELTSEG%I(IELEM+(K-1)*NELEM)
              IF(.NOT.YESNO(ISEG)) THEN
                P1 = GLOSEG(ISEG,1)
                P2 = GLOSEG(ISEG,2)
                NNEIGH(P1) = NNEIGH(P1)+1.D0
                NNEIGH(P2) = NNEIGH(P2)+1.D0
                DO IPLAN = 1,NPLAN-1
                  SMONITOR(P1,IPLAN) = SMONITOR(P1,IPLAN)
     &                               +  MONITOR(P2,IPLAN)
                  SMONITOR(P2,IPLAN) = SMONITOR(P2,IPLAN)
     &                               +  MONITOR(P1,IPLAN)
                ENDDO
                YESNO(ISEG)=.TRUE.
              ENDIF
            ENDDO
          ENDDO
!         THE POINT ITSELF
          DO IPOIN = 1,NPOIN2
            NNEIGH(IPOIN)=NNEIGH(IPOIN)+1.D0
            DO IPLAN = 1,NPLAN
              SMONITOR(IPOIN,IPLAN)=SMONITOR(IPOIN,IPLAN)
     &                             + MONITOR(IPOIN,IPLAN)
            ENDDO
          ENDDO
        ENDIF
!
!       FINAL AVERAGING
!
        DO IPLAN = 2,NPLAN-2
          DO IPOIN = 1,NPOIN2
            MONITOR(IPOIN,IPLAN)=SMONITOR(IPOIN,IPLAN)/NNEIGH(IPOIN)
          ENDDO
        ENDDO
!
        ENDDO  ! JPLAN = 1,20 (20 ITERATION OF HORIZONTAL SMOOTHING)
!
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      3. FIND NEW POINT LOCATIONS USING A SINGLE STEP
!         OF A GAUSS-SIEDEL TYPE METHOD
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        DO IPLAN = 2,NPLAN-1
          DO IPOIN = 1,NPOIN2
            NEWZ(IPOIN,IPLAN) = (1.D0-RR)*ZVALS(IPOIN,IPLAN) +
     &        RR*( MONITOR(IPOIN,IPLAN  )*ZVALS(IPOIN,IPLAN+1) +
     &             MONITOR(IPOIN,IPLAN-1)*ZVALS(IPOIN,IPLAN-1) ) /
     &            (MONITOR(IPOIN,IPLAN)+MONITOR(IPOIN,IPLAN-1))
          ENDDO
        ENDDO
!
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      4. FOR EACH EXTREMUM, MOVE NEAREST NEW MESH POINT
!         TO COINCIDE WITH LOCATION OF EXTREMUM
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        DO IPOIN = 1,NPOIN2
          DO IEXTR = 1,NEXTR(IPOIN)
            MINDIST = 1.D10
            JPLAN = 0
            DO IPLAN = 2,NPLAN-1
              IF( ABS(NEWZ(IPOIN,IPLAN)-ZEXT(IPOIN,IEXTR))
     &                                     .LT. MINDIST ) THEN
                MINDIST = ABS(NEWZ(IPOIN,IPLAN)-ZEXT(IPOIN,IEXTR))
                JPLAN = IPLAN
              ENDIF         ! POINT IPLAN IS NEAREST YET TO EXTREMUM
            ENDDO           ! IPLAN = 2,NPLAN-1
            NEWZ(IPOIN,JPLAN) = ZEXT(IPOIN,IEXTR)
          ENDDO               ! IEXTR = 1,NEXTR(IPOIN)
        ENDDO                 ! IPOIN = 1,NPOIN2
!
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      5. INTERPOLATE ORIGINAL SOLUTION AT NEW MESH POINTS
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        DO IPOIN = 1,NPOIN2
!
          LASTJ=1
          DO IPLAN = 1,NPLAN
!
!           ------------------------------
!           FIND INTERVAL CONTAINING POINT
!           ------------------------------
!
            DO JPLAN = LASTJ,NPLAN-1
              ZL = Z0(IPOIN,JPLAN)
              ZR = Z0(IPOIN,JPLAN+1)
              IF(ZL.LE.NEWZ(IPOIN,IPLAN).AND.
     &           NEWZ(IPOIN,IPLAN).LE.ZR) THEN
                SL = OBJSOL(IPOIN,JPLAN)
                SR = OBJSOL(IPOIN,JPLAN+1)
                DSDZ = (SR-SL)/(ZR-ZL)
                INTSOL(IPOIN,IPLAN) = SL + DSDZ*(NEWZ(IPOIN,IPLAN)-ZL)
                GO TO 1000
              ENDIF           ! INTERVAL FOUND
            ENDDO             ! JPLAN = 1,NPLAN
!           POINT NOT LOCATED
            WRITE(LU,*) 'AMR_PLAN: POINT OUTSIDE INTERVAL: ',
     &                  NEWZ(IPOIN,IPLAN)
            CALL PLANTE(1)
            STOP
1000        CONTINUE
            LASTJ=JPLAN
          ENDDO               ! IPLAN = 1,NPLAN
!
        ENDDO                 ! IPOIN = 1,NPOIN2
!
!      ~~~~~~~~~~~~~~~~~~~
!      6. SET ZVALS = NEWZ
!      ~~~~~~~~~~~~~~~~~~~
!
        DO IPLAN = 1,NPLAN
          DO IPOIN = 1,NPOIN2
            ZVALS(IPOIN,IPLAN) = NEWZ(IPOIN,IPLAN)
          ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO ! MAIN SUBITERATION LOOP
!
!-----------------------------------------------------------------------
!
! ZVALS NOW CONTAINS THE NEW LAYER POSITIONS
! INTSOL CONTAINS AN INTERPOLATION OF THE ORIGINAL SOLUTION
! ON THE NEW MESH POINTS (NOT NEEDED BY TELEMAC)
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(YESNO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

