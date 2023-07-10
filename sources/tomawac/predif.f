!                       *****************
                        SUBROUTINE PREDIF
!                       *****************
!
     &(CX    , CY    , IKLE2 , IFABOR, ELT   , ETA   , XK    ,
     & CG    , ITR01 , NPOIN3, NPOIN2, NELEM2, NDIRE , NF    ,
     & COURAN, F     , RX    , RY    , RXX   , RYY   , NEIGB )
!
!***********************************************************************
! TOMAWAC   V6P3                                   25/06/2012
!***********************************************************************
!
!brief    PREPARES DIFFRACTION.
!+
!+            COMPUTES THE ADVECTION FIELD; TRACES BACK THE
!+                CHARACTERISTICS.
!
!history  E. KRIEZI (LNH)
!+        04/12/2006
!+        V5P5
!+
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2012
!+        V6P2
!+   Modification for V6P2
!+   Taking into account both Mean Sloe Equation model (Berkhoff,1972)
!+      and Revised Mild Slope Equation model (Porter,2003)
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        21/03/2013
!+        V6P3
!+   Call CONWAC added before call to DIFFRAC and DIFFRAC does only the
!+   modification of the velocities.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| CX             |<->| ADVECTION FIELD ALONG X(OR PHI)
!| CY             |<->| ADVECTION FIELD ALONG Y(OR LAMBDA)
!| ELT            |<->| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |<->| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| F              |<->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID,
!|                |   | SOLID OR PERIODIC BOUNDARY
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| ITR01          |<->| WORK TABLE
!| NEIGB          |-->| NEIGHBOUR POINTS FOR MESHFREE METHOD
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPOIN3         |-->| NPOIN2*NDIRE
!| RX             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RXX            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RY             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RYY            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| SHP            |<->| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHZ            |<->| BARYCENTRIC COORDINATES ALONG TETA OF THE
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| SINTET         |-->| SINE OF TETA ANGLE
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TRA01          |<->| WORK TABLE
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : PROMIN, STETA, SCT, DT, DEPTH, 
     &             MESH3D, MESH  ,SSHP1, SSHZ, TB, IELM3, ISUB, MAXNSP
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_PREDIF => PREDIF
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,INTENT(IN)    :: NPOIN3,NPOIN2,NELEM2,NDIRE,NF
      INTEGER,INTENT(IN)    :: IKLE2(NELEM2,3)
      INTEGER,INTENT(IN)    :: NEIGB(NPOIN2,MAXNSP)
      INTEGER,INTENT(INOUT) :: IFABOR(NELEM2,7)
      INTEGER,INTENT(INOUT) :: ELT(NPOIN3,NF), ETA(NPOIN3,NF)
      INTEGER,INTENT(INOUT) :: ITR01(NPOIN3,3)
      DOUBLE PRECISION,INTENT(IN) :: RX(MAXNSP,NPOIN2),RY(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: RXX(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: RYY(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: XK(NPOIN2,NF),CG(NPOIN2,NF)
      DOUBLE PRECISION,INTENT(IN) :: F(NPOIN2,NDIRE,NF)
      LOGICAL,INTENT(IN) :: COURAN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CX,CY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFF,IEL,I1,I2,I3
      TYPE(BIEF_OBJ) :: BID
      TYPE(SLVCFG)   :: SLVBID
      INTEGER :: SIZ_ISUB, JF_ISUB

!
!----------------------------------------------------------------------
!
      IF (.NOT.COURAN) THEN
!
!       RELATIVE = ABSOLUTE => ADVECTION IN 3D
!       SEPARATES OUT THE FREQUENCIES
!
        DO IFF=1,NF
!
!         COMPUTES THE ADVECTION FIELD
!
          CALL CONWAC
     &(CX%R, CY%R, SCT%R, XK, CG, NPOIN2, NDIRE, IFF, NF)
!
!         MODIFIESS THE ADVECTION FIELD WITH DIFFRACTION
!
          CALL DIFFRAC
     &  (CX%R, CY%R, SCT%R, XK, CG, NPOIN2, NDIRE, IFF, NF,
     &   F, RX, RY, RXX, RYY, NEIGB)
!
          DO IEL=1,NELEM2
            I1=IKLE2(IEL,1)
            I2=IKLE2(IEL,2)
            I3=IKLE2(IEL,3)
            IF(DEPTH(I1).LT.PROMIN.AND.DEPTH(I2).LT.PROMIN.AND.
     &         IFABOR(IEL,1).GT.0) IFABOR(IEL,1)=-1
            IF(DEPTH(I2).LT.PROMIN.AND.DEPTH(I3).LT.PROMIN.AND.
     &         IFABOR(IEL,2).GT.0) IFABOR(IEL,2)=-1
            IF(DEPTH(I3).LT.PROMIN.AND.DEPTH(I1).LT.PROMIN.AND.
     &         IFABOR(IEL,3).GT.0) IFABOR(IEL,3)=-1
          ENDDO
!
          WRITE(LU,*) 'FREQUENCE :',IFF
!
          IF(NCSIZE.GT.1) THEN
            SIZ_ISUB = NPOIN3
            JF_ISUB = IFF
          ELSE
            SIZ_ISUB = 1
            JF_ISUB = 1
          ENDIF
          CALL CHARAC(SSHZ%ADR(IFF)%P,SSHZ%ADR(IFF)%P,0,
     &                CX,CY,SCT,SCT,STETA,STETA,DT,MESH3D%IFABOR,IELM3,
     &                NPOIN2,NDIRE,1,1,.FALSE.,SSHP1%ADR(IFF)%P,
     &                SSHZ%ADR(IFF)%P,SSHZ%ADR(IFF)%P,TB,
     &                ELT(1,IFF),ETA(1,IFF),ETA(1,IFF),ITR01,
     &                ISUB((JF_ISUB-1)*SIZ_ISUB+1:JF_ISUB*SIZ_ISUB),
     &                ITR01(1,2),MESH3D,NELEM2,NELEM2,
     &                MESH%IKLE,
     &                MESH%SURDET,BID,BID,SLVBID,0.D0,.FALSE.,3,BID,1,
!                     A POSTERIORI INTERPOLATION
     &                .TRUE.,
!                     AND PERIODICITY
     &                .TRUE.)
!
        ENDDO !  IFF
!
      ELSE
!
!   ---------------------------------------------------------------
!
!   IN A RELATIVE REFERENCE SYSTEM => ADVECTION IN 4D
!   IT IS NO LONGER POSSIBLE TO SEPARATE THE FREQUENCIES OUT
        WRITE(LU,*) ''
        WRITE(LU,*) '***************************************'
        WRITE(LU,*) ' ATTENTION : DIFFRACTION IS NOT TAKEN  '
        WRITE(LU,*) ' INTO ACCOUNT IF CURRENTS OR VARYING   '
        WRITE(LU,*) ' WATER LEVELS ARE CONSIDERED           '
        WRITE(LU,*) ' ONE HAS TO CHOOSE BETWEEN CURRENT AND '
        WRITE(LU,*) ' DIFFRACTION                           '
        WRITE(LU,*) '***************************************'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!----------------------------------------------------------------------
!
      RETURN
      END
