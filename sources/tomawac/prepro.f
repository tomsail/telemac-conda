!                   *****************
                    SUBROUTINE PREPRO
!                   *****************
!
     &( CX    , CY    , IKLE2 , IFABOR, ELT   , ETA   , FRE   ,
     &  XK    , CG    , ITR01 , NPOIN3, NPOIN2, NELEM2, NDIRE ,
     &  NF    , COURAN)
!
!***********************************************************************
! TOMAWAC   V6P3                                   25/06/2012
!***********************************************************************
!
!brief    PREPARES ADVECTION.
!+
!+            COMPUTES THE ADVECTION FIELD; TRACES BACK THE
!+                CHARACTERISTICS.
!
!history  F. MARCOS (LNH)
!+        04/12/95
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
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2012
!+        V6P2
!+   Modifications : possibility of taking into account diffraction
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
!| FRE            |<->| NUMBER OF THE FREQUENCIES OF THE
!|                |   | POINTS TO BE ADVECTED
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID,
!|                |   | SOLID OR PERIODIC BOUNDARY
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| ISUB           |<->| ARRIVAL SUB-DOMAIN OF CHARACTERISTICS
!| ITR01          |<->| WORK TABLE
!| MESH           |-->| 2D MESH
!| MESH3D         |-->| 3D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPOIN3         |-->| NPOIN2*NDIRE
!| SHF            |<->| BARYCENTRIC COORDINATES ALONG F OF THE
!|                |   | NODES IN THEIR ASSOCIATED FREQUENCIES "FRE"
!| SHP            |<->| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHZ            |<->| BARYCENTRIC COORDINATES ALONG TETA OF THE
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_PREPRO => PREPRO
      USE DECLARATIONS_TELEMAC, ONLY : NAMECODE
!
      USE DECLARATIONS_TOMAWAC, ONLY : PROMIN, DEPTH, DT, STETA, SFR,
     &   SCT,SCF, SSHP1, SSHZ,  SSHF, IELM3, TB, MESH  , MESH3D, SISUB
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPOIN3,NPOIN2,NELEM2,NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF),CG(NPOIN2,NF)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN3,NF),ETA(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: FRE(*)
      INTEGER, INTENT(IN)    :: IKLE2(NELEM2,3)
      INTEGER, INTENT(INOUT) :: ITR01(NPOIN3,3),IFABOR(NELEM2,7)
      LOGICAL, INTENT(IN)    :: COURAN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CX,CY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JF,IEL,I1,I2,I3
      TYPE(BIEF_OBJ) :: BID
      TYPE(SLVCFG) :: SLVBID
      INTEGER :: SIZ_ISUB, SIZ_FRE, JF_ISUB, JF_FRE
      INTEGER, ALLOCATABLE :: TMP_ISUB(:)
!
!----------------------------------------------------------------------
!
      IF(.NOT.COURAN) THEN
!
!   -------------------------------------------------------------------
!
!   RELATIVE = ABSOLUTE => ADVECTION IN 3D
!   SEPARATES OUT THE FREQUENCIES
!
        DO JF=1,NF
!
!      ---------------------------------------------------------------
!
!      COMPUTES THE ADVECTION FIELD
!
          CALL CONWAC
     &( CX%R, CY%R, SCT%R, XK, CG, NPOIN2, NDIRE, JF, NF)
!
!      ----------------------------------------------------------------
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
          WRITE(LU,*) 'FREQUENCE :',JF
!
          IF(NCSIZE.GT.1) THEN
            SIZ_ISUB = NPOIN3
            JF_ISUB = JF
          ELSE
            SIZ_ISUB = 1
            JF_ISUB = 1
          ENDIF
          ! Manually creating contiguous temporary array
          ALLOCATE(TMP_ISUB(SIZ_ISUB))
          TMP_ISUB = SISUB%I((JF_ISUB-1)*SIZ_ISUB+1:JF_ISUB*SIZ_ISUB)
          CALL CHARAC(SSHZ%ADR(JF)%P,SSHZ%ADR(JF)%P,0,
     &            CX,CY,SCT,SCT,STETA,STETA,DT,MESH3D%IFABOR,IELM3,
     &            NPOIN2,NDIRE,1,1,.FALSE.,SSHP1%ADR(JF)%P,
     &            SSHZ%ADR(JF)%P,SSHZ%ADR(JF)%P,TB,
     &            ELT(1:NPOIN3,JF),ETA(1:NPOIN3,JF),ETA(1:NPOIN3,JF),
     &            ITR01(1:NPOIN3,1),
     &            TMP_ISUB,
     &            ITR01(1:NPOIN3,2),MESH3D,NELEM2,NELEM2,
     &            MESH%IKLE,
     &            MESH%SURDET,
     &            BID,BID,SLVBID,0.D0,.FALSE.,3,BID,1,
!                 A POSTERIORI INTERPOLATION
     &            .TRUE.,
!                 AND PERIODICITY
     &            .TRUE.)
          SISUB%I((JF_ISUB-1)*SIZ_ISUB+1:JF_ISUB*SIZ_ISUB) = TMP_ISUB
          DEALLOCATE(TMP_ISUB)
!
        ENDDO ! JF
!
      ELSE
!
!   ---------------------------------------------------------------
!
!   IN A RELATIVE REFERENCE SYSTEM => ADVECTION IN 4D
!   IT IS NO LONGER POSSIBLE TO SEPARATE THE FREQUENCIES OUT
!
        DO JF=1,NF
!
          CALL CONW4D(CX%R,CY%R,SCT%R,SCF%R, XK,CG,NPOIN2,NDIRE,
     &                JF,NF)
!
        ENDDO
!
        DO JF=1,NF
!
          IF(NCSIZE.GT.1) THEN
            SIZ_ISUB = NPOIN3
            JF_ISUB = JF
          ELSE
            SIZ_ISUB = 1
            JF_ISUB = 1
          ENDIF
          IF(COURAN.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
            SIZ_FRE = NPOIN3
            JF_FRE = JF
          ELSE
            SIZ_FRE = 1
            JF_FRE = 1
          ENDIF
          ! Manually creating contiguous temporary array
          ALLOCATE(TMP_ISUB(SIZ_ISUB))
          TMP_ISUB = SISUB%I((JF_ISUB-1)*SIZ_ISUB+1:JF_ISUB*SIZ_ISUB)
          CALL CHARAC(SSHZ%ADR(JF)%P,SSHZ%ADR(JF)%P,0,
     &                CX,CY,SCT,SCF,STETA,SFR,DT,MESH3D%IFABOR,IELM3,
     &                NPOIN2,NDIRE,JF,NF,.FALSE.,SSHP1%ADR(JF)%P,
     &                SSHZ%ADR(JF)%P,SSHF%ADR(JF)%P,TB,
     &                ELT(1:NPOIN3,JF),ETA(1:NPOIN3,JF),
     &                FRE((JF_FRE-1)*SIZ_FRE+1:JF_FRE*SIZ_FRE),
     &                ITR01(1:NPOIN3,1),
     &                TMP_ISUB,
     &                ITR01(1:NPOIN3,2),MESH3D,NELEM2,NELEM2,
     &                MESH%IKLE, MESH%SURDET,
     &                BID,BID,SLVBID,0.D0,.FALSE.,3,BID,1,
!                     A POSTERIORI INTERPOLATION
     &                .TRUE.,
!                     AND PERIODICITY
     &                .TRUE.,
!                     AND 4D
     &                .TRUE.)
          SISUB%I((JF_ISUB-1)*SIZ_ISUB+1:JF_ISUB*SIZ_ISUB) = TMP_ISUB
          DEALLOCATE(TMP_ISUB)
!
        ENDDO
!
      ENDIF
!
!----------------------------------------------------------------------
!
      RETURN
      END
