!                   *****************
                    SUBROUTINE VELRES
!                   *****************
!
     &(U,V,W,DP,PX,PY,PZ,MSK,MASKEL,MESH3D,S,IELM3,OPTBAN,
     & UNSVOL,DO_UNSVOL,NPOIN3,NPOIN2,SIGMAG,IPBOT,AGGLOH,KSORT,NPTFR3,
     & LIUBOL,CONCOR)
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief    COMPUTES THE FINAL, SOLENOIDAL VELOCITY FIELD (UE, VE, WE)
!+                GIVEN THE DYNAMIC PRESSURE P AND THE INTERMEDIATE
!+                VELOCITY FIELD (UP, VP, WP).
!code
!+                   DPART DP
!+   UE_I = UP_I +  -----------
!+                   DPART X_I
!+
!+   U,V,W ARE UP AT THE BEGINNING
!+   U,V,W ARE UE AT THE END
!+
!+
!+   I.E. REALISES THE FINAL VELOCITY PROJECTION STEP
!+   NOTE: PHYSICAL DYNAMIC PRESSURE IS DP  MULTIPLIED BY RHO0/DT
!+        (NEEDED FOR OUTPUTS)
!
!history  JACEK A. JANKOWSKI - UNIVERSITAET HANNOVER
!+        12/98 - 04/99
!+
!+   NON-HYDROSTATIC VERSION
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
!history  J-M HERVOUET (LNHE)
!+        06/04/2012
!+        V6P2
!+   Unsv3d changed into unsvol and recomputed here with Z, not ZPROP
!+   Accordingly IPBOT is redone before with Z.
!
!history  J-M HERVOUET (LNHE)
!+        27/06/2016
!+        V7P2
!+   New correction on boundaries: free velocities are changed to get
!+   a better divergence after the Chorin algorithm.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CONCOR         |-->| IF YES, CONTINUITY CORRECTION ON OPEN BOUNDARIES
!| DO_UNSVOL      |-->| IF YES, UNSVOL IS COMPUTED, IF NOT, IT IS GIVEN
!| DP             |-->| HYDRODYNAMIC PRESSURE, TIMESTEP N+1
!| IELM3          |-->| TYPE OF ELEMENT
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| KSORT          |-->| CONVENTION FOR FREE VELOCITIES
!| LIUBOL         |-->| LATERAL BOUNDARY CONDITIONS OF VELOCITIES
!| MASKEL         |<->| ELEMENT MASKING
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| MASKING LOGICAL FLAG
!|                |   | IF YES, THERE IS MASKED ELEMENTS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NPTFR3         |-->| NUMBER OF 3D BOUNDARY POINTS
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| PX             |<->| H-DYN. PRESSURE PARTIAL DERIVATIVES
!| PY             |<->| H-DYN. PRESSURE PARTIAL DERIVATIVES
!| PZ             |<->| H-DYN. PRESSURE PARTIAL DERIVATIVES
!| S              |-->|
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!| U              |<->| COMPONENT OF VELOCITY
!| UNSVOL         |<->| INVERSE OF VOLUME OF BASIS FUNCTIONS
!| V              |<->| COMPONENT OF VELOCITY
!| W              |<->| COMPONENT OF VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: IELM3,OPTBAN,NPOIN3,NPOIN2
      INTEGER, INTENT(IN)            :: KSORT,NPTFR3
      DOUBLE PRECISION, INTENT(IN)   :: AGGLOH
      TYPE(BIEF_OBJ),  INTENT(IN)    :: S,DP,LIUBOL
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: UNSVOL,PX,PY,PZ,U,V,W
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH3D
      INTEGER, INTENT(IN)            :: IPBOT(NPOIN2)
      LOGICAL, INTENT(IN)            :: MSK,SIGMAG,DO_UNSVOL,CONCOR
!
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: MASKEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,I2D,I3D,I3DP,IPTFR3
      DOUBLE PRECISION MU
      CHARACTER(LEN=15) FORMUL
!
!-----------------------------------------------------------------------
! DYNAMIC PRESSURE DERIVATIVES AT NODES
!-----------------------------------------------------------------------
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
!       HERE GRADIENTS WITH FILTERING OF CRUSHED ELEMENTS
        FORMUL='GRADF 2        '
      ELSE
!       STANDARD GRADIENTS
        FORMUL='GRADF          '
      ENDIF
!
      CALL VECTOR(PX,'=',FORMUL//'X',IELM3,1.D0,DP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(PY,'=',FORMUL//'Y',IELM3,1.D0,DP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(PZ,'=',FORMUL//'Z',IELM3,1.D0,DP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
!
!-----------------------------------------------------------------------
!
      IF(DO_UNSVOL) THEN
        CALL VECTOR(UNSVOL, '=', 'MASBAS          ',IELM3,1.D0-AGGLOH,
     &              S,S,S,S,S,S,MESH3D,.FALSE.,MASKEL)
        IF(AGGLOH.GT.1.D-6) THEN
          CALL VECTOR(UNSVOL, '+', 'MASBAS2         ',IELM3,AGGLOH,
     &                S,S,S,S,S,S,MESH3D,.FALSE.,MASKEL)
        ENDIF
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(UNSVOL,2,MESH3D)
          CALL OS('X=1/Y   ',X=UNSVOL,Y=UNSVOL,
     &                                    IOPT=2,INFINI=1.D6,ZERO=1.D-6)
        ELSE
          CALL OS('X=1/Y   ',X=UNSVOL,Y=UNSVOL,
     &            IOPT=2,INFINI=1.D6,ZERO=1.D-6)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM (PX, 2, MESH3D)
        CALL PARCOM (PY, 2, MESH3D)
        CALL PARCOM (PZ, 2, MESH3D)
      ENDIF
!
!-----------------------------------------------------------------------
! FINAL VELOCITY ( PROJECTION )
!-----------------------------------------------------------------------
!
      DO I3D=1,NPOIN3
        U%R(I3D)=U%R(I3D)-PX%R(I3D)*UNSVOL%R(I3D)
        V%R(I3D)=V%R(I3D)-PY%R(I3D)*UNSVOL%R(I3D)
        W%R(I3D)=W%R(I3D)-PZ%R(I3D)*UNSVOL%R(I3D)
      ENDDO
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        DO I2D=1,NPOIN2
          IF(IPBOT(I2D).GT.0) THEN
            I3DP=I2D+IPBOT(I2D)*NPOIN2
!           VALUE OF THE FIRST FREE POINT IS COPIED BELOW
            DO IP=0,IPBOT(I2D)-1
              I3D=I2D+IP*NPOIN2
              U%R(I3D)=U%R(I3DP)
              V%R(I3D)=V%R(I3DP)
              W%R(I3D)=W%R(I3DP)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!     CORRECTING THE FREE VELOCITIES FOR A BETTER DIVERGENCE
!
!     NOTE THAT HERE NO SOURCE OF DIVERGENCE IS CONSIDERED
!     THIS IS TO BE COMPLETED (CASE OF RAIN...)
!
      IF(CONCOR) THEN
        CALL VECTOR(PX,'=',FORMUL//'X', IELM3,1.D0,
     &              U,U,U,U,U,U,MESH3D,MSK,MASKEL)
        CALL VECTOR(PX,'+',FORMUL//'Y', IELM3,1.D0,
     &              V,V,V,V,V,V,MESH3D,MSK,MASKEL)
        CALL VECTOR(PX,'+',FORMUL//'Z', IELM3,1.D0,
     &              W,W,W,W,W,W,MESH3D,MSK,MASKEL)
        CALL VECTOR(PY,'=','FLUBOR          ',IELBOR(IELM3,2),
!                              U,V : THESE 2 ARE USED ONLY
     &              1.D0,U,U,U,U,V,V,MESH3D,.FALSE.,MASKEL)
!                   NOT THE GOOD SIZE BUT NOT USED: MASKEL
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(PX,2,MESH3D)
          CALL PARCOM_BORD(PY%R,2,MESH3D)
        ENDIF
        DO IPTFR3=1,NPTFR3
!         CORRECTING FREE VELOCITIES
          IF(LIUBOL%I(IPTFR3).EQ.KSORT) THEN
            IF(ABS(PY%R(IPTFR3)).GT.1.D-10) THEN
              I3D=MESH3D%NBOR%I(IPTFR3)
!             HERE WE ASSUME THAT THE LOCAL VELOCITY IS THE ONLY CONTRIBUTING
!             TO THE VALUE OF FLUBOR, WRONG BUT ENOUGH EFFICIENT TO PREVENT
!             SPURIOUS VELOCITIES...
              MU=(PY%R(IPTFR3)-PX%R(I3D))/PY%R(IPTFR3)
              U%R(I3D)=MU*U%R(I3D)
              V%R(I3D)=MU*V%R(I3D)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

