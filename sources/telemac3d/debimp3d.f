!                   *******************
                    SUBROUTINE DEBIMP3D
!                   *******************
!
     &(Q,UBOR,VBOR,WBOR,U,V,H,NUMLIQ,IFRLIQ,
     & T3_01,T3_02,T3_03,
     & NPTFR,NETAGE,MASK,MESH,FORMUL,NPOIN2,
     & IELM2V,SIGMAG,SVIDE,MASKBR,ZPROP)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    IMPOSES FLUX BOUNDARY CONDITIONS,
!+                WITH AN ASSUMPTION OF AFFINITY WITH THE
!+                VELOCITY PROFILES AT THE ENTRANCE.
!code
!+      PRINCIPLE: 1) COMPUTES THE FLOW ON THE BOUNDARY COMPRISED
!+                    BETWEEN THE BOUNDARY NODES NDEB AND NFIN.
!+
!+                 2) CORRECTS THE IMPOSED VELOCITY BETWEEN NDEB AND
!+                    NFIN SO THAT THE DISCHARGE EQUALS THE DESIRED
!+                    DISCHARGE. THIS CORRECTION IS A SIMPLE RULE OF 3
!+                    AND THUS DOES NOT MODIFY THE VELOCITY PROFILE
!+                    PREVIOUSLY PROVIDED OR COMPUTED.
!+
!+      THE RESULT IS PUT IN THE ARRAYS UBOR AND VBOR TO BE
!+      IMPOSED TO U AND V DURING THE VARIOUS STAGES OF COMPUTATION.
!+
!+      IF THE DISCHARGE THROUGH A BOUNDARY IS 0, CANNOT USE THE
!+      RULE OF 3, UBOR AND VBOR ARE THEN REPLACED BY U AND V
!+      OBTAINED AT THE TIME STEP N.
!+
!+      THE CASE WHERE THE SEGMENT (NDEB,NFIN) PRESENTS A NUMBERING
!+      DISCONTINUITY (CASE WHERE THIS SEGMENT CONTAINS THE NODE
!+      NUMBER 1) IS TREATED.
!
!note     JMH : T3_01, SIGMAG NOT USED.
!
!history  AG
!+        **/07/02
!+        V5P5
!+   3D VERSION OF THE 2D DEBIMP
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
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FORMUL         |---| 'FLUBOR          ' ONLY IN PRACTICE
!| IELM2V         |-->| DISCRETISATION TYPE FOR 2D VERTICAL MESH
!| IFRLIQ         |-->| NUMBER OF LIQUID BOUNDARY
!| MASK           |-->| MASK
!| MASKBR         |<->| MASK OF DIRICHLETS SEGMENTS RESTRICTED
!|                |<->| TO A FEW LIQUID BOUNDARIES
!| MESH           |---| MESH
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| Q              |-->| VALUE OF PRESCRIBED FLOWRATE DU DEBIT IMPOSE
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!|                |   | NOT USED
!| SVIDE          |<->| DUMMY
!| T3_01          |<->| WORK ARRAY: NOT USED
!| T3_02          |<->| WORK ARRAY
!| T3_03          |<->| WORK ARRAY: NOT USED
!| UBOR           |<->| INLET PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<->| INLET PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| WBOR           |<->| INLET PRESCRIBED BOUNDARY CONDITION ON VELOCITY W
!| ZPROP          |<->| VERTICAL COORDINATES FOR PROPAGATION STEP: NOT USED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR,NETAGE,NPOIN2,IFRLIQ
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR)
!
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*),WBOR(*)
      DOUBLE PRECISION, INTENT(IN) :: MASK(*)
      DOUBLE PRECISION, INTENT(IN) :: Q
!
      CHARACTER(LEN=16) FORMUL
!
      INTEGER, INTENT(IN) :: IELM2V
      LOGICAL, INTENT(IN) :: SIGMAG
      TYPE(BIEF_MESH) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKBR
      TYPE(BIEF_OBJ) , INTENT(INOUT):: H,U,V
      TYPE(BIEF_OBJ) , INTENT(INOUT):: T3_01,T3_02,T3_03
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: ZPROP
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: SVIDE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IETAGE,IPTFR,I3D
      DOUBLE PRECISION Q1
!
      INTRINSIC ABS
!
!=======================================================================
!     COMPUTES THE FLUX
!=======================================================================
!
!  IN THE FOLLOWING LOOP RESTRICTS THE MASK OF DIRICHLETS SEGMENTS
!  TO THOSE OF THE LIQUID BOUNDARY NUMBER IFRLIQ. AS NUMLIQ IS
!  DEFINED AT NODES, POSSIBLY INTRODUCES AN ERROR FOR THE SEGMENT
!  FOLLOWING THE LAST BOUNDARY NODE. IN FACT THIS SEGMENT WILL BE
!  SOLID AND WILL ALREADY HAVE A MASK AT 0.
!
      CALL OS('X=0     ',X=MASKBR)
!
!  CHECKS IF THERE'S A DIRICHLET ON EITHER U OR V
!
      IF(MASKBR%ELM.EQ.70) THEN
!
!         QUADRILATERAL ON THE LATERAL BOUNDARIES
!
          DO K = 1,NPTFR
            IF(NUMLIQ(K).EQ.IFRLIQ) THEN
              DO IETAGE = 1,NETAGE
                MASKBR%R((IETAGE-1)*NPTFR+K)=MASK(K)
              ENDDO
            ENDIF
          ENDDO
!
      ELSEIF(MASKBR%ELM.EQ.60) THEN
!
!         TRIANGLES ON THE LATERAL BOUNDARIES
!
          DO K = 1,NPTFR
            IF(NUMLIQ(K).EQ.IFRLIQ) THEN
              DO IETAGE = 1,NETAGE
                MASKBR%R((IETAGE-1)*2*NPTFR+K      )=MASK(K)
                MASKBR%R((IETAGE-1)*2*NPTFR+K+NPTFR)=MASK(K)
              ENDDO
            ENDIF
          ENDDO
!
      ELSE
        WRITE(LU,*) 'UNKNOWN ELEMENT FOR MASKBR IN DEBIMP3D'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      FORMUL = 'FLUBOR          '
!     IF (SIGMAG) FORMUL(7:7) = '2'
      CALL VECTOR(T3_02,'=',FORMUL,IELM2V,1.D0,SVIDE,SVIDE,SVIDE,
     &            U,V,SVIDE,MESH,.TRUE.,MASKBR)
!
      Q1 = - BIEF_SUM(T3_02)
!
      IF(NCSIZE.GT.1) Q1=P_SUM(Q1)
!
!  ZERO FLOW: WARNING MESSAGE
!
      IF(ABS(Q1).LT.1.D-10) THEN
        IF(ABS(Q).GT.1.D-10) THEN
          WRITE(LU,31) IFRLIQ
31        FORMAT(1X,'DEBIMP3D : PROBLEM ON BOUNDARY NUMBER ',1I6,/,1X,
     &     '         GIVE A VELOCITY PROFILE  ',/,1X,
     &     '         IN THE BOUNDARY CONDITIONS FILE',/,1X,
     &     '         OR CHECK THE WATER DEPTHS')
          CALL PLANTE(1)
          STOP
        ELSE
          Q1 = 1.D0
        ENDIF
      ENDIF
!
!=======================================================================
!   NORMALISES UBOR VBOR WBOR
!=======================================================================
!
      DO IPTFR=1,NPTFR
        IF(NUMLIQ(IPTFR).EQ.IFRLIQ) THEN
          DO IETAGE =1, NETAGE+1
            I3D=(IETAGE-1)*NPTFR+IPTFR
            UBOR(I3D) = UBOR(I3D) * Q / Q1
            VBOR(I3D) = VBOR(I3D) * Q / Q1
!           SEE BORD3D
!           WBOR(I3D) = 0.D0
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
