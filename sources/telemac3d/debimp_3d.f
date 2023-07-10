!                   ********************
                    SUBROUTINE DEBIMP_3D
!                   ********************
!
     &(Q,UBOR,VBOR,U,V,NUMLIQ,NUMLIQ_ELM,IFRLIQ,T3_02,
     & NPTFR,NETAGE,MASK,MESH,FORMUL,IELM2V,SVIDE,MASKBR,NELEB)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   19/03/2014
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
!history  J-M HERVOUET
!+        19/09/2011
!+        V6P2
!+   3D VERSION OF THE 2D DEBIMP, TO REPLACE DEBIMP3D
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Boundary segments have now their own numbering, independent of
!+   boundary points numbering.
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
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| NUMLIQ_ELM     |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY ELEMENTS
!| Q              |-->| VALUE OF PRESCRIBED FLOWRATE DU DEBIT IMPOSE
!| SVIDE          |<->| DUMMY
!| T3_02          |<->| WORK ARRAY
!| U              |-->| X-COMPONENT OF VELOCITY
!| UBOR           |<->| INLET PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| V              |-->| Y-COMPONENT OF VELOCITY
!| VBOR           |<->| INLET PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
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
      INTEGER, INTENT(IN) :: NPTFR,NETAGE,IFRLIQ,NELEB
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR),NUMLIQ_ELM(NELEB)
!
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION, INTENT(IN) :: Q
!
      CHARACTER(LEN=16) FORMUL
!
      INTEGER, INTENT(IN)            :: IELM2V
      TYPE(BIEF_MESH)                :: MESH
      TYPE(BIEF_OBJ) , INTENT(IN)    :: MASK
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: MASKBR
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: U,V
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: T3_02
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: SVIDE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IETAGE,IPTFR,I3D,IELEB
      DOUBLE PRECISION Q1
!
      INTRINSIC ABS
!
!=======================================================================
!     COMPUTES THE FLUX OBTAINED IF UBOR AND VBOR ARE UNCHANGED
!=======================================================================
!
!     U AND V ARE SET HERE TO THE PRESCRIBED VALUES (THAT MAY EVOLVE IN
!     TIME AND NOT BE EQUAL TO DIRICHLET VALUE OF PREVIOUS TIME STEP)
!
      DO IPTFR=1,NPTFR
        IF(NUMLIQ(IPTFR).EQ.IFRLIQ) THEN
          DO IETAGE=1,NETAGE+1
            I3D=(IETAGE-1)*NPTFR+IPTFR
            U%R(MESH%NBOR%I(I3D))=UBOR(I3D)
            V%R(MESH%NBOR%I(I3D))=VBOR(I3D)
!           W%R = WILL NOT CHANGE THE FLUX IF BOUNDARY VERTICAL
          ENDDO
        ENDIF
      ENDDO
!
!     IN THE FOLLOWING LOOP ONE RESTRICTS THE MASK OF DIRICHLET ELEMENTS
!     TO THOSE OF THE LIQUID BOUNDARY NUMBER IFRLIQ.
!
      CALL OS('X=0     ',X=MASKBR)
!
!     LOOP ON BOUNDARY ELEMENTS
!
      DO IELEB=1,NELEB
        IF(NUMLIQ_ELM(IELEB).EQ.IFRLIQ) MASKBR%R(IELEB)=MASK%R(IELEB)
      ENDDO
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
!     ZERO FLOW: WARNING MESSAGE
!
      IF(ABS(Q1).LT.1.D-10) THEN
        IF(ABS(Q).GT.1.D-10) THEN
          WRITE(LU,31) IFRLIQ
31        FORMAT(1X,'DEBIMP_3D: PROBLEM ON BOUNDARY NUMBER ',1I6,/,1X,
     &     '         GIVE A VELOCITY PROFILE  ',/,1X,
     &     '         IN THE BOUNDARY CONDITIONS FILE',/,1X,
     &     '         OR CHECK THE WATER DEPTHS',/,1X,
     &     '         OTHER POSSIBLE CAUSE:',/,1X,
     &     '         SUPERCRITICAL ENTRANCE WITH FREE DEPTH')
          CALL PLANTE(1)
          STOP
        ELSE
          Q1 = 1.D0
        ENDIF
      ENDIF
!
!=======================================================================
!   NORMALISES UBOR VBOR
!=======================================================================
!
      DO IPTFR=1,NPTFR
        IF(NUMLIQ(IPTFR).EQ.IFRLIQ) THEN
          DO IETAGE=1,NETAGE+1
            I3D=(IETAGE-1)*NPTFR+IPTFR
            UBOR(I3D) = UBOR(I3D) * Q / Q1
            VBOR(I3D) = VBOR(I3D) * Q / Q1
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
