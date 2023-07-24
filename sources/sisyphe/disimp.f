!                   *****************
                    SUBROUTINE DISIMP
!                   *****************
!
     &(Q,Q2BOR,NUMLIQ,IFRLIQ,NSOLDIS,WORK1,QBOR,NPTFR,MASK,MESH)
!
!***********************************************************************
! SISYPHE   V6P2                                   24/07/2012
!***********************************************************************
!
!brief    Imposes solid discharge boundary conditions. Q2BOR is the
!+        discharge in m2/s, the integral of Q2BOR on the boundary QBOR
!+        is multiplied by a constant to get the correct discharge Q
!
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        24/07/2012
!+        V6P2
!+
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFRLIQ         |-->| RANK OF LIQUID BOUNDARY
!| MASK           |-->| BLOCK OF MASKS FOR BOUNDARY CONDITIONS
!| MESH           |-->| MESH STRUCTURE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSOLDIS        |-->| NUMBER OF SOLID DISCHARGES GIVEN IN PARAMETER
!|                |   | FILE
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| Q              |-->| PRESCRIBED VALUE OF DISCHARGE
!| Q2BOR          |<--| PRESCRIBED SOLID DISCHARGE
!| RATIO          |<--| RATIO, QBOR WILL BE RATIO*Q2BOR
!| WORK1          |<->| WORK BIEF_OBJ STRUCTURE
!| QBOR           |<->| THE RESULTING DISCHARGE IN M3/S
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPTFR,IFRLIQ,NSOLDIS
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: MASK(NPTFR),Q
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: WORK1,QBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: Q2BOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IELM
!
      DOUBLE PRECISION Q1
!
      INTRINSIC ABS
!
!=======================================================================
!     COMPUTES FLUX
!=======================================================================
!
!     IN THE FOLLOWING LOOP ONE RESTRICTS THE MASK OF DIRICHLETS SEGMENTS
!     TO THOSE OF THE LIQUID BOUNDARY NUMBER IFRLIQ. AS NUMLIQ IS
!     DEFINED AT NODES, ONE RISKS AN ERROR FOR THE SEGMENT FOLLOWING
!     THE LAST NODE ON THE BOUNDARY. IN FACT THIS SEGMENT WILL BE SOLID
!     AND WILL HAVE A MASK ALREADY SET TO ZERO.
!
      DO K=1,NPTFR
        IF(NUMLIQ(K).EQ.IFRLIQ) THEN
          WORK1%R(K)=MASK(K)
        ELSE
          WORK1%R(K)=0.D0
        ENDIF
      ENDDO
!
!     Q2BOR IS INTEGRATED ALONG THE BOUNDARY
!
      IELM=11
      CALL VECTOR(QBOR,'=','MASVEC          ',IELBOR(IELM,1),
!                      USED  VOID  VOID  VOID  VOID  VOID
     &            1.D0,Q2BOR,Q2BOR,Q2BOR,Q2BOR,Q2BOR,Q2BOR,
     &            MESH,.TRUE.,WORK1)
!
!=======================================================================
!     FINAL QBOR IF Q2BOR ONLY A PROFILE
!=======================================================================
!
      IF(NSOLDIS.GE.IFRLIQ) THEN
!
!       A VALUE OF DISCHARGE HAS BEEN GIVEN IN THE PARAMETER FILE
!       FOR THIS BOUNDARY. Q2BOR IS CONSIDERED AS ONLY A PROFILE
!
!       FOR THE USER: POSITIVE DISCHARGE = ENTERING
        Q1 = BIEF_SUM(QBOR)
        IF(NCSIZE.GT.1) Q1 = P_DSUM(Q1)
!
        IF(ABS(Q1).LT.1.D-10) THEN
!         ZERO FLUX: WARNING MESSAGE
          IF(ABS(Q).GT.1.D-10) THEN
            WRITE(LU,31) IFRLIQ
31          FORMAT(1X,'DISIMP : PROBLEM ON BOUNDARY NUMBER ',1I6,/,1X,
     &     '           GIVE A SOLID DISCHARGE PROFILE  ',/,1X,
     &     '           IN THE BOUNDARY CONDITIONS FILE')
            CALL PLANTE(1)
            STOP
          ELSE
            Q1 = 1.D0
          ENDIF
        ENDIF
!
        DO K=1,NPTFR
          IF(NUMLIQ(K).EQ.IFRLIQ) THEN
            QBOR%R(K) = QBOR%R(K) * Q / Q1
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
