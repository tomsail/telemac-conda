!                     *********************
                      SUBROUTINE CALCS3D_O2
!                     *********************
!
     & (NPOIN3,NPOIN2,NPLAN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,TIMP,T31,T32,T21,HPROP,ZPROP,UN,VN)
!
!***********************************************************************
! WAQTEL   V8P4
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR O2 WAQ PROCESS FOR 3D CASE
!
!history  R. ATA
!+        21/02/2016
!+        V7P2
!+       CREATION
!
!history  S.E. BOURBAN (HRW)
!+        07/06/2017
!+        V7P3
!+        Indexing tracer (IND_*) to avoid conflicting naming convention
!+        between user defined tracers, water quality processes and
!+        ice processes. Introduction of the array RANK_*.
!
!history  S.E. BOURBAN (HRW)
!+        25/09/2017
!+        V7P3
!+        TEXP and TIMP are now additive to account for a variety of
!+        of sources / sinks on a given TRACER
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEMBEN        |-->| BENTIC DEMAND
!| DT            |-->| TIME STEP
!| FORMK2        |-->| FORMULA FOR COMPUTINK K2
!| HPROP         |-->| PROPAGATION DEPTH
!| K1            |-->| CONST. OF DEGRADATION KINETIC OF ORGANIC LOAD
!| K22           |<--| CONST. OF REAERATION
!| K44           |-->| CONST. OF NITRIFICATION KINETIC
!| MASSOU        |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| NPOIN         |-->| NUMBER OF NODES IN THE MESH
!| O2SATU        |<--| O2 SATURATION DENSITY OF WATER (CS)
!| PHOTO         |-->| PHOTOSYNTHESIS
!| WATTEMP       |-->| TEMPERATURE OF WATER (DEG CELSIUS)
!| T1,T2         |-->| WORKING ARRAYS
!| TEXP          |<--| EXPLICIT SOURCE TERM.
!| TIMP          |<--| IMPLICIT SOURCE TERM.
!| TN            |-->| TRACERS AT TIME N
!| UN,VN         |-->| VELOCITY COMPONENTS AT TIME N
!| ZPROP         |-->| PROPAGATION ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL,ONLY: FORMCS,K2,SECTODAY,
     &  IND_T,IND_O2,IND_OL,IND_NH4
      USE INTERFACE_PARALLEL
      USE INTERFACE_WAQTEL, EX_CALCS3D_O2 => CALCS3D_O2
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER          , INTENT(IN   ) :: NPOIN2,NPOIN3,NPLAN
      INTEGER          , INTENT(IN   ) :: FORMK2
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,UN,VN,ZPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T31,T32,T21
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
      INTEGER                     :: I,J
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION            :: BENCORR,PMOINR,CONV_K1,CONV_K44
      DOUBLE PRECISION            :: POWER
      INTRINSIC MAX
!
      PMOINR  = (PHOTO-RESP)*SECTODAY
!     CONVERT DAYS TO SECONDS
      CONV_K44= K44   *SECTODAY
      CONV_K1 = K1    *SECTODAY
      BENCORR = DEMBEN*SECTODAY
      POWER   = WATTEMP-20.D0
      DO I=1,NPOIN3
        IF( IND_T.GT.0 ) POWER=TN%ADR(IND_T)%P%R(I)-20.D0
!       CORR2T AND BENCOR STOCKED HERE IN T31,T32
        T31%R(I)=CORR2**POWER
        T32%R(I)=BENCORR*(CORR1**POWER)
      ENDDO
!
!     COMPUTE CS (O2SATU, STOCKED IN T21)
!
      IF( IND_T.EQ.0 )THEN
        CALL SATUR_O2(O2SATU,FORMCS,WATTEMP,EPS)
        CALL OS('X=C     ',X=T21,C=O2SATU       )
      ELSE
        DO I=1,NPOIN2
          J=(NPLAN-1)*NPOIN2+I
          CALL SATUR_O2(T21%R(I),FORMCS,TN%ADR(IND_T)%P%R(J),EPS)
        ENDDO
      ENDIF
!
!     COMPUTE K2
!
      CALL REAER(FORMK2,K2,K22,NPOIN2,NPLAN,UN,VN,HPROP,EPS)
!     CONVERT DAY TO SEC
      CALL OS('X=CX    ',X=K2,C=SECTODAY)
!
!     COMPUTE RS (DONE IN DIFSOU)
!
!----------------------------------------------------------------------
!     LET'S NOW COMPUTE SOURCE TERMS
!
!     FIRST TRACER O2 (RANK IND_O2)
!     warning: here there are lots of choices that can be changed
!              by the user:
!              1- reareration is considered for only water surface,
!              2- bentic demand concerns only the bottom or all the water
!              column (here the second choice is considered)
!              these choices and many others, have to be decided by WAQ
!              experts
!
!     EXPLICIT PART
!     =============
!
!     SURFACE SOURCES
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
!       SURFACE LAYER THICKNESS/2: TEMPORARILY STORED IN T21==>2D TABLE
        T21%R(I)=0.5D0*(ZPROP%R(J)-ZPROP%R(J-NPOIN2))
        TEXP%ADR(IND_O2)%P%R(J) = TEXP%ADR(IND_O2)%P%R(J) +
     &    T31%R(J)*K2%R(I)*MAX((T21%R(I)-TN%ADR(IND_O2)%P%R(J)),0.D0) -
!                 O2 CONCENTRATION CAN NOT BE GREATER THAN SATURATED
     &    T32%R(I) * T21%R(I)
!
!      ADD PHOTOSYNTHESIS - RESPIRATION
!      CALL OS('X=X+C   ',X=TEXP%ADR(IND_O2)%P,C=PMOINR)
!
        TEXP%ADR(IND_O2)%P%R(J) = TEXP%ADR(IND_O2)%P%R(J)
     &                          + PMOINR
      ENDDO
!
!
!
!
!     THE REMAINING TERMS
!
      CALL OS('X=X+CY  ',X=TEXP%ADR(IND_O2)%P,Y=TN%ADR(IND_OL)%P,
     &         C=-CONV_K1 )
      CALL OS('X=X+CY  ',X=TEXP%ADR(IND_O2)%P,Y=TN%ADR(IND_NH4)%P,
     &         C=-CONV_K44)
!     IMPLICIT PART:
!     ==============
!     SOFAR, ALL TERMS ARE EXPLICIT FOR O2. CHANGE
!     IF THERE ARE DISAPPOINTING RESULTS
!
!     SECOND TRACER [L] ORGANIC LOAD
!
!      CALL OS('X=C     ',X=TIMP%ADR(IND_OL)%P,C=CONV_K1)
      CALL OS('X=X+C   ',X=TIMP%ADR(IND_OL)%P,C=CONV_K1)
!
!     THIRD TRACER [NH4]
!
!      CALL OS('X=C     ',X=TIMP%ADR(IND_NH4)%P,C=CONV_K44)
      CALL OS('X=X+C   ',X=TIMP%ADR(IND_NH4)%P,C=CONV_K44)
!
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS (TO CHECK)
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
