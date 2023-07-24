!                     *********************
                      SUBROUTINE CALCS2D_O2
!                     *********************
!
     & (NPOIN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,TIMP,T2,T3,T4,HPROP,UN,VN,DEBUG)
!
!***********************************************************************
! WAQTEL   V8P1
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR O2 WAQ PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
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
!| DEBUG         |-->| FOR DEBUGGING
!| DEMBEN        |-->| BENTIC DEMAND
!| FORMK2        |-->| FORMULA FOR COMPUTINK K2
!| K1            |-->| CONST. OF DEGRADATION KINETIC OF ORGANIC LOAD
!| HPROP         |-->| PROPAGATION DEPTH
!| K22           |<--| CONST. OF REAERATION
!| K44           |-->| CONST. OF NITRIFICATION KINETIC
!| NPOIN         |-->| NUMBER OF NODES IN THE MESH
!| O2SATU        |<--| O2 SATURATION DENSITY OF WATER (CS)
!| PHOTO         |-->| PHOTOSYNTHESIS
!| WATTEMP       |-->| TEMPERATURE OF WATER (DEG CELSIUS)
!| T2,T3,T4      |-->| WORKING ARRAY
!| TEXP          |<--| EXPLICIT SOURCE TERM.
!| TIMP          |<--| IMPLICIT SOURCE TERM.
!| TN            |-->| TRACERS AT TIME N
!| UN,VN         |-->| VELOCITY COMPONENTS AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL,ONLY: FORMCS,K2,SECTODAY,
     &  IND_T,IND_O2,IND_OL,IND_NH4
      USE INTERFACE_PARALLEL
      USE INTERFACE_WAQTEL, EX_CALCS2D_O2 => CALCS2D_O2
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER          , INTENT(IN   ) :: FORMK2,NPOIN,DEBUG
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T2,T3,T4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
      INTEGER                     :: I
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION            :: BENCORR,PMOINR,CONV_K1,CONV_K44
      DOUBLE PRECISION            :: POWER
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
!
!     PRELIMINARY COMPUTATIONS
!
      PMOINR  = (PHOTO-RESP)*SECTODAY
!     CONVERT DAYS TO SECONDS
      CONV_K44= K44   *SECTODAY
      CONV_K1 = K1    *SECTODAY
      BENCORR = DEMBEN*SECTODAY
      POWER   = WATTEMP-20.D0
      DO I=1,NPOIN
        IF( IND_T.GT.0 ) POWER=TN%ADR(IND_T)%P%R(I)-20.D0
!       CORR2T AND BENCOR STOCKED HERE IN T3,T4
        T3%R(I)=CORR2**POWER
        T4%R(I)=BENCORR*(CORR1**POWER)
      ENDDO
!
!     COMPUTE CS (O2SATU, STOCKED IN T2)
!
      IF( IND_T.EQ.0 )THEN
        CALL SATUR_O2(O2SATU,FORMCS,WATTEMP,EPS)
        CALL OS('X=C     ',X=T2,C=O2SATU       )
      ELSE
        DO I=1,NPOIN
          CALL SATUR_O2(T2%R(I),FORMCS,TN%ADR(IND_T)%P%R(I),EPS)
        ENDDO
      ENDIF
!
!     COMPUTE K2
!
      CALL REAER(FORMK2,K2,K22,NPOIN,1,UN,VN,HPROP,EPS)
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
      DO I=1,NPOIN
        TEXP%ADR(IND_O2)%P%R(I) = TEXP%ADR(IND_O2)%P%R(I)
     &    + T3%R(I)*K2%R(I) * MAX((T2%R(I)-TN%ADR(IND_O2)%P%R(I)),0.D0)
!                    O2 CONCENTRATION CAN NOT BE GREATER THAN SATURATED
     &    - T4%R(I)/MAX(EPS,HPROP%R(I))
      ENDDO
!
!     ADD PHOTOSYNTHESIS - RESPIRATION
!
      CALL OS('X=X+C   ',X=TEXP%ADR(IND_O2)%P,C=PMOINR)
!
!     THE REMAINING TERMS
!
      CALL OS('X=X+CY  ',X=TEXP%ADR(IND_O2)%P,Y=TN%ADR(IND_OL)%P,
     &         C=-CONV_K1 )
      CALL OS('X=X+CY  ',X=TEXP%ADR(IND_O2)%P,Y=TN%ADR(IND_NH4)%P,
     &         C=-CONV_K44)
      IF(DEBUG.GT.0)WRITE(LU,*)'IN O2, STEP1:',TEXP%ADR(IND_O2)%P%R(1)

!     IMPLICIT PART:
!     ==============
!     SOFAR, ALL TERMS ARE EXPLICIT FOR O2. CHANGE
!     IF THERE ARE DISAPPOINTING RESULTS
!
!     SECOND TRACER [L] ORGANIC LOAD
!
! /!\ contrary to 3D formulation
      CALL OS('X=X+CY  ',X=TIMP%ADR(IND_OL)%P,Y=HPROP,C=-CONV_K1)
!      CALL OS('X=X+C   ',X=TIMP%ADR(IND_OL)%P,C=CONV_K1)
      IF(DEBUG.GT.0)WRITE(LU,*)'IN O2, STEP2:',TIMP%ADR(IND_OL)%P%R(1)
!
!     THIRD TRACER [NH4]
!
! /!\ contrary to 3D formulation
      CALL OS('X=X+CY  ',X=TIMP%ADR(IND_NH4)%P,Y=HPROP,C=-CONV_K44)
!      CALL OS('X=X+C   ',X=TIMP%ADR(IND_NH4)%P,C=CONV_K44)
      IF(DEBUG.GT.0)WRITE(LU,*)'IN O2, STEP3:',TIMP%ADR(IND_NH4)%P%R(1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
