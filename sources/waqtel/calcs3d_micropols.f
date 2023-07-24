!                   ****************************
                    SUBROUTINE CALCS3D_MICROPOLS
!                   ****************************
     & (NPOIN2,BTABOF,TN,CF,UN,VN,T2_1)
!
!***********************************************************************
! WAQTEL   V8P4
!***********************************************************************
!
!brief    COMPUTES SURFACIC SOURCE TERMS FOR MICROPOL WAQ PROCESS IN 3D
!          FOR SEDIMENT EROSION IN THE WATER COLUMN
!
!history  Z. AMAMA (LNHE)
!+        //2022
!+        V8P4
!+        Creation from old calcs3d_micropol to split surfacic source
!+        terms from global and volumetric source terms
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BTABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM)
!| CF             |-->| FRICTION COEFFICIENT
!| NPOIN2         |-->| NUMBER OF NODES IN THE 2D MESH
!| TN             |-->| TRACERS
!| T2_1           |-->| WORKING ARRAYS
!| UN,VN          |-->| VELOCITY COMPONENTS AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL, ONLY : ERO,TAUR,RO0,IND_SF,IND_SS,RS
      USE INTERFACE_WAQTEL,EX_CALCS3D_MICROPOLS => CALCS3D_MICROPOLS
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN   ) :: NPOIN2
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: BTABOF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T2_1
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
      INTEGER I
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
!
!-----------------------------------------------------------------------
!
!     =======================================
!     PRELIMINARY COMPUTATIONS
!     =======================================
!
!     EROSION FLUX (RS): STORED IN RS ==> 2D TABLE
      CALL TAUB_WAQTEL(CF,RO0,T2_1,NPOIN2,UN,VN)
!
      CALL EROSION_FX(RS,T2_1,TN%ADR(IND_SF)%P,TAUR,ERO,1.D-10,
     &                NPOIN2)
!
!     =======================================
!     LET'S NOW COMPUTE SOURCE TERMS
!     =======================================
!
!     FIRST TRACER: SUSPENDED LOAD [SS] (IND_SS)
!
!     BED SOURCES
!      DO I=1,NPOIN2
!        TEXP%ADR(IND_SS)%P%R(I)=T2_3%R(I)-SED%R(I)
!      ENDDO
!      CALL OVD('X=Y/Z   ',TEXP%ADR(IND_SS)%P%R,TEXP%ADR(IND_SS)%P%R,
!     &         ZPROP%R,0.D0,NPOIN2,2,0.D0,EPS                      )
      DO I=1,NPOIN2
        BTABOF%ADR(IND_SS)%P%R(I)=RS%R(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
