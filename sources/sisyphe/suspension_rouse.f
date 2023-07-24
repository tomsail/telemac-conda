!                   ***************************
                    SUBROUTINE SUSPENSION_ROUSE
!                   ***************************
!
     &(USTAR,HN,NPOIN,KARMAN,ZERO,XWC,ZREF,T2)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE DEPOSITION FLUX AND
!+                CONCENTRATION ACCORDING TO ROUSE PROFILE.
!
!history  C. VILLARET     ; J-M HERVOUET
!+        14/04/04
!+        V5P5
!+
!
!history  F. HUVELIN
!+        04/01/05
!+        V5P6
!+
!
!history  J-M HERVOUET
!+        13/07/07
!+        V5P7
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HN             |-->| WATER DEPTH
!| KARMAN         |-->| VON KARMAN CONSTANT
!| NPOIN          |-->| NUMBER OF POINTS
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| USTAR          |-->| SHEAR VELOCITY
!| XWC            |-->| SETTLING VELOCITIES
!| ZERO           |-->| ZERO
!| ZREF           |-->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_SUSPENSION_ROUSE => SUSPENSION_ROUSE
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: USTAR,HN,ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,XWC,ZERO
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I
      DOUBLE PRECISION :: B,EXP1,ROUSE
!
      INTRINSIC MAX,MIN,LOG
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     ROUSE NUMBER AND MINIMUM BOUND OF THE EINSTEIN INTEGRAL
!
      DO I=1,NPOIN
!
!       ROUSE NUMBER
!
        ROUSE=  XWC / (KARMAN*MAX(USTAR%R(I),ZERO))
!
!       MINIMUM BOUND OF THE EINSTEIN INTEGRAL -->  B = KS/H
!
!       B ALWAYS LESS THAN 1.
        B = ZREF%R(I)/MAX(HN%R(I),ZREF%R(I))
!
!       RATIO BETWEEN REFERENCE CONC. ON BOTTOM AND MEAN CONC.
!       ASSUMING EXPONENTIAL PROFILE WITH EXPONENT ROUSE NUMBER --> T2
!
        EXP1=ROUSE-1.D0
        IF(ABS(EXP1).GT.1.D-4) THEN
          EXP1=MIN(EXP1,3.D0)
          T2%R(I)=B*(1.D0-B**EXP1)/EXP1
        ELSE
          T2%R(I)=-B*LOG(B)
        ENDIF
        T2%R(I)=MAX(1.D0/MAX(T2%R(I),ZERO),1.D0)
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
