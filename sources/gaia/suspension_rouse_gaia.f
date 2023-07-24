!                   ********************************
                    SUBROUTINE SUSPENSION_ROUSE_GAIA
!                   ********************************
!
     &(USTAR,HN,NPOIN,KARMAN,ZERO,XWC,ZREF,CSRATIO)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the deposition flux and concentration according to
!Rouse profile
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     USTAR      Shear velocity
!>@param[in]     HN         Water depth
!>@param[in]     NPOIN      Number of points
!>@param[in]     KARMAN     Von Karman constant
!>@param[in]     ZERO       Zero
!>@param[in]     XWC        Settling velocity
!>@param[in]     ZREF       Reference elevation
!>@param[in,out] CSRATIO         Working bief containing the Rouse profile
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: USTAR,HN,ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,XWC,ZERO
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSRATIO
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
!       ASSUMING EXPONENTIAL PROFILE WITH EXPONENT ROUSE NUMBER --> CSRATIO
!
        EXP1=ROUSE-1.D0
        IF(ABS(EXP1).GT.1.D-4) THEN
          EXP1=MIN(EXP1,3.D0)
          CSRATIO%R(I)=B*(1.D0-B**EXP1)/EXP1
        ELSE
          CSRATIO%R(I)=-B*LOG(B)
        ENDIF
        CSRATIO%R(I)=MAX(1.D0/MAX(CSRATIO%R(I),ZERO),1.D0)
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
