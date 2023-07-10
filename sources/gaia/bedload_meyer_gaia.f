!                   *****************************
                    SUBROUTINE BEDLOAD_MEYER_GAIA
!                   *****************************
!
     &(TETAP,HIDING,HIDFAC,DENS,GRAV,DCLA,AC,ACP,QSC,SLOPEFF,COEFCR,
     & XMVS)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Meyer-Peter bedload transport formulation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     AC      Critical shields parameter
!>@param[in]     COEFCR  Correction of critical Shields for sloping bed effect
!>@param[in]     DENS    Relative density of sediment
!>@param[in]     DCLA     Sediment grain diameter
!>@param[in]     GRAV    Acceleration of gravity
!>@param[in]     HIDFAC  Hiding factor formulas
!>@param[in]     HIDING  Hiding factor correction
!>@param[in,out] QSC     Bed load transport rate
!>@param[in]     SLOPEFF Formula for slope effect
!>@param[in]     TETAP   Adimensional skin friction
!>@param[in]     XMVS    Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_GAIA, EX_BEDLOAD_MEYER => BEDLOAD_MEYER_GAIA
      USE DECLARATIONS_GAIA, ONLY : MPM_ARAY
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, HIDING,COEFCR
      INTEGER,          INTENT(IN)    :: HIDFAC, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DCLA, AC, XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION :: C2
!
!======================================================================!
!                               PROGRAM                                !
!=======================================================================
!
      CALL CPSTVC(QSC,ACP)
      CALL OS('X=C     ', X=ACP, C=AC)
!
!     SLOPE EFFECT: SOULBY FORMULATION
!
      IF(SLOPEFF.EQ.2) THEN
        CALL OS('X=XY    ', X=ACP, Y=COEFCR )
      ENDIF
!
!     BEDLOAD TRANSPORT CORRECTED FOR EXTENDED GRAIN SIZE
!     WITH VARIABLE MPM_COEFFICIENT
!
      C2 = SQRT(GRAV*DENS*DCLA**3)
!
      IF(HIDFAC.EQ.1.OR.HIDFAC.EQ.2) THEN
!       CALL OS('X=XY    ', X=ACP, Y=HIDING)
!       CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP)
!       CALL OS('X=+(Y,C)', X=QSC, Y=QSC , C=0.D0)
!       CALL OS('X=Y**C  ', X=QSC, Y=QSC , C=1.5D0)
!       CALL OS('X=CX    ', X=QSC, C=C2)
!       CALL OS('X=XY    ', X=QSC, Y=MPM_ARAY)
        DO I=1,QSC%DIM1
          QSC%R(I)=C2*MPM_ARAY%R(I)
     &               *SQRT(MAX(TETAP%R(I)-ACP%R(I)*HIDING%R(I),0.D0))**3
        ENDDO
      ELSE
!       CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP)
!       CALL OS('X=+(Y,C)', X=QSC, Y=QSC, C=0.D0)
!       CALL OS('X=Y**C  ', X=QSC, Y=QSC, C=1.5D0)
!       CALL OS('X=CX    ', X=QSC, C=C2)
!       CALL OS('X=XY    ', X=QSC, Y=HIDING)
!       CALL OS('X=XY    ', X=QSC, Y=MPM_ARAY)
        DO I=1,QSC%DIM1
          QSC%R(I)=C2*MPM_ARAY%R(I)*HIDING%R(I)*SQRT(
     &                                 MAX(TETAP%R(I)-ACP%R(I),0.D0))**3
        ENDDO
      ENDIF
!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
!=======================================================================
!
      RETURN
      END
