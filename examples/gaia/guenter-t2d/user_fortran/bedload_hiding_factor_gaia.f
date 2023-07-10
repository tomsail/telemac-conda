!               *************************************
                SUBROUTINE BEDLOAD_HIDING_FACTOR_GAIA
!               *************************************
!
     &(ACLADM, HIDFAC, NPOIN, HIDI, DCLA, K_H_Y, HIDING)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Hiding factor for each node, sediment class and time step.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     ACLADM Mean diameter of active layer
!>@param[in]     DCLA    Sediment grain diameter
!>@param[in]     HIDFAC Hiding factor formulas
!>@param[in]     HIDI   Hiding factor for particular size class (hidfac =0)
!>@param[in,out] HIDING Hiding factor correction
!>@param[in]     K_H_Y  Karim, holly & yang constant
!>@param[in]     NPOIN  Number of points
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,
     &    EX_BEDLOAD_HIDING_FACTOR => BEDLOAD_HIDING_FACTOR_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM
      INTEGER,          INTENT(IN)    :: HIDFAC, NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: HIDI, DCLA, K_H_Y
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
!
      INTEGER          :: J
      DOUBLE PRECISION :: C1, C2
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     ***************************
!     IA - CONSTANT HIDING FACTOR
!     ***************************
!
      IF (HIDFAC == 0) THEN
!
        CALL OS('X=C     ', X=HIDING, C=HIDI)
!
!     ***************************
!     IB - EGIAZAROFF FORMULATION
!     ***************************
!
      ELSEIF (HIDFAC == 1) THEN
!
        C1 = LOG10(19.D0)
        C2 = 19.D0*DCLA
        DO J = 1, NPOIN
          HIDING%R(J) = (C1/LOG10(C2/ACLADM%R(J)))**2
        ENDDO
!
!     ***********************************
!     IC - ASHIDA AND MICHIUE FORMULATION
!     ***********************************
!
      ELSEIF (HIDFAC == 2) THEN
!
        C1 = LOG10(19.D0)
        C2 = 19.D0*DCLA
        DO J = 1, NPOIN
!
          IF(DCLA/ACLADM%R(J) >= 0.4D0) THEN
            HIDING%R(J) = (C1 / LOG10(C2/ACLADM%R(J)) )**2
          ELSE
            HIDING%R(J) = 0.85D0*(ACLADM%R(J)/DCLA)
          ENDIF
!
        ENDDO
!
!     **************************************
!     IE - KARIM, HOLLY AND YANG FORMULATION
!     **************************************
!
      ELSEIF (HIDFAC == 4) THEN
!
        CALL OS('X=1/Y   ', X=HIDING, Y=ACLADM)
        CALL OS('X=CX    ', X=HIDING, C=DCLA)
        CALL OS('X=Y**C  ', X=HIDING, Y=HIDING, C=K_H_Y)
!
      ELSEIF(HIDFAC == 5)THEN
        DO J=1,NPOIN
          HIDING%R(J)=(DCLA/ACLADM%R(J))**(-0.8D0)
        ENDDO

      ELSE
!
        WRITE(LU,*) 'UNKNOWN HIDING FACTOR FORMULA: ',HIDFAC
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END SUBROUTINE BEDLOAD_HIDING_FACTOR_GAIA
