!                   *****************
                    SUBROUTINE PHSTAT
!                   *****************
!
     &   (PH, DELTAR, Z, TRA01, TRA02, RHO0, GRAV,
     &    NPOIN3, NPOIN2, NPLAN )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE HYDROSTATIC PRESSURE FIELD PH [PA]
!+                THROUGH INTEGRATION BASED ON TRAPEZIUM RULE IN VERTICAL.
!+
!+            THIS IS NEEDED FOR APPLICATIONS WHERE THE GLOBAL
!+                PRESSURE OUTPUT IS REQUIRED.
!code
!+                                               S
!+ PH(Z) = G * RHO0 * (S-Z) + G * RHO0 * INTEGRAL  DELTAR DZ
!+                                               Z
!+ WHERE: DELTAR = (RHO-RHO0)/RHO0
!
!history  JACEK A. JANKOWSKI - UNIVERSITAET HANNOVER
!+        **/04/99
!+        V5P1
!+   FORTRAN95 VERSION
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DELTAR         |-->| RELATIVE DENSITY DELTAR = (RHO-RHO0)/RHO0
!| GRAV           |-->| GRAVITY ACCELERATION
!| NPLAN          |-->| NUMBER OF MESH PLANES
!| NPOIN2         |-->| NUMBER OF 2D-POINTS
!| NPOIN3         |-->| NUMBER OF 3D-POINTS
!| PH             |<->| HYDROSTATIC PRESSURE
!| RHO0           |-->| WATER DENSITY AT REFERENCE CONCENTRATION
!| TRA01          |<->| WORK FIELDS
!| TRA02          |<->| WORK FIELDS
!| Z              |-->| Z-COORDINATES OF NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN3, NPOIN2, NPLAN
      DOUBLE PRECISION, INTENT(INOUT) :: PH(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: DELTAR(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION,INTENT(INOUT)  :: TRA01(NPOIN3), TRA02(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: RHO0, GRAV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K1,K2
!
!----------------------------------------------------------------------
! COMPUTES USING THE DENSITY VARIABLE PART
!
!                            S
! PH(Z) = G * RHO0 * INTEGRAL  DELTAR DZ
!                            Z
!
      CALL OV('X=Y     ', X=TRA02, Y=DELTAR, DIM1=NPOIN3)
      K1 = (NPLAN-1) * NPOIN2 + 1
      CALL OV('X=C     ', X=TRA01(K1), C=0.0D0, DIM1=NPOIN2)
!
      DO I=NPLAN-1,1,-1
        K2 = K1-NPOIN2
        CALL OV('X=Y-Z   ', X=TRA01, Y=Z(K1), Z=Z(K2), DIM1=NPOIN2)
        CALL OV('X=X+Y   ', X=TRA02(K1), Y=TRA02(K2), DIM1=NPOIN2)
        CALL OV('X=XY    ', X=TRA01,      Y=TRA02(K1), DIM1=NPOIN2)
        CALL OV('X=Y+Z   ', X=TRA01(K2),  Y=TRA01, Z=TRA01(K1),
     &          DIM1=NPOIN2)
        K1 = K2
      END DO
      CALL OV('X=CY    ', X=PH, Y=TRA01, C=0.5D0*GRAV*RHO0, DIM1=NPOIN3)
!
! COMPUTES THE CONSTANT DENSITY PART
! PH(Z) = PH(Z) + G * RHO0 * (S-Z)
!
      K1 = (NPLAN-1) * NPOIN2 + 1
      CALL OV('X=Y-Z   ', X=TRA01, Y=Z(K1), Z=Z(1), DIM1=NPOIN2)
!
      DO I=1,NPLAN
        K2 = (I-1)*NPOIN2+1
        CALL OV('X=Y-Z   ', X=TRA01, Y=Z(K1), Z=Z(K2),  DIM1=NPOIN2)
        CALL OV('X=Y+CZ  ', X=PH(K2), Y=PH(K2), Z=TRA01, C=GRAV*RHO0,
     &          DIM1=NPOIN2)
      END DO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE PHSTAT
