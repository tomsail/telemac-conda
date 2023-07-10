!                   **************************
                    SUBROUTINE FRICTION_KHIONE
!                   **************************
!
     &( NPOIN, KFROT, GRAV,KARMAN, CHESTR, CF, H,U,V )
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    Sets the friction coefficient under the ice cover.
!
!code
!+     FRICTION LAWS PROGRAMMED :
!+
!+     KFROT = 0 :  NO FRICTION
!+     KFROT = 1 :  LAW OF HAALAND
!+     KFROT = 2 :  LAW OF CHEZY
!+     KFROT = 3 :  LAW OF STRICKLER
!+     KFROT = 4 :  LAW OF MANNING
!+     KFROT = 5 :  LAW OF NIKURADSE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| CHESTR         |-->| FRICTION PARAMETER
!| GRAV           |-->| GRAVITY ACCELERATION
!| H              |-->| WATER DEPTH
!| KARMAN         |-->| VON KARMAN'S CONSTANT
!| KFROT          |-->| LAW USED FOR THE CALCULATION
!| NPOIN          |-->| NUMBER OF NODES
!| U              |-->| X COMPONENT OF THE VELOCITY
!| V              |-->| Y COMPONENT OF THE VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_KHIONE, EX_FRICTION_KHIONE => FRICTION_KHIONE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN, KFROT
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, KARMAN
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CHESTR,H,U,V
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                       :: I
      DOUBLE PRECISION, PARAMETER   :: TIERS = 1.D0/3.D0
      DOUBLE PRECISION, PARAMETER   :: SUR30 = 1.D0/30.D0
      DOUBLE PRECISION              :: UNORM, INLOG, AUX
      DOUBLE PRECISION, PARAMETER   :: EPS=1.D-3
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      SELECT CASE (KFROT)
!
! NO FRICTION
! -----------
!
      CASE(0)
!
        DO I = 1,NPOIN
          CF%R(I) = 0.D0
        ENDDO
!
! LAW OF HAALAND
! --------------
!
      CASE(1)
!
        DO I = 1,NPOIN
          UNORM = MAX(SQRT( U%R(I)**2 + V%R(I)**2 ),1.D-6)
!                       1.D-6: LAMINAR VISCOSITY OF WATER
          INLOG = (6.9D0*1.D-6/4.D0  /MAX(H%R(I),EPS)/UNORM)**3
     &          + (CHESTR%R(I)/14.8D0/MAX(H%R(I),EPS))**3.33D0
          INLOG = MIN(1.D0-1.D-6,INLOG)
          AUX   = -0.6D0*LOG(INLOG)/LOG(10.D0)
          CF%R(I) = 0.25D0 / AUX**2
        ENDDO
!
! LAW OF CHEZY
! ------------
!
      CASE(2)
!
        DO I = 1,NPOIN
          CF%R(I) = 2.D0*GRAV/(CHESTR%R(I)**2)
        ENDDO
!
! LAW OF STRICKLER
! ----------------
!
      CASE(3)
!
        DO I = 1,NPOIN
          CF%R(I) = 2.D0*GRAV/CHESTR%R(I)**2/MAX(H%R(I),EPS)**TIERS
        ENDDO
!
! LAW OF MANNING
! --------------
!
      CASE(4)
!
        DO I = 1,NPOIN
          CF%R(I) = 2.D0*GRAV*(CHESTR%R(I)**2)/MAX(H%R(I),EPS)**TIERS
        ENDDO
!
! LAW OF NIKURADSE
! ----------------
!
      CASE(5)
!
!       NOTE: 11.036 IS 30.D0/EXP(1.D0)
        DO I = 1,NPOIN
          AUX=MAX(1.001D0,MAX(H%R(I),EPS)*11.036D0/CHESTR%R(I))
          CF%R(I) = 2.D0 / (LOG(AUX)/KARMAN)**2
        ENDDO
!
! OTHER CASES
! -----------
!
      CASE DEFAULT
!
        WRITE(LU,2) KFROT
2       FORMAT(I5,' : UNKNOWN UNDER ICE COVER FRICTION LAW')
        CALL PLANTE(1)
        STOP
!
      END SELECT
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END SUBROUTINE FRICTION_KHIONE
