!                   *********************************
                    SUBROUTINE SUSPENSION_EROSION_COH
!                   *********************************
!
     &(TAUP,NPOIN,XMVS,PARTHENIADES,
     & FLUER,TOCE_VASE,NOMBLAY,DT,MS_VASE)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE FLUX OF DEPOSITION AND EROSION
!+                ACCOUNTING FOR THE VERTICAL STRUCTURE.
!+
!+            !! NEW SUBROUTINE !!
!
!history  C. VILLARET
!+        31/07/2008
!+        V6P0
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
!history  C. VILLARET
!+        22/08/2012
!+        V6P2
!+        Added a number of improvements and modifications
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| FLUER          |<->| EROSION RATE
!| MS_VASE        |<->| MASS OF MUD PER LAYER (not modified here)
!| NOMBLAY        |-->| NUMBER OF LAYERS OF THE CONSOLIDATION MODEL
!| NPOIN          |-->| NUMBER OF POINTS
!| PARTHENIADES   |-->| PARTHENIADES CONSTANT (M/S)
!| TAUP           |-->| SKIN FRICTION
!| TOCE_VASE      |-->| CRITICAL BED SHEAR STRESS OF THE MUDPER LAYER
!| XMVS           |-->| DENSITY OF SOLID
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_SUSPENSION_EROSION_COH=>
     &                          SUSPENSION_EROSION_COH
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : NLAYMAX
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NOMBLAY
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XMVS
      DOUBLE PRECISION, INTENT(IN)    :: PARTHENIADES
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_VASE(NOMBLAY), DT
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TAUP
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I, J
      DOUBLE PRECISION :: AUX
      DOUBLE PRECISION :: FLUER_LOC(NLAYMAX),MER_VASE,TEMPS
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
! supprimer VITCE = SQRT (TOCE_VASE(1)), ES
! supprimer XMVE, TASS, DEBUG, GRAV
! rjouter NLAYAX
      ! *************************************************  !
      ! IA - FORMULATION FOR COHESIVE SEDIMENTS            !
      !      (WITHOUT CONSOLIDATION: UNIFORM SEDIMENT BED) !                                   !
      ! ******************************************* *****  !

      IF(NOMBLAY.EQ.1) THEN
        DO I = 1, NPOIN

          IF(TAUP%R(I).GT.TOCE_VASE(1)) THEN
!           AUX = MAX(((USTARP/VITCE)**2 - 1.D0),0.D0)
            AUX=((TAUP%R(I)/MAX(TOCE_VASE(1),1.D-08))-1.D0)
          ELSE
            AUX = 0.D0
          ENDIF
          FLUER%R(I) = PARTHENIADES*AUX
        ENDDO
      ELSE
      ! **************************************************** !
      ! IB - FORMULATION FOR COHESIVE SEDIMENTS  + CONSOLIDATION !
      !      (WITH BEDLOAD)                                  !
      ! **************************************************** !
!      BEWARE: HERE PARTHENIADES IS IN M/S
        DO I=1,NPOIN
! Calcul des Flux de masse/couche  en Kg/m2/s
          DO J=1,NOMBLAY
            IF(TAUP%R(I).GT.TOCE_VASE(J))THEN
              FLUER_LOC(J)=PARTHENIADES*XMVS
     &              *((TAUP%R(I)/MAX(TOCE_VASE(J),1.D-08))-1.D0)
            ELSE
              FLUER_LOC(J)=0.D0
            ENDIF
          ENDDO
!
! MER_VASE: total mass to be potentially eroded
!
          MER_VASE = 0.D0
          TEMPS= DT
!
          DO J= 1, NOMBLAY
            IF(MS_VASE(I,J).GE.1.D-8) THEN
!             COMPUTES THE MASS POTENTIALLY ERODED IN LAYER J (KG/M2)
!              QE_COUCHE = FLUER_LOC(J) *XMVS * TEMPS
              IF(FLUER_LOC(J)*TEMPS.LT.MS_VASE(I,J)) THEN
                MER_VASE = MER_VASE  + FLUER_LOC(J)*TEMPS
                GO TO 10
              ELSE
                MER_VASE = MER_VASE + MS_VASE(I,J)
                TEMPS= TEMPS-MS_VASE(I,J)/FLUER_LOC(J)
                TEMPS=MAX(TEMPS,0.D0)
              ENDIF
            ENDIF
          ENDDO
!
10        CONTINUE
!

          FLUER%R(I) = MER_VASE/DT/XMVS
!
        ENDDO
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
